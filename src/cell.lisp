;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


#| TODO:
  * This thing has grown in size. Some simpler CELL super types should probably be added.

  * Consider constructing the hash-tables lazily to save some space.

  * Think about type declarations and the VALUE slot. It's probably possible to do something using
    MOP here (meta-class.lisp).

  * Consider inlining functions so stack traces look better.
|#


(defclass cell (single-value-model) ;; TODO: EVENT-ROUTER? .. hm ..
  ((alivep :reader alivep-of
           :type (member t nil)
           :initform t)

   (accepts-conditions-p :accessor accepts-conditions-p-of
                         :type (member or nil)
                         :initform nil
                         :documentation "
If T, the ASSIGN-CONDITION restart will automatically be invoked (NIL is default).")

   (formula :initarg :formula
            :type function
            :initform λλnil)

   (value :accessor value-of
          :initform nil)

   (equal-p-fn :accessor equal-p-fn-of :initarg :equal-p-fn
               :type function
               :initform #'eq)

   (init-evalp :accessor init-evalp-of :initarg :init-evalp
               :type (member t nil)
               :initform nil)

   (input-evalp :accessor input-evalp-of :initarg :input-evalp
                :type (member t nil)
                :initform t
                :documentation "
Note that setting this slot might not have any immediate effect; use
CELL-FORCE-UPDATE, possibly wrapped in SW-STM:WITH-DISABLED-COMMIT-BODIES.")

   (output-evalp :accessor output-evalp-of :initarg :output-evalp
                 :type (member t nil :cached)
                 :initform nil
                 :documentation "
Note that setting this slot might not have any immediate effect; use
CELL-FORCE-UPDATE, possibly wrapped in SW-STM:WITH-DISABLED-COMMIT-BODIES.")

   ;; STM-CLASS doesn't cover the hash-table here, but I think that's ok.
   (target-cells :reader target-cells-of
                 :type hash-table
                 :initform (make-hash-table :test #'eq :weakness :value)
                 :documentation "
This contains CELLs that will be notified when our value changes.

NOTE: Weak links; the target CELL will be removed if it would otherwise be
garbage. See AMX:WITH-LIFETIME or WITH-FORMULA."))

  (:metaclass stm-class))


(defmethod initialize-instance :after ((cell cell) &key)
  (when (or (input-evalp-of cell) (init-evalp-of cell))
    (cell-execute-formula cell)))


#| TODO: This is currently a bad idea; it will currently cause debugging (stack-traces) to have side-effects wrt.
STM. |#
#|(defmethod print-object ((cell cell) stream)
  (print-unreadable-object (cell stream :type t :identity t)
    (if (slot-boundp cell 'value)
        (prin1 (value-of cell) stream)
        (prin1 :not-bound stream))))|#


(defn cell-execute-formula (t ((cell cell)))
  (if (or (member cell *source-cells* :test #'eq)
          (eq *target-cell* cell))
      (value-of cell)
      #| NOTE: We track dependencies even though INPUT-EVALP is NIL. This will enable the user to set INPUT-EVALP
      to T later and have it update based on those dependencies from there on. |#
      (let ((*target-cell* cell #|(when (input-evalp-of cell) cell)|#))
        (let* ((condition)
               (result
                (restart-case
                    (handler-bind ((error (lambda (c)
                                            (setf condition
                                                  (make-instance 'mvc-cell-error :cell cell :condition c))
                                            (when (accepts-conditions-p-of cell)
                                              (invoke-restart 'assign-condition)))))
                      (funcall (truly-the function (slot-value cell 'formula))))
                  (assign-condition ()
                    :report (lambda (stream)
                              (muffle-compiler-note
                                (format stream "SW-MVC: Assign ~S as a value for ~S." condition cell)))
                    condition)
                  (skip-cell ()
                    :report (lambda (stream)
                              (muffle-compiler-note
                                (format stream "SW-MVC: Skip ~S (and any \"child CELLs\") and keep propagating."
                                        cell)))
                    (return-from cell-execute-formula (value-of cell))))))
          (prog1 result
            (setf (cell-deref cell) result
                  (init-evalp-of cell) t))))))


(defun cell-force-update (cell)
  (declare (cell cell))
  (cell-execute-formula cell))


#| This will mark a CELL as dead and it'll lazily be removed from other CELL's TARGET-CELLS hash-table. |#
(defn cell-mark-as-dead (null ((cell cell)))
  (nilf (slot-value cell 'alivep))
  (values))


(defn cell-set-formula (null ((cell cell) (formula function)))
  (setf (init-evalp-of cell) nil
        (slot-value cell 'formula) formula)
  (values))


(defn cell-observedp ((member t nil) ((cell cell)))
  (let ((target-cells (target-cells-of cell)))
    (sb-ext:with-locked-hash-table (target-cells)
      (plusp (hash-table-count target-cells)))))


(defn cell-add-target-cell (null ((cell cell) (target-cell cell)))
  "When CELL changes, TARGET-CELL wants to know about it."
  (let ((target-cells (target-cells-of cell)))
    (sb-ext:with-locked-hash-table (target-cells)
      (setf (gethash target-cell (target-cells-of cell)) target-cell)))
  (values))


(defn cell-notify-targets (null ((cell cell)))
  "Re-evaluate target-cells which depend on the value of CELL."
  (let ((target-cells (target-cells-of cell)))
    (dolist (target-cell (sb-ext:with-locked-hash-table (target-cells)
                           (hash-table-values target-cells)))
      (if (alivep-of target-cell)
          (when (input-evalp-of target-cell)
            (cell-execute-formula target-cell))
          (remhash target-cell target-cells))))
  (values))


;; TODO: When AMX:DEFN has support for specifying return types like this, get rid of this decl.
(eval-now (proclaim '(ftype (function (t cell) (values t (member t nil) &optional))
                      (setf cell-deref)))
          (proclaim '(inline (setf cell-deref))))
;; TODO: Document why this needs to return multiple values. IIRC it has something to do with dom-cache.lisp in SW,
;; and it's probably a bad idea.
(defun (setf cell-deref) (new-value cell)
  (if (member cell *source-cells*)
      (values (value-of cell) t)
      (let ((*source-cells* (cons cell *source-cells*)))
        (values new-value
                (let ((assign-p t))
                  (restart-case
                      (handler-case (funcall (truly-the function (equal-p-fn-of cell))
                                             (value-of cell) new-value)
                        (error (c)
                          (signal 'mvc-cell-assign-signal
                                  :condition c
                                  :new-value new-value
                                  :format-control "SW-MVC: The EQUAL-P test for ~A failed."
                                  :format-arguments (list cell))
                          new-value))
                    (continue ()
                      :report "SW-MVC: (EQUAL-P test) Assign the new value.")
                    (skip-cell ()
                      :report  "SW-MVC: (EQUAL-P test) Do not assign the new value."
                      (nilf assign-p)))
                  (when assign-p
                    (prog1 t
                      (setf (value-of cell) new-value)
                      (cell-notify-targets cell))))))))


;; TODO: Inlining causes weird problems wrt. the WITH-CELLS macro.
#|(eval-now (proclaim '(inline cell-deref)))|#
(declaim (inline cell-deref))
(defn cell-deref (t ((cell cell)))
  (when *target-cell*
    (cell-add-target-cell cell *target-cell*))
  (if (init-evalp-of cell)
      (ecase (output-evalp-of cell)
        ((or :cached nil) (value-of cell))
        ((t) (cell-execute-formula cell)))
      (cell-execute-formula cell)))
(declaim (notinline cell-deref))


(add-deref-type 'cell
                :get-expansion (λ (arg-sym) `(cell-deref ,arg-sym))
                :set-expansion t)


(defmacro with-cells ((&rest cells) &body body)
  "Convenience macro. Instead of saying ~A or (SETF ~A 1) one can say A
and (SETF A 1)."
  (with-gensyms (mcells)
    `(let ((,mcells (vector ,@cells)))
       (declare (dynamic-extent ,mcells)
                (ignorable ,mcells))

       #+:sw-mvc-debug-p
       (loop
          :for cell :across ,mcells
          :for cell-sym :in ',cells
          :do (unless (typep cell 'cell)
                (error "
CELL referred to by ~A turned out to be a ~A (~A) instead of a CELL.
Nesting of WITH-CELL and/or WITH-FORMULA forms? Debug output follows:

~A"
                       cell-sym (type-of cell) cell
                       (with-output-to-string (ss)
                         ,@(loop :for cell-sym :in cells
                              :collect `(when (symbol-macro-bound-p ,cell-sym)
                                          (format ss "  (SYMBOL-MACRO-BOUND-P ~A) => T (culprit?)~%" ',cell-sym)))))))

       (symbol-macrolet (,@(muffle-compiler-note
                            (loop :for cell :in cells
                               :for index fixnum :from 0
                               :collect `(,cell (cell-deref (svref ,mcells ,index))))))
         ,@body))))
