;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
=common-headers=


#| TODO:
  * This thing has grown in size. Some simpler CELL super types should probably be added.

  * Consider constructing the hash-tables lazily to save some space.

  * Think about type declarations and the VALUE slot. It's probably possible to do something using
    MOP here (meta-class.lisp).

  * Consider inlining functions so stack traces look better.
|#


(define-variable *after-event-pulse-fns*
    :doc "Functions to be executed after an entire \"event pulse\" has been completed.
The functions are executed in the order in which they where added.")


(defun add-after-event-pulse-fn (fn)
  (push fn *after-event-pulse-fns*))



(eval-now (defclass single-value-model () ()))
(eval-now (defclass cell () () (:metaclass stm-class)))

(defclass cell (single-value-model)
  ((alivep :reader alivep-of
           :type (member t nil)
           :initform t)

   (accepts-conditions-p :accessor accepts-conditions-p-of
                         :type (member t nil)
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

   (source-cells :reader source-cells-of
                 :type hash-table
                 :initform (make-hash-table :test #'eq :weakness :value)
                 :documentation "
This refers to CELLs that are known to notify our/this CELL of changes.")

   (target-cells :reader target-cells-of
                 :type hash-table
                 :initform (make-hash-table :test #'eq :weakness :value)
                 :documentation "
This contains CELLs that will be notified when our value changes.

NOTE: Weak links; the target CELL will be removed if it would otherwise be
garbage. See AMX:WITH-LIFETIME or WITH-FORMULA.")

   (on-cell-added-as-source-fn :accessor on-cell-added-as-source-fn-of
                               :initarg :on-cell-added-as-source-fn
                               :initform nil)

   (on-cell-added-as-target-fn :accessor on-cell-added-as-target-fn-of
                               :initarg :on-cell-added-as-target-fn
                               :initform nil)

   ;; The indirection used here is needed for TG:FINALIZE, below.
   (on-cell-removed-as-source-fn :initform λRnil)


   ;; The indirection used here is needed for TG:FINALIZE, below.
   (on-cell-removed-as-target-fn :initform λRnil))


  (:metaclass stm-class))


(defmethod initialize-instance :after ((cell cell) &key)
  (let ((source-cells (slot-value cell 'source-cells))
        (target-cells (slot-value cell 'target-cells))
        (on-cell-removed-as-source-fn (slot-value cell 'on-cell-removed-as-source-fn))
        (on-cell-removed-as-target-fn (slot-value cell 'on-cell-removed-as-target-fn)))
    (tg:finalize cell
                 (lambda ()
                   (with-sync ()
                     (withp (ref-value-of on-cell-removed-as-source-fn)
                       (funcall it source-cells target-cells))
                     (withp (ref-value-of on-cell-removed-as-target-fn)
                       (funcall it source-cells target-cells))

                     (maphash-values (lambda (source-cell)
                                       (funcall (on-cell-removed-as-source-fn-of source-cell)
                                                (source-cells-of source-cell)
                                                (target-cells-of source-cell)))
                                     source-cells)))))

  (when (or (input-evalp-of cell) (init-evalp-of cell))
    (let ((*after-event-pulse-fns* nil)) ;; Throw away.
      (cell-execute-formula cell))))


(defmethod (setf on-cell-removed-as-source-fn-of) (new-value (cell cell))
  (setf (ref-value-of (slot-value cell 'on-cell-removed-as-source-fn))
        new-value))


(defmethod on-cell-removed-as-source-fn-of ((cell cell))
  (ref-value-of (slot-value cell 'on-cell-removed-as-source-fn)))


(defmethod (setf on-cell-removed-as-target-fn-of) (new-value (cell cell))
  (setf (ref-value-of (slot-value cell 'on-cell-removed-as-target-fn))
        new-value))


(defmethod on-cell-removed-as-target-fn-of ((cell cell))
  (ref-value-of (slot-value cell 'on-cell-removed-as-target-fn)))


(defmethod print-object ((cell cell) stream)
  (let ((value-eslotd (find 'value (class-slots (class-of cell))
                            :key #'slot-definition-name)))
    (print-unreadable-object (cell stream :type t :identity t)
      (format stream ":VALUE ~S"
              ;; TODO: See the TODO for (PRINT-OBJECT REF T) in sw-stm/src/ref.lisp.
              (if *current-transaction*
                  (multiple-value-bind (value found-p)
                      (gethash (cons cell value-eslotd) (tr-equal-object-data *current-transaction*))
                    (if found-p
                        (car value)
                        (standard-instance-access cell (slot-definition-location value-eslotd))))
                  (standard-instance-access cell (slot-definition-location value-eslotd)))))))


(defn cell-execute-formula (t ((cell cell)))
  (if (or (eq *target-cell* cell)
          (member cell *source-cells* :test #'eq))
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
                      #| TODO: This seems less than ideal; it is almost as if we have two "paths" to this situation
                      where the other "path" is the other binding we do vs. *SOURCE-CELLS* in (SETF CELL-DEREF).
                      Think about this... |#
                      (let ((*source-cells* (cons cell *source-cells*)))
                        (funcall (truly-the function (slot-value cell 'formula)))))
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
                    (return-from cell-execute-formula (value-of cell)))
                  (mark-as-dead ()
                    :report (lambda (stream)
                              (muffle-compiler-note
                                (format stream "SW-MVC: Mark ~S as \"dead\" and keep propagating."
                                        cell)))
                    (cell-mark-as-dead cell)
                    (return-from cell-execute-formula nil)))))
          (prog1 result
            (setf (cell-deref cell) result
                  (init-evalp-of cell) t))))))


(defun cell-force-update (cell)
  (declare (cell cell))
  (cell-execute-formula cell))


(defn cell-mark-as-dead (null ((cell cell)))
  (nilf (slot-value cell 'alivep))
  (withp (on-cell-removed-as-target-fn-of cell)
    (funcall it (source-cells-of cell) (target-cells-of cell)))
  (maphash-values (lambda (source-cell)
                    (remhash cell (target-cells-of source-cell))
                    (withp (on-cell-removed-as-source-fn-of source-cell)
                      (funcall it (source-cells-of source-cell) (target-cells-of source-cell))))
                  (source-cells-of cell))
  (values))


(defn cell-set-formula (null ((cell cell) (formula function)))
  (setf (init-evalp-of cell) nil
        (slot-value cell 'formula) formula)
  (values))


(defn cell-observedp ((member t nil) ((cell cell)))
  (plusp (hash-table-count (target-cells-of cell))))


(defn cell-add-target-cell (null ((cell cell) (target-cell cell)))
  "When CELL changes, TARGET-CELL wants to know about it. This also updates (SOURCE-CELLS-OF TARGET-CELL)."
  #+:sw-mvc-debug (assert (not (eq cell target-cell)))
  (assert (alivep-of cell))
  (assert (alivep-of target-cell))
  (unless (gethash target-cell (target-cells-of cell))
    (setf (gethash target-cell (target-cells-of cell))
          target-cell
          (gethash cell (source-cells-of target-cell))
          cell)
    (withp (on-cell-added-as-target-fn-of target-cell)
      (funcall it cell))
    (withp (on-cell-added-as-source-fn-of cell)
      (funcall it target-cell)))
  (values))


(defn cell-notify-targets (null ((cell cell)))
  "Re-evaluate target-cells which depend on the value of CELL."
  (maphash-values (lambda (target-cell)
                    #+:sw-mvc-debug (assert (not (eq target-cell cell)))
                    (when (and (alivep-of target-cell)
                               (input-evalp-of target-cell))
                      (cell-execute-formula target-cell)))
                  (target-cells-of cell))
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
        (let-spec (*after-event-pulse-fns* nil)
          (values new-value
                  (let ((assign-p t))
                    (restart-case
                        (handler-case (setf assign-p
                                            (not (funcall (truly-the function (equal-p-fn-of cell))
                                                          (value-of cell)
                                                          new-value)))
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
                        (cell-notify-targets cell)
                        (when (= (length *source-cells*) 2)
                          (dolist (after-event-pulse-fn (reverse *after-event-pulse-fns*))
                            (funcall after-event-pulse-fn)))))))))))


#|(eval-now (proclaim '(inline cell-deref)))|#
(defn cell-deref (t ((cell cell)))
  (when *target-cell*
    ;; When CELL changes, *TARGET-CELL* wants to know about it.
    (cell-add-target-cell cell *target-cell*))
  (if (init-evalp-of cell)
      (ecase (output-evalp-of cell)
        ((or :cached nil) (value-of cell))
        ((t) (cell-execute-formula cell)))
      (cell-execute-formula cell)))


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
