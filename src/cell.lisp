;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

;;(declaim #.(optimizations))
(declaim (optimize speed))


#| TODO:
  * Consider constructing the hash-tables lazily to save some space.
|#


(defclass cell ()
  ((formula :initarg :formula
            :type function
            :initform λλnil)

   (value :initform nil)

   (writer-fn :accessor writer-fn-of :initarg :writer-fn
              :type function
              :initform (lambda (cell new-value) (setf (slot-value (truly-the cell cell) 'value) new-value)))

   (reader-fn :accessor reader-fn-of :initarg :reader-fn
              :type function
              :initform (lambda (cell) (slot-value (truly-the cell cell) 'value)))

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
                 :initform (make-hash-table :test #'eq :weakness :value)))

  (:metaclass stm-class))


(defmethod initialize-instance :after ((cell cell) &key)
  (when (or (input-evalp-of cell) (init-evalp-of cell))
    (cell-execute-formula cell)))


(defmethod value-of ((cell cell))
  (funcall (truly-the function (reader-fn-of cell)) cell))


(defmethod (setf value-of) (new-value (cell cell))
  (funcall (truly-the function (writer-fn-of cell)) cell new-value))


(defmethod cell-execute-formula ((cell cell))
  (if (member cell *source-cells* :test #'eq)
      (value-of cell)
      ;; NOTE: We track dependencies even though INPUT-EVALP is NIL. This will enable the user to set INPUT-EVALP
      ;; to T later and have it update based on those dependencies from there on.
      (let ((*target-cell* cell #|(when (input-evalp-of cell) cell)|#)
            (*source-cells* (cons cell *source-cells*)))
        (catch :abort-cell-propagation
          (let ((result (funcall (truly-the function (slot-value cell 'formula)))))
            (prog1 result
              (setf ~cell result
                    (init-evalp-of cell) t)))))))


(defmethod cell-force-update ((cell cell))
  (cell-execute-formula cell))


(defmethod cell-set-formula ((cell cell) (formula function))
  (setf (init-evalp-of cell) nil
        (slot-value cell 'formula) formula)
  (when (init-evalp-of cell)
    (cell-execute-formula cell))
  (values))


(defmethod cell-add-target-cell ((cell cell) (target-cell cell))
  "When CELL changes, TARGET-CELL wants to know about it."
  (setf (gethash target-cell (target-cells-of cell)) target-cell)
  (values))


(defmethod cell-observedp ((cell cell))
  (plusp (hash-table-count (target-cells-of cell))))


(eval-now (proclaim '(ftype (function (cell) (values t &optional))
                       cell-deref))
          ;; TODO: Inlining causes weird problems wrt. the WITH-CELLS macro.
          #|(proclaim '(notinline cell-deref))|#)
(declaim (inline cell-deref))
(defun cell-deref (cell)
  (if *get-formula-p*
      (slot-value cell 'formula)
      (progn
        (when *target-cell*
          (cell-add-target-cell cell *target-cell*))
        (if (init-evalp-of cell)
            (case (output-evalp-of cell)
              ((or :cached nil) (value-of cell))
              ((t) (cell-execute-formula cell)))
            (cell-execute-formula cell)))))
(declaim (notinline cell-deref))


(eval-now (proclaim '(ftype (function (t cell) (values t (member t nil) &optional))
                      (setf cell-deref)))
          (proclaim '(inline (setf cell-deref))))
(defun (setf cell-deref) (new-value cell)
  (if *get-formula-p*
      (let ((*get-formula-p* nil))
        (cell-set-formula cell new-value))
      (values new-value
              (if (funcall (truly-the function (equal-p-fn-of cell))
                           (value-of cell)
                           new-value)
                  nil
                  (prog1 t
                    (setf (value-of cell) new-value)
                    (maphash (lambda (key target-cell)
                               (declare (ignore key))
                               (when (input-evalp-of (truly-the cell target-cell))
                                 (cell-execute-formula (truly-the cell target-cell))))
                             (target-cells-of cell)))))))


(defmethod deref ((cell cell))
  (declare (inline cell-deref))
  (cell-deref cell))


(defmethod (setf deref) (new-value (cell cell))
  (setf (cell-deref cell) new-value))


(defmethod deref-expand ((arg symbol) (type (eql 'cell)))
  `(cell-deref (truly-the cell ,arg)))


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
