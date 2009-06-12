;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defmacro with-cells ((&rest cells) &body body)
  "Convenience macro. Instead of saying (VALUE-OF A) or (SETF (VALUE-OF A) 1)
one can instead say A and (SETF A 1)."
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

       (symbol-macrolet (,@(loop :for cell :in cells
                                 :for index fixnum :from 0
                              :collect `(,cell (value-of (svref ,mcells ,index)))))
         ,@body))))



(defclass cell (single-value-model)
  ((value :accessor value-of :initarg :value))

  (:metaclass mvc-stm-class))


(defmethod (setf slot-value-using-class) :around (new-value (class mvc-stm-class) (instance cell) slot-definition)
  (if-let (translator (and (eq 'value (slot-definition-name slot-definition))
                           (input-translator-of instance)))
    (call-next-method (funcall translator new-value) class instance slot-definition)
    (call-next-method)))


(declaim (inline mk-cell))
(defun mk-cell (&optional (value nil value-supplied-p))
  (if value-supplied-p
      (make-instance 'cell :value value)
      (make-instance 'cell)))


(defmacro mk-fcell ((&rest args) &body body)
  `(mk-cell (mk-formula (,@args) ,@body)))


(defmethod deref-expand ((arg symbol) (type (eql 'cell)))
  `(slot-value ,arg 'value))


(defmethod deref ((cell cell))
  (slot-value cell 'value))


(defmethod (setf deref) (new-value (cell cell))
  (setf (slot-value cell 'value) new-value))
