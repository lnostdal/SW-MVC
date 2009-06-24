;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


#| TODO: I think it'd make sense to convert this to a DEFSTRUCT later. This
should run as fast as possible and have as little overhead as possible. |#

(defclass formula ()
  ((closure :reader closure-of :initarg :closure
            :type function
            :initform (error ":CLOSURE needed."))

   (modes :accessor mode-of :initarg :modes
          :type list
          :initform (list :input-eval))

   (sources :reader sources-of
            :type list
            :initform nil)

   (targets :reader targets-of
            :type list
            :initform nil)

   (value :type ref
          :initform (mk-ref))))


(flet ((set-value (formula new-value)
         (setf (ref-value-of (slot-value formula 'value))
               new-value)))
  (declare (inline set-value))


  (defmethod (setf value-of) (new-value (formula formula))
    (set-value formula new-value))


  (defmethod (setf deref) (new-value (formula formula))
    (set-value formula new-value)))


(flet ((get-value (formula)
         (ref-value-of (slot-value formula 'value))))
  (declare (inline get-value))


  (defmethod value-of ((formula formula))
    (get-value formula))


  (defmethod deref ((formula formula))
    (get-value formula)))


(defmacro with-ignored-sources ((&rest sources) &body body)
  (if sources
      (error "SW-MVC: WITH-IGNORED-SOURCES with anything else than NIL for SOURCES argument not implemented yet.")
      `(let ((*formula* nil))
         ,@body)))


(defun formula-add-source (formula source &optional (source-slot nil source-slot-supplied-p))
  (declare (formula formula))
  (with-slots (sources) formula
    (if source-slot-supplied-p
        (let ((source-signature (cons source-slot source)))
          (unless (find source-signature sources :test #'equal)
            (push source-signature sources)
            (dolist (target (targets-of formula))
              (let ((target-slot (car target))
                    (target (cdr target)))
                (formula-add-slot-callback formula target target-slot source source-slot)))))
        (unless (find source sources :test #'eq)
          (push source sources)))))


(defun formula-add-slot-callback (formula target target-slot source source-slot)
  (declare (formula formula)
           (symbol target-slot source-slot))
  (add-slot-callback target source-slot source
                     (lambda (event)
                       (let ((new-value (funcall (the function (closure-of formula)) event)))
                         (prog1 new-value
                           (let ((*simulate-slot-set-event-p* t))
                             (setf (slot-value target target-slot) new-value)))))))


(defun formula-add-target (formula target target-slot)
  (declare (formula formula)
           (symbol target-slot))
  (with-slots (targets) formula
    (pushnew (cons target-slot target) targets :test #'equal))
  (dolist (source (sources-of formula))
    (if (consp source)
        (formula-add-slot-callback formula target target-slot (cdr source) (car source))
        (add-object-callback target source
                             (closure-of formula)))))


;; TODO: Ability to explicitly state what objects we're interested in monitoring?
(defmacro mk-formula ((&rest args) &body body)
  "ARGS: [CELLS] &KEY (EVENT-SYM 'EVENT)
Returns an instance of FORMULA."
  ;; TODO: Move some of this stuff to a function/method.
  (with-gensyms (formula)
    `(with-cells (,@(when (listp (first args)) (first args)))
       (let (,formula)
         (setf ,formula
               (make-instance 'formula
                              ,@(if (listp (first args))
                                    (rest args)
                                    args)
                              :closure (lambda (=event=)
                                         (declare (ignorable =event=))
                                         (let ((*formula* ,formula))
                                           (declare (ignorable *formula*))
                                           (setf (value-of ,formula)
                                                 (progn ,@body))))))
         (prog1 ,formula
           ;; Trigger an initial sync which will also create the connections
           ;; which will take care of automatic syncing in the future.
           (let ((*event-stack* (cons ,formula *event-stack*)))
             (funcall (the function (closure-of ,formula)) nil)))))))
