;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(define-symbol-macro creating-formula-p *creating-formula*)


#| NOTE: A formula, by itself, is not a value or ("in") a cell. Don't waste time
here trying to make it into this. |#


#| TODO: I think it'd make sense to convert this to a DEFSTRUCT later. This
should run as fast as possible and have as little overhead as possible. |#

(defclass formula ()
  ((closure :reader closure-of :initarg :closure
            :type function
            :initform (error ":CLOSURE needed."))

   (sources :reader sources-of
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
      `(let ((*creating-formula* nil))
         ,@body)))


(defun formula-add-source (formula source &optional (slot-name nil slot-name-supplied-p))
  (declare (formula formula))
  (with-slots (sources) formula
    (if slot-name-supplied-p
        (pushnew (list slot-name source) sources :test #'equal)
        (pushnew source sources :test #'eq))))


(defmethod formula-add-target (formula target target-slot)
  (declare (formula formula)
           (symbol target-slot))
  (assert (sources-of formula) nil
          "SW-MVC: The FORMULA ~A about to be assigned to ~A has no sources." formula target)
  (dolist (source (sources-of formula))
    (if (listp source)
        (destructuring-bind (slot-name source) source
          (add-slot-callback target slot-name source
                             (lambda (event)
                               (let ((new-value (funcall (the function (closure-of formula)) event)))
                                 (prog1 new-value
                                   ;; "Simulate" a SLOT-SET event done towards TARGET.
                                   (let ((*simulate-slot-set-event-p* t))
                                     (setf (slot-value target target-slot) new-value)))))))
        (add-object-callback target source
                             (closure-of formula)))))


;; TODO: Ability to explicitly state what objects we're interested in monitoring?
(defmacro mk-formula ((&rest args) &body body)
  "ARGS: [CELLS] &KEY (EVENT-SYM 'EVENT)
Returns an instance of FORMULA."
  ;; TODO: Move some of this stuff to a function/method.
  (with-gensyms (formula)
    (destructuring-bind (&key (event-sym 'event))
        (if (listp (first args)) (rest args) args)
      `(with-cells (,@(when (listp (first args)) (first args)))
         (let (,formula)
           (setf ,formula
                 (make-instance 'formula
                                :closure (lambda (,event-sym)
                                           (declare (ignorable ,event-sym))
                                           (setf (value-of ,formula)
                                                 (progn ,@body)))))
           (prog1 ,formula
             ;; Trigger an initial sync which will also create the connections
             ;; which will take care of automatic syncing in the future.
             (let ((*creating-formula* ,formula))
               (funcall (the function (closure-of ,formula)) nil))
             (assert (sources-of ,formula) nil
                     "SW-MVC: The MK-FORMULA form was not able to automatically determine what resources
it is to monitor for changes.")))))))


(defmacro formula-of (&body body)
  "This is used to extract a FORMULA instance from \"something\". This tends to
apply for FORMULA instances stored in slots."
  `(let ((*get-formula-p* t)
         (*creating-formula* nil))
     (let ((result (progn ,@body)))
       (unless (typep result 'formula)
         (warn "SW-MVC, FORMULA-OF: Returning something not a formula; ~A" result))
       result)))
