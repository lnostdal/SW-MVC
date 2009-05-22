;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(define-symbol-macro creating-formula-p *creating-formula*)


(defclass formula (single-value-model)
  ((closure :reader closure-of :initarg :closure
            :type function
            :initform (error ":CLOSURE needed."))

   (sources :reader sources-of
            :type list
            :initform nil)

   (static-p :reader static-p-of :initarg :static-p
             :initform t)

   (concurrency :reader concurrency-of :initarg :concurrency
                :type null ;;(or null t semaphore) ;; TODO: Finish support for this.
                :initform nil)

   (value :initform (mk-ref))))


;; These hide the indirection (needed for transactions).
(defmethod value-of ((formula formula))
  (ref-value-of (slot-value formula 'value)))


(defmethod deref ((formula formula))
  (ref-value-of (slot-value formula 'value)))


(defmethod (setf value-of) (new-value (formula formula))
  (setf (ref-value-of (slot-value formula 'value))
        new-value))


(defun formula-add-source (formula source &optional (slot-name nil slot-name-supplied-p))
  (declare (formula formula))
  (with-slots (sources) formula
    (if slot-name-supplied-p
        (pushnew (list slot-name source) sources :test #'equal)
        (pushnew source sources))))


(defmethod formula-add-target (formula target target-slot)
  (declare (formula formula)
           (symbol target-slot))
  (assert (sources-of formula) nil
          "The FORMULA ~A about to be assigned to ~A has no sources." formula target)
  (dolist (source (sources-of formula))
    (if (listp source)
        (destructuring-bind (slot-name source) source
          (add-slot-callback target slot-name source
                             (lambda (event)
                               (let ((new-value (funcall (the function (closure-of formula)) event)))
                                 (propagate (make-instance 'slot-set
                                                           :instance target
                                                           :slot-name target-slot
                                                           :new-value new-value))
                                 new-value))))
        (add-object-callback target source
                             (closure-of formula)))))


;; TODO: Ability to explicitly state what objects we're interested in monitoring?
(defmacro mk-formula ((&rest args) &body body)
  "ARGS: [cells] &key (event-sym 'event)
Returns an instance of FORMULA."
  ;; TODO: Move some of this stuff to a function/method.
  ;; TODO: Finish the :CONCURRENCY support.
  (with-gensyms (formula)
    (destructuring-bind (&key (event-sym 'event) (static-p nil) (concurrency nil))
        (if (listp (first args)) (rest args) args)
      `(with-cells (,@(when (listp (first args)) (first args)))
         (let (,formula)
           (setf ,formula
                 (make-instance 'formula
                                :static-p ,static-p
                                :concurrency ,(when (integerp concurrency)
                                               `(mk-semaphore :count ,concurrency))
                                :closure
                                ,(if concurrency
                                     `(lambda (,event-sym)
                                        (declare (ignorable ,event-sym))
                                        (semaphore-wait (concurrency-of ,formula))
                                        (let ((thread (with-thread (:fthread *creating-formula*)
                                                        (unwind-protect 
                                                             (setf (value-of ,formula)
                                                                   (progn ,@body))
                                                          (semaphore-signal (concurrency-of ,formula))))))
                                          (declare (ignorable thread))
                                          (when *creating-formula*
                                            (join-thread thread))))
                                     `(lambda (,event-sym)
                                        (declare (ignorable ,event-sym))
                                        (setf (value-of ,formula)
                                              (progn ,@body))))))
           (prog1 ,formula
             ;; Trigger an initial sync which will also create the connections
             ;; which will take care of automatic syncing in the future.
             (let ((*creating-formula* ,formula))
               (funcall (the function (closure-of ,formula)) nil))
             (assert (sources-of ,formula) nil
                     "The MK-FORMULA form was not able to automatically determine what resources
it is to monitor for changes.")))))))


(defmacro formula-of (&body body)
  `(let ((*get-formula-p* t)
         (*creating-formula* nil))
     (let ((result (progn ,@body)))
       (unless (typep result 'formula)
         (warn "FORMULA-OF: Returning something not a formula; ~A" result))
       result)))
