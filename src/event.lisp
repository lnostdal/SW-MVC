;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(declaim (optimize speed))


(defclass event ()
  ())


(defgeneric observables-of (event)
  (:method-combination append)
  (:documentation "
This returns a list of objects which can have observers interested in knowing
about the event."))


(defgeneric handle (event)
  (:documentation "
This \"handles\" or executes the EVENT. By default, PROPAGATE is called
after this method-combination is done."))


(defgeneric propagate (event)
  (:documentation "
This forwards the EVENT to others without \"handling\" or executing it
ourselves."))


(defmethod handle :around (event)
  #| With transactions (aromyxo/concurrency/transaction.lisp) introduced this
  might not be very interesting seeing as one could just have a nested
  transaction here? On the other hand creating one transaction for every single
  operation spawned and handled would probably be very slow. |#
  (with-simple-restart (abort-mvc-event "Abort the SW-MVC event; ~A" event)
    (call-next-method)))


(defmethod handle :after (event)
  (propagate event))


(defmethod propagate (event)
  (dolist (observable (observables-of event))
    (with-callbacks (observable)
      (if (typep observer 'view-base)
          (when-commit () (funcall callback event))
          (funcall callback event)))))


