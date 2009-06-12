;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass event ()
  ())


(defgeneric handle (event)
  (:documentation "
This \"handles\" or \"executes\" the EVENT."))


(defmethod handle :around (event)
  (let ((*event-stack* (cons event *event-stack*)))
    (with-simple-restart (abort-mvc-event "SW-MVC: Abort the SW-MVC event; ~A" event)
      (call-next-method))))


(defgeneric observables-of (event)
  (:method-combination append)
  (:documentation "
This returns a list of objects which can have observers interested in knowing
about the event."))