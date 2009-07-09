;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass event ()
  ())


(defgeneric handle (event)
  (:documentation "
This \"handles\" or \"executes\" the EVENT."))


(defgeneric observables-of (event)
  (:method-combination append)
  (:documentation "
This returns a list of objects which can have observers interested in knowing
about the event."))