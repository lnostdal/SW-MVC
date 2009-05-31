;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass event ()
  ())


(defgeneric handle (event)
  (:documentation "
This \"handles\" or \"executes\" the EVENT."))


(defmethod handle :around (event)
  (with-simple-restart (abort-mvc-event "SW-MVC: Abort the SW-MVC event; ~A" event)
    (call-next-method)))
