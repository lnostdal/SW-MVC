;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass event ()
  ())


(defclass event-router ()
  ((event :reader event-of
          :initform nil))

  (:metaclass mvc-class))


(defgeneric observables-of (event)
  (:method-combination append)
  (:documentation "
This returns a list of objects which can have observers interested in knowing
about the event."))


(defmethod handle ((event event))
  "This \"handles\" or \"executes\" the EVENT."
  (dolist (observable (observables-of event))
    (when (typep observable 'event-router)
      (event-router-notify observable event))))


(defmethod event-router-notify ((event-router event-router) (event event))
  (pulse (slot-value event-router 'event) event))