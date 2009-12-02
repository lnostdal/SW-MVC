;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(eval-now
  (defclass event ()
    ()))


(eval-now
  (defclass event-router ()
    ((event :reader event-of
            :type (or event null)
            :initform nil))

    (:metaclass mvc-class)))


(defgeneric observables-of (event)
  (:method-combination append)
  (:documentation "This returns a list of objects which can have observers interested in knowing about the event."))


(defmethod handle ((event event))
 "This method notifies any observers of (OBSERVABLES-OF ..) the EVENT. Methods with :BEFORE and :AFTER qualifiers tend to be the ones actually \"executing\" the event \"locally\". I.e., it is useful to signal observers about stuff about to be removed :BEFORE it actually is removed."
 (dolist (observable (observables-of event))
   (when (typep observable 'event-router)
     (event-router-notify observable event))))


(defmethod event-router-notify ((event-router event-router) (event event))
  (pulse (slot-value event-router 'event) event))