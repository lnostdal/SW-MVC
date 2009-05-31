;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container-exchange-mvc ()
  ((exchange-event :reader exchange-event-of
                   :type (or null container-exchange)
                   :initform nil))

  (:metaclass mvc-stm-class))



(defclass container-exchange (container-event)
  ((target-position :reader target-position-of :initarg :target-position
                    :initform (error ":TARGET-POSITION needed."))))


(defmethod handle ((event container-exchange))
  (let ((container (container-of event)))
    (container-exchange event container)
    ;; Notify stuff observing the container and the objects being exchanged.
    (dolist (observable (observables-of event))
      (when (typep observable 'container-exchange-mvc)
        (with-object observable
          (setf ¤exchange-event event
                ¤exchange-event nil))))))


(defmethod exchange (object-1 object-2)
  (assert (eq (container-of object-1)
              (container-of object-2))
          nil
          "EXCHANGE: Exchange between two containers not implemented.")
  (handle (make-instance 'container-exchange
                         :container (container-of object-1)
                         :object object-1
                         :target-position object-2)))
