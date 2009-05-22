;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container-exchange (container-event)
  ((target-position :reader target-position-of :initarg :target-position
                    :initform (error ":TARGET-POSITION needed."))))


(defmethod handle ((event container-exchange))
  (container-exchange event (container-of event)))


(defmethod exchange (object-1 object-2)
  (assert (eq (container-of object-1)
              (container-of object-2))
          nil
          "EXCHANGE: Exchange between two containers not implemented.")
  (handle (make-instance 'container-exchange
                         :container (container-of object-1)
                         :object object-1
                         :target-position object-2)))
