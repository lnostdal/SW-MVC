;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass container-exchange (container-event)
  ((target-position :reader target-position-of :initarg :target-position
                    :initform (error ":TARGET-POSITION needed."))))


(defmethod observables-of append ((event container-exchange))
  (list (target-position-of event)))


(defmethod handle :before ((event container-exchange))
  (dolist (container (containers-of event))
    (container-exchange event container)))


(defmethod exchange (object-1 object-2)
  (declare ((or model view-base) object-1 object-2))
  (assert (eq (container-of object-1)
              (container-of object-2))
          nil "SW-MVC:EXCHANGE of objects between two different containers is not implemented.")
  (handle (make-instance 'container-exchange
                         :container (container-of object-1)
                         :object object-1
                         :target-position object-2)))
