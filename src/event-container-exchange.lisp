;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass container-exchange (container-event)
  ((target-position :reader target-position-of :initarg :target-position
                    :initform (error ":TARGET-POSITION needed."))))


(defmethod observables-of append ((event container-exchange))
  (list (target-position-of event)))


(defmethod handle ((event container-exchange))
  (let ((container (container-of event)))
    (prog1 (container-exchange event container)
      ;; Notify stuff observing the container and the objects being exchanged.
      (dolist (observable (observables-of event))
        (when (typep observable 'event-router)
          (event-router-notify observable event))))))


(defmethod exchange (object-1 object-2)
  (declare ((or model view-base) object-1 object-2))
  (assert (eq (container-of object-1)
              (container-of object-2))
          nil "SW-MVC:EXCHANGE of objects between two different containers is not implemented.")
  (handle (make-instance 'container-exchange
                         :container (container-of object-1)
                         :object object-1
                         :target-position object-2)))
