;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container-remove (container-event)
  ()

  (:documentation "
This represent various ways of removing an OBJECT from a CONTAINER."))


(defmethod handle ((event container-remove))
  (let ((container (container-of (container-of event))))
    (prog1 (container-remove event container)
      (dolist (observable (observables-of event))
        ;; Notify stuff observing the container and the objects being removed.
        (when (typep observable 'event-router)
          (event-router-notify observable event))))))


(defun remove (object container)
  "Remove OBJECT from CONTAINER.
Returns CONTAINER."
  (declare ((or model view-base) container))
  (dolist (object (setf object (ensure-list object)))
    (check-type object (or model view-base)))
  (handle (make-instance 'container-remove
                         :container container
                         :objects object)))


(defun remove-from (container &rest objects)
  "Remove OBJECTS from CONTAINER.
Returns CONTAINER."
  (declare ((or model view-base) container))
  (dolist (object objects)
    (check-type object (or model view-base)))
  (handle (make-instance 'container-remove
                         :container container
                         :objects objects)))


(defun remove-all (container)
  (declare ((or model view-base) container))
  (let ((objects ~(container-of container)))
    (when objects
      (handle (make-instance 'container-remove
                             :container container
                             :objects objects)))))
