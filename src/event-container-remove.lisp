;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container-remove (container-event)
  ()
  
  (:documentation "
This represent various ways of removing an OBJECT from a CONTAINER."))


(defmethod handle ((event container-remove))
  (container-remove event (container-of event)))


(defun remove (object container)
  "Remove OBJECT from CONTAINER.
Returns CONTAINER."
  (handle (make-instance 'container-remove
                         :object object
                         :container container))
  container)


(defun remove-from (container &rest objects)
  "Remove OBJECTS from CONTAINER.
Returns CONTAINER."
  (handle (make-instance 'container-remove
                         :objects objects
                         :container container))
  container)
