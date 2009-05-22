;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container-add (container-event)
  ()

  (:documentation "
This represent the various ways of adding an OBJECT to a CONTAINER."))


(defmethod handle ((event container-add))
  (container-add event (container-of event)))


(defun add (object container)
  "Add OBJECT to CONTAINER.
Returns CONTAINER."
  (handle (make-instance 'container-add
                         :object object
                         :container container))
  container)


(defun add-to (container &rest objects)
  "Add OBJECTS to CONTAINER.
Returns CONTAINER."
  (handle (make-instance 'container-add
                         :objects objects
                         :container container))
  container)
