;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container-remove-mvc ()
  ((remove-event :reader remove-event-of
                 :type (or null container-remove)
                 :initform nil))
  
  (:metaclass mvc-stm-class))



(defclass container-remove (container-event)
  ()
  
  (:documentation "
This represent various ways of removing an OBJECT from a CONTAINER."))


(defmethod handle ((event container-remove))
  (let ((container (container-of event)))
    (container-remove event container)
    ;; TODO: This currently only notifies the CONTAINER; what about notifying the objects which are removed?
    (when (typep container 'container-remove-mvc)
      (with-object container
        (setf ¤remove-event event
              ¤remove-event nil)))))


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
