;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container-remove-mvc ()
  ((remove-event :reader remove-event-of
                 :cellp t
                 :initform nil))

  (:metaclass mvc-class))



(defclass container-remove (container-event)
  ()

  (:documentation "
This represent various ways of removing an OBJECT from a CONTAINER."))


(defmethod handle ((event container-remove))
  (let ((container (container-of event)))
    (container-remove event container)
    (dolist (observable (observables-of event))
      ;; Notify stuff observing the container and the objects being removed.
      (when (typep observable 'container-remove-mvc)
        (with-object observable
          (pulse ¤remove-event event))))))


(defun remove (object container)
  "Remove OBJECT from CONTAINER.
Returns CONTAINER."
  (handle (make-instance 'container-remove
                         :container container
                         :objects object))
  container)


(defun remove-from (container &rest objects)
  "Remove OBJECTS from CONTAINER.
Returns CONTAINER."
  (handle (make-instance 'container-remove
                         :container container
                         :objects objects))
  container)


(defun remove-all (container)
  (let ((objects ~container))
    (when objects
      (handle (make-instance 'container-remove
                             :container container
                             :objects objects)))))
