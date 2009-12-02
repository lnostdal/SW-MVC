;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
=common-headers=


(defclass container-remove (container-event)
  ()

  (:documentation "
This represent various ways of removing an OBJECT from a CONTAINER."))


(defmethod handle :after ((event container-remove))
  (container-remove event (container-of event)))


(defun remove (object container)
  (declare ((or multiple-value-model view-base) container))
  (dolist (object (setf object (ensure-list object)))
    (check-type object (or model view-base)))
  (handle (make-instance 'container-remove
                         :container container
                         :objects object)))


(defun remove-from (container &rest objects)
  (declare ((or multiple-value-model view-base) container))
  (when objects
    (dolist (object objects)
      (check-type object (or model view-base)))
    (handle (make-instance 'container-remove
                           :container container
                           :objects objects))))



(defclass container-remove-all (container-event)
  ()
  (:default-initargs :object +null-model+))


(defmethod handle :after ((event container-remove-all))
  (container-remove-all event (container-of event)))


(defun remove-all (container)
  (declare ((or multiple-value-model view-base) container))
  (unless (empty-p-of container)
    (handle (make-instance 'container-remove-all
                           :container container))))
