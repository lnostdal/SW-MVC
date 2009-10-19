;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)
(in-readtable sw-mvc)

(declaim #.(optimizations))


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


;; TODO: It'd be nicer to have a separate event for this.
(defun remove-all (container)
  (declare ((or multiple-value-model view-base) container))
  (let ((objects (with1 ~(model-of container)  ;; [VIEW-BASE ->] CONTAINER -> NODES
                   (map-into it #'deref it)))) ;; NODES -> OBJECTS
    (when objects
      (handle (make-instance 'container-remove
                             :container container
                             :objects objects)))))
