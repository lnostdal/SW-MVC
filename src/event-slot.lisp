;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
=common-headers=


(defclass slot-event (event)
  ((context :reader context-of :initarg :context
            :initform (error ":CONTEXT needed."))

   (object :reader object-of)

   (slot-name :reader slot-name-of :initarg :slot-name
              :type symbol
              :initform (error ":SLOT-NAME needed.")))

  (:documentation "
This works in collaboration with ADD-SLOT-OBSERVERS (util.lisp)."))


(defmethod initialize-instance :after ((event slot-event) &key
                                       (object nil object-supplied-p))
  (check-type (slot-value event 'context) event-router)
  (assert object-supplied-p nil ":OBJECT needed.")
  (setf (slot-value event 'object)
        (ensure-model object)))


(defmethod objects-of ((event slot-event))
  (ensure-list (object-of event)))


(defmethod observables-of append ((event slot-event))
  (list (context-of event)))


(defun slot-set (instance slot-name context)
  (handle (make-instance 'slot-event
                         :object instance :slot-name slot-name
                         :context context)))
