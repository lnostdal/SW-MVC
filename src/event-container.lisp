;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container-event (event)
  ((container :reader container-of :initarg :container
              :type container
              :initform (error ":CONTAINER needed.")
              :documentation "
The container instance in question which had an event.")

   ;; Initialization of this slot happens below.
   (objects :reader objects-of 
            :type list
            :documentation "
The objects that where for instance removed or added to the container in
question. The content of this slot might change while it is being handled.")))


(defmethod initialize-instance :after ((event container-event) &key
                                       (object nil object-supplied-p)
                                       (objects nil objects-supplied-p))
  ;; Initialization of the OBJECTS slot using an :OBJECT or :OBJECTS initarg.
  (setf (slot-value event 'objects)
        (cond
          (object-supplied-p
           (list object))

          (objects-supplied-p
           objects)

          (t
           (error ":OBJECT or :OBJECTS needed.")))))


(defmethod observables-of append ((event container-event))
  (cons (container-of event)
        (objects-of event)))


(defmethod object-of ((event container-event))
  (let ((objects (objects-of event)))
    (assert (= 1 (length objects)) nil "
The use of OBJECT-OF expects that the container in question (~A) will or must
contain only one element." (container-of event))
    (first objects)))

