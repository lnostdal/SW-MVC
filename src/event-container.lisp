;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container-event (event)
  ;; TODO: I don't think this slot belongs here; it is too specific. We often manipulate many containers.
  ((container :reader container-of :reader model-of :initarg :container
              ;;:type container ;; NOTE: This might be a Model or a View.
              :initform (error ":CONTAINER needed.")
              :documentation "
The container instance in question which had an event.")

   ;; TODO: I don't think this slot belongs here; it is too specific.
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
           (if (atom objects)
               (list objects)
               ;; The (CONTAINER-INSERT CONTAINER-INSERT DLIST) method (for one..) mutates the OBJECTS slot.
               (copy-seq objects)))

          (t
           (error ":OBJECT or :OBJECTS needed.")))))


(defmethod observables-of append ((event container-event))
  (with (cons (container-of event)
              (objects-of event))
    (let ((maybe-container-model (container-of (car it))))
      (if (eq (car it) maybe-container-model)
          it
          (cons maybe-container-model it)))))


(defmethod object-of ((event container-event))
  (let ((objects (objects-of event)))
    (assert (= 1 (length objects)) nil "
The use of OBJECT-OF expects that the event in question (~A) will or must
contain only one element." event)
    (first objects)))
