;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass container-event (event)
  ((container :reader container-of
              :type container
              :documentation "The CONTAINER instance in question which has an event.")

   (container-proxy :reader container-proxy-of
                    :type (or null container-proxy)
                    :initform nil
                    :documentation "If non-NIL this holds a CONTAINER-PROXY, which in turn points to or holds the same container as the CONTAINER slot.")

   (objects :reader objects-of
            :type list
            :documentation "A list of MODELs to which the event is applied or related to in some way.")))


(defmethod initialize-instance :after ((event container-event) &key
                                       (container (error ":CONTAINER needed."))
                                       (object nil object-supplied-p)
                                       (objects nil objects-supplied-p))
  (assert (xor object-supplied-p objects-supplied-p) nil
          ":OBJECT or :OBJECTS needed.")

  (labels ((assign-containers (container)
             (etypecase container
               (container-proxy
                (setf (slot-value event 'container) (model-of container)
                      (slot-value event 'container-proxy) container))

               (mvc-class-observer
                (assign-containers (model-of container)))

               (container
                (setf (slot-value event 'container) container)))))
    (assign-containers container))

  (setf (slot-value event 'objects)
        (with (cond
                (object-supplied-p
                 (list object))

                (objects-supplied-p
                 (if (atom objects)
                     (list objects)
                     objects)))
          (map-into it (λ (obj) (ensure-model obj))
                    it))))


(defmethod observables-of append ((event container-event))
  (with (cons (container-of event) (objects-of event))
    (if-let (proxy (container-proxy-of event))
      (cons proxy it)
      it)))


(defmethod object-of ((event container-event))
  (let ((objects (objects-of event)))
    (assert (sequence-of-length-p objects 1) nil "The use of OBJECT-OF expects that the event in question (~A) will or must contain only one element." event)
    (first objects)))
