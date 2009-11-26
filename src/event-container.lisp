;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass container-event (event)
  ;; TODO: I don't think this slot belongs here; it is too specific.
  ((containers :reader containers-of
               :type list
               :documentation "
The CONTAINER instances in question which has an event.")

   ;; TODO: I don't think this slot belongs here; it is too specific.
   (objects :reader objects-of
            :type list
            :documentation "
A list of MODELs to which the event is applied or related to in some way.")))


(defmethod initialize-instance :after ((event container-event) &key
                                       (container (error ":CONTAINER needed."))
                                       ;; TODO: (containers ..)
                                       (object nil object-supplied-p)
                                       (objects nil objects-supplied-p))
  (assert (xor object-supplied-p objects-supplied-p) nil
          ":OBJECT or :OBJECTS needed.")

  (setf (slot-value event 'containers)
        (ensure-list (ensure-container container)))
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


#| TODO: The two following methods are stupid and fragile. At the moment I cannot think of a better way to do this.
I cannot make a copy of the EVENT instance and change the CONTAINER slot in the copy and "forward" it by sending it
to the HANDLE method etc.. |#
(defmethod container-of ((event container-event))
  (find-if (lambda (c) (not (typep c 'proxied-container)))
           (containers-of event)))


(defmethod proxied-container-of ((event container-event))
  (find-if (lambda (c) (typep c 'proxied-container))
           (containers-of event)))


(defmethod observables-of append ((event container-event))
  (append (containers-of event)
          (objects-of event)))


(defmethod object-of ((event container-event))
  (let ((objects (objects-of event)))
    (assert (sequence-of-length-p objects 1) nil "
The use of OBJECT-OF expects that the event in question (~A) will or must contain only one element." event)
    (first objects)))
