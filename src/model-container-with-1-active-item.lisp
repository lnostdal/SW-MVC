;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass container-with-1-active-item (proxied-container mvc-class-observer null-container event-router)
  ((active-item :accessor active-item-of
                :type (or null model)
                :initform nil)

   (fallback-item :accessor fallback-item-of
                  :type (or null model)
                  :initform nil
                  :documentation "
The item assigned to this slot will be the new ACTIVE-ITEM if the current ACTIVE-ITEM is the one being REMOVEd.
If no item is assigned to this slot (NIL), then the :CLOSEST (see the (SETF ACTIVE-ITEM-OF method)) item will be
selected as the new ACTIVE-ITEM."))

  (:metaclass mvc-class)
  (:documentation "
This works as a proxy for a back-end container Model instance and is used to keep track of which item is \"active\"
in that container. There may be multiple instances of this class all assigned with the same back-end container Model
instance. This means that a container may have multiple \"one active items\" given different contexts, e.g., Views.
This is particularly useful when implementing the \"remove item → FALLBACK-ITEM support\" for a combo-box (SW:COMBO-
BOX) or list-box widget or similar where you might want different FALLBACK-ITEM behavior for each View (user)
instance."))
(export '(container-with-1-active-item active-item-of fallback-item-of))


(defmethod container-of ((proxy container-with-1-active-item))
  "The container of a proxied container is the MODEL slot (VIEW-BASE)."
  (model-of proxy))


(flet ((%insert (proxy event)
         (dolist (object (objects-of event))
           (unless (active-item-of proxy)
             (setf (active-item-of proxy) object))))


       (%remove (proxy event)
         (dolist (object (objects-of event))
           (when (eq object (active-item-of proxy)) ;; Removing currently active object.
             (setf (active-item-of proxy)
                   (if-let (fallback-item (and (not (eq proxy (proxied-container-of event)))
                                               (fallback-item-of proxy)))
                     (progn
                       (assert (not (eq object fallback-item)) nil
                               "If you really want to do this then set the FALLBACK-ITEM slot to NIL first.")
                       fallback-item)
                     :closest))))))


  (defmethod set-model nconc ((proxy container-with-1-active-item) (container multiple-value-model))
    (list λI(when-let (event (event-of container))
              (when (eq container (container-of event))
                (typecase event
                  (container-insert (%insert proxy event))
                  (container-remove (%remove proxy event))))))))


(defmethod (setf active-item-of) ((val (eql :closest)) (proxy container-with-1-active-item))
  (let ((current-node (node-in-context-of ~proxy (active-item-of proxy))))
    (check-type current-node node)
    (setf (active-item-of proxy)
          (or ~(right-of current-node)
              ~(left-of current-node)))))


(defmethod (setf fallback-item-of) (item (proxy container-with-1-active-item))
  (when item
    (assert (with (node-in-context-of ~proxy item)
              (check-type it node)
              (eq ~proxy (container-of it)))
            nil
            "~A must already be a member of the container ~A for it to be assigned as a FALLBACK-ITEM."
            item ~proxy))
  (setf (slot-value proxy 'fallback-item) item))



;;(terpri) (terpri)
#|(with (make-instance 'container-with-1-active-item)
  (let ((x λV2) (y λV4))
    (dbg-prin1 ~(active-item-of it))
    (insert (list x y) :in it)
    (dbg-prin1 ~(active-item-of it))
    (remove x it)
    (dbg-prin1 ~(active-item-of it))
    (remove y it)
    (dbg-prin1 ~(active-item-of it))
    (insert λV42 :in it)
    (dbg-prin1 ~(active-item-of it))
    ))|#



#|(with (dlist λV0 λV1)
  (let ((p1 (make-instance 'container-with-1-active-item :model it))
        (p2 (make-instance 'container-with-1-active-item :model it)))
    (setf (active-item-of p1) (first ~~it)
          (active-item-of p2) (first ~~it))
    (dbg-prin1 ~(active-item-of p1))
    (dbg-prin1 ~(active-item-of p2))
    (remove (active-item-of p1) p2)
    (dbg-prin1 ~(active-item-of p1))
    (dbg-prin1 ~(active-item-of p2))))|#
