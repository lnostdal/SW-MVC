;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass container-with-1-active-item (dlist)
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

  (:metaclass mvc-class))
(export '(container-with-1-active-item active-item-of fallback-item-of))


(flet ((%insert (container event)
         (dolist (object (objects-of event))
           (unless (or (eq object container)
                       (active-item-of container))
             (setf (active-item-of container) object))))

       (%remove (container event)
         (dolist (object (objects-of event))
           (when (and (not (eq object container))
                      (eq object (active-item-of container)))
             (setf (active-item-of container)
                   (if-let (fallback-item (fallback-item-of container))
                     fallback-item
                     :closest))))))


  (defmethod initialize-instance :after ((container container-with-1-active-item) &key)
    (with-formula container
      (when-let (event (event-of container))
        (when (eq container (container-of event))
          (typecase event
            (container-insert (%insert container event))
            (container-remove (%remove container event))))))))


(defmethod (setf active-item-of) ((val (eql :closest)) (container container-with-1-active-item))
  (let ((current-node (node-in-context-of container (active-item-of container))))
    (check-type current-node dlist-node)
    (setf (slot-value container 'active-item)
          (or ~(right-of current-node)
              ~(left-of current-node)))))


(defmethod (setf fallback-item-of) :before ((item model) (container container-with-1-active-item))
  (assert (with (node-in-context-of container item)
            (and it (eq container (container-of it))))
          nil
          "~A must already be a member of the container ~A for it to be assigned as a FALLBACK-ITEM."
          item container))




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
