;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass container-with-1-active-item (dlist)
  ((active-item :accessor active-item-of
                :type (or null model)
                :initform nil))

  (:metaclass mvc-class))
(export '(container-with-1-active-item active-item-of))


(flet ((%insert (container event)
         (dolist (object (objects-of event))
           (unless (or (eq object container)
                       (active-item-of container))
             (setf (active-item-of container) object))))

       (%remove (container event)
         (dolist (object (objects-of event))
           (when (and (not (eq object container))
                      (eq object (active-item-of container)))
             (setf (active-item-of container) :closest)))))


  (defmethod initialize-instance :after ((container container-with-1-active-item) &key)
    (with-formula container
      (when-let (event (event-of container))
        (when (eq container (container-of event))
          (typecase event
            (container-insert (%insert container event))
            (container-remove (%remove container event))))))))


(defmethod (setf active-item-of) ((val (eql :closest)) (container container-with-1-active-item))
  (let ((current-node (node-in-context-of container (active-item-of container))))
    (check-type current-node node)
    (setf (slot-value container 'active-item)
          (or ~(right-of current-node)
              ~(left-of current-node)))))




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
