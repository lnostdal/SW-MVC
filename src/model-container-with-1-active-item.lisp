;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defclass container-with-1-active-item (dlist)
  ((active-item :reader active-item-of
                ;;:type (or null 1-active-item)
                :initform nil))

  (:metaclass mvc-class))
(export '(container-with-1-active-item active-item-of))


(flet ((%insert (container event)
         (dolist (object (objects-of event))
           (unless (active-item-of container)
             (setf (active-item-of container) (node-of object)))))


       (%remove (container event)
         (dolist (object (objects-of event))
           (when (eq (node-of object) (active-item-of container))
             (setf (active-item-of container) :closest)))))


  (defmethod initialize-instance :after ((container container-with-1-active-item) &key)
    (let ((old-active-item #λnil))
      (with-formula container
        (with (active-item-of container)
          (when (not (eq it ~old-active-item))
            (setf ~old-active-item it)))))

    (with-formula container
      (when-let (event (event-of container))
        (typecase event
          (container-insert (%insert container event))
          (container-remove (%remove container event)))))))


(defmethod (setf active-item-of) ((item model) (container container-with-1-active-item))
  (assert (eq (container-of (node-of item)) container) nil
          "Trying to set ~S as active item of ~S,~%but that item is not a member of the container."
          item container)
  (setf (slot-value container 'active-item) (node-of item)))


(defmethod (setf active-item-of) ((val (eql nil)) (container container-with-1-active-item))
  (nilf (slot-value container 'active-item)))


(defmethod (setf active-item-of) ((val (eql :closest)) (container container-with-1-active-item))
  (let ((current-object (active-item-of container)))
    (setf (slot-value container 'active-item)
          (or (right-of current-object)
              (left-of current-object)))))




;;(terpri) (terpri)
#|(with (make-instance 'container-with-1-active-item)
  (let ((x #λ2) (y #λ4))
    (dbg-prin1 (active-item-of it))
    (insert (list x y) :in it)
    (dbg-prin1 (active-item-of it))
    (remove x it)
    (dbg-prin1 (active-item-of it))
    (remove y it)
    ;;(setf (active-item-of it) x)
    #|(insert y :in it)|#
    #|(setf (active-item-of it) y)|#
    (dbg-prin1 (active-item-of it))
    ))|#






























;; TODO: This stuff might not make all that much sense now.
#|(defclass 1-active-item ()
  ((container :accessor container-of :initarg :container
              :type (or null container-with-1-active-item)
              :initform nil)

   (active-p :reader active-p-of :initarg :active-p
             :type (member t nil)
             :initform nil))

  (:metaclass mvc-class))|#


#|(defmethod (setf active-p-of) (new-state (item 1-active-item))
  )|#