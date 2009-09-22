;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(eval-now
(defclass dlist-node (node single-value-model)
  ((left :accessor left-of :initarg :left
         :initform nil)

   (right :accessor right-of :initarg :right
          :initform nil)

   (value :accessor value-of
          :initform ":VALUE needed."))

  (:metaclass mvc-class)
  (:documentation "
Doubly-linked list node with support for dataflow and transactions."))



(defclass dlist (container event-router)
  ((head :accessor head-of :initarg :head
         :initform nil)

   (tail :accessor tail-of
         :initform nil))

  (:default-initargs :key-fn (lambda (obj) (value-of obj)))
  (:metaclass mvc-class)
  (:documentation "
Doubly-linked list with support for dataflow and transactions."))
)


(defmethod initialize-instance :after ((dlist-node dlist-node) &key
                                       (dlist nil dlist-supplied-p)
                                       (value (error ":VALUE needed.")))
  (setf (value-of dlist-node) (model-of value))
  (when dlist-supplied-p
    (setf (container-of dlist-node) dlist)))


(defmethod print-object ((dlist-node dlist-node) stream)
  (print-unreadable-object (dlist-node stream :type t :identity t)
    (when (slot-boundp dlist-node 'value)
      (prin1 (value-of dlist-node) stream))))


(defmethod deref ((dlist-node dlist-node))
  (slot-value dlist-node 'value))


(defmethod (setf deref) (new-value (dlist-node dlist-node))
  (setf (slot-value dlist-node 'value) new-value))


(defmethod (setf container-of) :after ((dlist dlist) (dlist-node dlist-node))
  (setf (node-in-context-of dlist (value-of dlist-node))
        dlist-node))


(defmethod list<- ((dlist dlist) &rest args)
  "Return a snapshot view of DLIST in form of a CONS-based Lisp list.
If this is done while in a WITH-SYNC context it'll basically mean exclusive
access to the entire DLIST for the duration of the WITH-SYNC form."
  (declare (ignore args))
  ;; Yes, you probably don't want to do this too often.
  (when-let ((head (head-of dlist)))
    (tail-of dlist) ;; Touch (transaction).
    (collecting
      (do ((node head (right-of node)))
          ((null node))
        (left-of node) ;; Touch (transaction).
        (collect node)))))


(defmethod dlist<- ((list list))
  (apply #'dlist list))


(defmethod deref ((dlist dlist))
  (list<- dlist))


(defmethod (setf deref) ((new-values list) (dlist dlist))
  (error "TODO: (SETF DEREF .. DLIST). Or, does this even make sense?"))


(flet ((fill-dlist (dlist items)
         (when items
           (let ((tail (setf (head-of dlist)
                             (make-instance 'dlist-node
                                            :dlist dlist
                                            :value (first items)))))
             (dolist (item (rest items))
               (setf tail
                     (setf (right-of tail)
                           (make-instance 'dlist-node
                                          :dlist dlist
                                          :left tail
                                          :value item))))
             (setf (tail-of dlist) tail)))))


  (defun dlist (&rest items)
    (declare (dynamic-extent items))
    (with1 (make-instance 'dlist)
      (fill-dlist it items)))


  #|(defmethod transform-into ((target dlist) (source list))
    "Transform TARGET container to contain the values in SOURCE by initiating
container type events vs. TARGET."
    (let ((key (key-fn-of target))
          (test (test-fn-of target))
          (before (list<- target)) ;; DLIST-NODE instances.
          (after nil)
          (to-insert nil))
      (declare (list before after to-insert)
               (function key test))

      ;; TARGET is empty.
      (unless before
        (when source
          (fill-dlist target source))
        ;; TODO: Hm. We're returning without triggering any "container type events".
        (return-from transform-into))

      (dolist (value source (nreversef after))
        (if-let (node (find value before :key key :test test))
          (push node after)
          (push (make-instance 'dlist-node :value value :dlist target)
                to-insert)))

      ;; REMOVE.
      (dolist (removed-element (set-difference before after :test #'eq))
        (deletef before removed-element :test #'eq)
        (remove removed-element target))

      ;; EXCHANGE.
      (let ((already-swapped nil))
        (map nil (lambda (before-elt after-elt)
                   (when (and (not (funcall test (deref before-elt) (deref after-elt)))
                              (not (find after-elt already-swapped :test #'eq)))
                     (push before-elt already-swapped)
                     (exchange before-elt after-elt)))
             before
             after))

      ;; INSERT.
      (if before
          (let ((last-node (last1 before)))
            (dolist (node to-insert)
              (if-let ((right-val (cadr (member (funcall key node) source :test test))))
                (insert node :before (node-in-context-of target right-val) #|(node-of right-val)|#)
                (insert node :after last-node))))
        (insert (nreversef to-insert) :in target))))|#)


(defmethod container-remove ((event container-remove) (dlist dlist))
  (dolist (object (objects-of event) (length (objects-of event)))
    (let* ((node (node-in-context-of (container-of event) object))
           (left (left-of node))
           (right (right-of node)))
      (nilf (container-of node)
            (left-of node)
            (right-of node))
      (if left
          (setf (right-of left) right)
          (setf (head-of dlist) right))
      (if right
          (setf (left-of right) left)
          (setf (tail-of dlist) left)))))


(defmethod container-insert ((event container-insert) (dlist dlist))
  (let ((relative-position (relative-position-of event))
        (relative-object (relative-object-of event)))

    (dolist (object (objects-of event) (objects-of event))
      (let ((dlist-node (make-instance 'dlist-node :dlist dlist :value object)))
        (ecase relative-position
          (:before
           (with-slots (head tail) dlist
             (with-slots (left right) dlist-node
               (setf left (left-of relative-object)
                     right relative-object)
               (if (left-of relative-object)
                   (setf (right-of (left-of relative-object)) dlist-node
                         (left-of relative-object) dlist-node)
                   (setf head dlist-node
                         (left-of relative-object) dlist-node)))))

          (:after
           (with-slots (head tail) dlist
             (with-slots (left right) dlist-node
               (setf left relative-object
                     right (right-of relative-object))
               (if (right-of relative-object)
                   (setf (left-of (right-of relative-object)) dlist-node
                         (right-of relative-object) dlist-node)
                   (setf tail dlist-node
                         (right-of relative-object) dlist-node)))))

          ((nil)
           (with-slots (head tail) dlist
             (with-slots (left) dlist-node
               (if tail
                   (setf left tail
                         (right-of tail) dlist-node
                         tail dlist-node)
                   (setf head dlist-node
                         tail dlist-node))))))

        (setf relative-position :after
              relative-object dlist-node)))))


(defmethod container-exchange ((event container-exchange) (dlist dlist))
  (let* ((object-a (target-position-of event))
         (object-b (object-of event))
         (a-value (value-of object-a)))
    (setf (value-of object-a) (value-of object-b)
          (value-of object-b) a-value)
    (list object-a object-b)))
