;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))

#| TODO: This thing is slowly morphing into a tree type of thing.
I should refactor most of these slots into their own mixin classes, I think.
|#


(eval-when (:compile-toplevel :load-toplevel)
  (defclass dlist-node () () (:metaclass mvc-stm-class))
  (defclass dlist () () (:metaclass mvc-stm-class)))


(defclass dlist-node (single-value-model)
  ((dlist :accessor dlist-of :accessor container-of :initarg :dlist
          :type (or dlist null)
          :initform nil)

   (parent :accessor parent-of :initarg :parent
           :type (or dlist-node null)
           :initform nil)

   (left :accessor left-of :initarg :left
         :type (or dlist-node null)
         :initform nil)

   (right :accessor right-of :initarg :right
          :type (or dlist-node null)
          :initform nil)

   (value :accessor value-of :initarg :value
          :initform (error ":VALUE needed.")))

  (:metaclass mvc-stm-class)
  (:documentation "
Doubly-linked list node with support for dataflow and transactions."))


(defmethod print-object ((dlist-node dlist-node) stream)
  (print-unreadable-object (dlist-node stream :type t :identity t)
    (prin1 (value-of dlist-node) stream)))


(defmethod deref ((dlist-node dlist-node))
  (slot-value dlist-node 'value))


(defmethod (setf deref) (new-value (dlist-node dlist-node))
  (setf (slot-value dlist-node 'value) new-value))



(defclass dlist (container
                 container-insert-mvc container-remove-mvc container-exchange-mvc)
  ((head :accessor head-of :initarg :head
         :type (or dlist-node null)
         :initform nil)

   (tail :accessor tail-of
         :type (or dlist-node null)
         :initform nil))

  (:default-initargs :key-fn #'value-of)
  (:metaclass mvc-stm-class)
  (:documentation "
Doubly-linked list with support for dataflow and transactions."))


(defmethod list<- ((dlist dlist) &rest args)
  "Return a snapshot view of DLIST in form of a CONS-based Lisp list.
If this is done while in a WITH-SYNC context it'll basically mean exclusive
access to the DLIST for the duration of the WITH-SYNC form."
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


(defun dlist (&rest items)
  (declare (dynamic-extent items))
  (let ((dlist (make-instance 'dlist)))
    (prog1 dlist
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
          (setf (tail-of dlist) tail))))))


(defmethod transform-into ((target dlist) (source list))
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
    ;; TODO: This is basically a copy of the code in #'DLIST
    (unless before
      (when source
        (let ((tail (setf (head-of target)
                          (make-instance 'dlist-node
                                         :dlist target
                                         :value (first source)))))
          (dolist (value (rest source))
            (setf tail
                  (setf (right-of tail)
                        (make-instance 'dlist-node
                                       :dlist target
                                       :left tail
                                       :value value))))
          (setf (tail-of target) tail)))
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
              ;; TODO: CONTAINER-FIND currently converts the DLIST to a CONS-list on each iteration.
              (insert node :before (container-find right-val target))
              (insert node :after last-node))))
        (insert (nreversef to-insert) :in target))))



(defmethod container-remove ((event container-remove) (dlist dlist))
  (dolist (object (objects-of event))
    ;; TODO: Find all objects in one go instead.
    (let* ((node (if (typep object 'dlist-node)
                     object
                     (container-find object dlist)))
           (left (left-of node))
           (right (right-of node)))
      (nilf (dlist-of node)
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
    (flet ((mk-dlist-node (object)
             (if (typep object 'dlist-node)
                 (prog1 object (setf (dlist-of object) dlist))
                 (make-instance 'dlist-node :dlist dlist :value object))))
      (declare (inline mk-dlist-node))

      (dolist (object (objects-of event))
        (let ((dlist-node (mk-dlist-node object)))
          (case relative-position
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
                           tail dlist-node)))))

            (otherwise
             (error "CONTAINER-INSERT: Got an invalid value for RELATIVE-POSITION: ~A" relative-position)))

          (setf relative-position :after
                relative-object dlist-node))))))


(defmethod container-exchange ((event container-exchange) (dlist dlist))
  (let* ((object-a (target-position-of event))
         (object-b (object-of event))
         (a-value (value-of object-a)))
    (setf (value-of object-a) (value-of object-b)
          (value-of object-b) a-value)))
