;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(eval-now
(defclass dlist-node (node single-value-model)
  ((left :accessor left-of :initarg :left
         ;;:type (or null dlist-node)
         :initform nil)

   (right :accessor right-of :initarg :right
          ;;:type (or null dlist-node)
          :initform nil)

   (value :accessor value-of :initarg :value
          ;;:type model
          :initform ":VALUE needed."))

  (:metaclass mvc-class)
  (:documentation "
Doubly-linked list node with support for dataflow and transactions."))



(defclass dlist (container event-router)
  ((head :accessor head-of :initarg :head
         ;;:type (or null dlist-node)
         :initform nil)

   (tail :accessor tail-of
         ;;:type (or null dlist-node)
         :initform nil))

  (:default-initargs :key-fn (λ (obj) (value-of obj)))
  (:metaclass mvc-class)
  (:documentation "
Doubly-linked list with support for dataflow and transactions."))
)


(defmethod print-object ((dlist-node dlist-node) stream)
  (print-unreadable-object (dlist-node stream :type t :identity t)
    (when (slot-boundp dlist-node 'value)
      (prin1 (value-of dlist-node) stream))))


(defmethod deref ((dlist-node dlist-node))
  (slot-value dlist-node 'value))


(defmethod (setf deref) (new-value (dlist-node dlist-node))
  (setf (slot-value dlist-node 'value) new-value))


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
           (dolist (item items)
             (check-type item model))
           (let ((tail (setf (head-of dlist)
                             (make-instance 'dlist-node
                                            :container dlist
                                            :value (first items)))))
             (dolist (item (rest items))
               (setf tail
                     (setf (right-of tail)
                           (make-instance 'dlist-node
                                          :container dlist
                                          :left tail
                                          :value item))))
             (setf (tail-of dlist) tail)))))


  (defun dlist (&rest items)
    (declare (dynamic-extent items))
    (with1 (make-instance 'dlist)
      (fill-dlist it items)))


  (defmethod transform-into ((target dlist) (source list))
    "Transform the TARGET container to match the values in SOURCE by initiating container events vs. TARGET."
    (let ((before (mapcar (λ (node) (cons node ~node)) ~target)) ;; (DLIST-NODE . MODEL)* ← DLIST
          (after nil) ;; (DLIST-NODE . MODEL)*
          (to-insert nil)) ;; MODEL*
      (declare (list before after to-insert))

      ;; TARGET is empty.
      #|(unless before
        (when source
          (fill-dlist target source))
        ;; TODO: Hm. We're returning without triggering any events.
        (return-from transform-into))|#

      (dolist (source-model source (nreversef after))
        (if-let (node.model (find source-model before :test #'eq :key #'cdr))
          (push node.model after)
          (push source-model to-insert)))

      ;; REMOVE.
      (dolist (removed-element (set-difference before after :test #'eq :key #'cdr))
        (deletef before removed-element :test #'eq)
        (remove (cdr removed-element) target))

      ;; EXCHANGE.
      (let ((already-swapped nil))
        (map nil (lambda (before-elt after-elt)
                   (when (and (not (eq (cdr before-elt) (cdr after-elt)))
                              (not (find after-elt already-swapped :test #'eq)))
                     (push before-elt already-swapped)
                     (exchange (car before-elt) (car after-elt))))
             before
             after))

      ;; INSERT.
      (if before
          (let ((last-node (car (last1 before))))
            (dolist (model to-insert)
              (if-let ((right-model (cadr (member model source :test #'eq))))
                (insert model :before (node-in-context-of target right-model))
                (insert model :after last-node))))
          (when to-insert
            (insert (nreversef to-insert) :in target)))))


  (defmethod transform-into ((target dlist) (source dlist))
    (transform-into target ~~source))) ;; MODEL* ← DLIST-NODE* ← DLIST


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
        (relative-node (relative-node-of event)))
    (dolist (dlist-node (mapcar (λ (object) (make-instance 'dlist-node :container dlist :value object))
                                (objects-of event))
                        (objects-of event))
      (ecase relative-position
        (:before
         (with-slots (head tail) dlist
           (with-slots (left right) dlist-node
             (setf left (left-of relative-node)
                   right relative-node)
             (if (left-of relative-node)
                 (setf (right-of (left-of relative-node)) dlist-node
                       (left-of relative-node) dlist-node)
                 (setf head dlist-node
                       (left-of relative-node) dlist-node)))))

        (:after
         (with-slots (head tail) dlist
           (with-slots (left right) dlist-node
             (setf left relative-node
                   right (right-of relative-node))
             (if (right-of relative-node)
                 (setf (left-of (right-of relative-node)) dlist-node
                       (right-of relative-node) dlist-node)
                 (setf tail dlist-node
                       (right-of relative-node) dlist-node)))))

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
            relative-node dlist-node))))


(defmethod container-exchange ((event container-exchange) (dlist dlist))
  (let* ((object-a (target-position-of event))
         (object-b (object-of event))
         (a-value (value-of object-a)))
    (setf (value-of object-a) (value-of object-b)
          (value-of object-b) a-value)
    (list object-a object-b)))
