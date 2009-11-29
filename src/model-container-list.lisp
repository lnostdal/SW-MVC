;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(eval-now (defclass dlist-node () () (:metaclass mvc-class)))

(defclass dlist-node (node single-value-model)
  ((left :accessor left-of :initarg :left
         :type (or null dlist-node)
         :initform nil)

   (right :accessor right-of :initarg :right
          :type (or null dlist-node)
          :initform nil)

   (value :accessor value-of :initarg :value
          :type model
          :initform ":VALUE needed."))

  (:metaclass mvc-class)
  (:documentation "
Doubly-linked list node with support for dataflow and transactions."))


(defmethod touch ((dlist-node dlist-node))
  (container-of dlist-node))


(defclass dlist (container event-router)
  ((head :accessor head-of :initarg :head
         :type (or null dlist-node)
         :initform nil)

   (tail :accessor tail-of
         :type (or null dlist-node)
         :initform nil))

  (:metaclass mvc-class)
  (:documentation "
Doubly-linked list with support for dataflow and transactions."))


(defmethod print-object ((dlist-node dlist-node) stream)
  (print-unreadable-object (dlist-node stream :type t :identity t)
    (when (slot-boundp dlist-node 'value)
      (prin1 (value-of dlist-node) stream))))


(add-deref-type 'dlist-node
                :get-expansion (λ (arg-sym) `(slot-value ,arg-sym 'value))
                :set-expansion t)


(defmethod empty-p-of ((dlist dlist))
  (null (head-of dlist)))


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


(add-deref-type 'dlist
                :get-expansion (λ (arg-sym) `(list<- ,arg-sym)))


(defmethod node-constructor ((container dlist) (model model))
  (make-instance 'dlist-node :container container :value model))


(flet ((remove-node (node dlist)
         (let ((left (left-of node))
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


  (defmethod container-remove ((event container-remove) (dlist dlist))
    (dolist (object (objects-of event))
      (remove-node (node-in-context-of (container-of event) object nil nil)
                   dlist)))



  (defmethod container-remove-all ((event container-remove-all) (dlist dlist))
    (dolist (node (list<- dlist))
      (remove-node node dlist))))


(defmethod container-insert ((event container-insert) (dlist dlist))
  (let ((relative-position (relative-position-of event))
        (relative-node (relative-node-of event)))
    (dolist (dlist-node (mapcar (λ (object) (node-in-context-of dlist object t))
                                (objects-of event)))
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
          (value-of object-b) a-value)))


(flet ((fill-dlist (dlist items)
         (when items
           (dolist (item items)
             (check-type item model))
           (let ((tail (setf (head-of dlist)
                             (node-in-context-of dlist (first items) t))))
             (dolist (item (rest items))
               (setf tail
                     (setf (right-of tail)
                           (with1 (node-in-context-of dlist item t)
                             (setf (left-of it) tail)))))
             (setf (tail-of dlist) tail)))))


  (defun dlist (&rest items)
    "Create an MODEL-CONTAINER-LIST instance (container data structure). If any item in ITEMS is not a sub-type of
MODEL it will be automatically wrapped in a CELL with \"value semantics\" (MK-VCELL)."
    (with1 (make-instance 'dlist)
      (fill-dlist it (mapcar (λ (i) (typecase i
                                      (model i)
                                      (t λVi)))
                             items))))


  (defmethod transform-into ((target dlist) (source list) &key
                             really-remove-fn really-exchange-fn)
    "Transform the TARGET container to match the values in SOURCE by initiating container events vs. TARGET.
REALLY-REMOVE-FN is passed a single argument; the Model to (maybe) remove. If one return T from this function the
Model will be removed, and if NIL is returned the remove operation will be skipped for that Model."
    (declare ((or function null) really-remove-fn really-exchange-fn))
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
        (when (or (not really-remove-fn)
                  (funcall really-remove-fn (cdr removed-element)))
          (deletef before removed-element :test #'eq)
          (remove (cdr removed-element) target)))

      ;; EXCHANGE.
      (let ((already-swapped nil))
        (map nil (lambda (before-elt after-elt)
                   (when (and (or (not really-exchange-fn)
                                  (and (funcall really-exchange-fn (cdr before-elt))
                                       (funcall really-exchange-fn (cdr after-elt))))
                              (not (eq (cdr before-elt) (cdr after-elt)))
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


  (defmethod transform-into ((target dlist) (source dlist) &rest args)
    (apply #'transform-into target ~~source args))) ;; MODEL* ← DLIST-NODE* ← DLIST
