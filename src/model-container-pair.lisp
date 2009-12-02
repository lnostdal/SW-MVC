;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
=common-headers=


(defclass pair (container event-router)
  ((left :reader left-of
         ;;:type model
         :initform nil)

   (right :reader right-of
          ;;:type model
          :initform nil))

  (:metaclass mvc-class))


(add-deref-type 'pair
                :get-expansion (λ (arg-sym) `(cons (left-of ,arg-sym)
                                                   (right-of ,arg-sym))))


(defmethod initialize-instance :after ((pair pair) &key left right)
  (setf (left-of pair) left)
  (setf (right-of pair) right))


(defmethod (setf left-of) ((model model) (pair pair))
  (setf (slot-value pair 'left) model))


(defmethod (setf right-of) ((model model) (pair pair))
  (setf (slot-value pair 'right) model))


(defmethod print-object ((pair pair) stream)
  (print-unreadable-object (pair stream :type t :identity t)
    (format stream ":LEFT ~S :RIGHT ~S" (left-of pair) (right-of pair))))


(defun mk-pair (&optional left right)
  (make-instance 'pair :left left :right right))
