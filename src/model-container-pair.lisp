;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defclass pair (container event-router)
  ((left :accessor left-of
         :initform nil)

   (right :accessor right-of
          :initform nil))

  (:metaclass mvc-class))
(export '(pair left left-of right right-of))


(defmethod initialize-instance :after ((pair pair) &key left right)
  (when left
    (setf (left-of pair) (model-of left)))
  (when right
    (setf (right-of pair) (model-of right))))


(defmethod print-object ((pair pair) stream)
  (print-unreadable-object (pair stream :type t :identity t)
    (format stream ":LEFT ~S :RIGHT ~S" (left-of pair) (right-of pair))))


(defmethod deref ((pair pair))
  (cons (left-of pair) (right-of pair)))


(defun mk-pair (&optional left right)
  (make-instance 'pair :left left :right right))
(export 'mk-pair)
