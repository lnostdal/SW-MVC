;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

#|
What we can do to make instances of this persistent is to add an observer
via the MVC-CLASS mechanism. DB-CLASS fits in as classes of the contained_
objects. This is the key point and the answer I've been looking for:

  data structure --mvc--> db-class for storing structure

  elements _in_ data structure --may have a metaclass--> db-class for storing data elements
|#


(defclass pair (container event-router)
  ((left :accessor left-of :initarg :left
         :initform nil)

   (right :accessor right-of :initarg :right
          :initform nil))

  (:metaclass mvc-class))
(export '(pair left left-of right right-of))


(defmethod print-object ((pair pair) stream)
  (print-unreadable-object (pair stream :type t :identity t)
    (format stream ":LEFT ~S :RIGHT ~S" (left-of pair) (right-of pair))))


(defmethod deref ((pair pair))
  (cons (left-of pair) (right-of pair)))


(defun mk-pair (&optional left right)
  (make-instance 'pair :left left :right right))
(export 'mk-pair)
