;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defclass container-move (container-event)
  ((from)
   (to))

  (:documentation "
This represents the various ways of moving an OBJECT. This does not replace
anything at its destination."))


(defmethod handle ((event container-move))
  (prog1 (container-move event
                         (container-of event)
                         (from-of event)
                         (to-of event))))


(defmethod move (object from to)
  (handle (make-instance 'container-move
                         :container (container-of object)
                         :from from
                         :to to)))
