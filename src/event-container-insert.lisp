;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defclass container-insert (container-event)
  ((relative-position :reader relative-position-of :initarg :relative-position
                      :type symbol
                      :initform nil)
   
   (relative-object :reader relative-object-of :initarg :relative-object
                    :type (or null model-base)
                    :initform nil))

  (:documentation "
This represents the various ways of inserting an object (or a set of objects?)
into some location(s?) in a container."))


(defmethod handle ((event container-insert))
  (container-insert event (container-of event)))


(defmethod insert (object &rest args &key
                   (before nil before-supplied-p)
                   (after nil after-supplied-p)
                   (in nil in-supplied-p))
  "If :IN is given OBJECT will be inserted at what is determined to be the most
suitable or natural position in IN."
  (assert (= 1 (count t (list before-supplied-p after-supplied-p in-supplied-p))))
  ;; TODO: Proper objects with class-hierarchy to specify 'positions'?
  (let ((result (cond
                  (before-supplied-p
                   (cons :before before))
                  
                  (after-supplied-p
                   (cons :after after))

                  (in-supplied-p
                   t)

                  (t
                   (error "INSERT was given incorrect arguments: ~A" args)))))
    
    (handle (if (eq t result)
                (make-instance 'container-insert
                               :object object
                               :container (container-of in))
                (make-instance 'container-insert
                               :object object
                               :container (container-of (cdr result))
                               :relative-position (car result)
                               :relative-object (cdr result))))))
