;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container-insert (container-event)
  ((relative-position :reader relative-position-of :initarg :relative-position
                      :type symbol
                      :initform nil)

   (relative-object :reader relative-object-of :initarg :relative-object
                    :initform nil))

  (:documentation "
This represents the various ways of inserting an object (or a set of objects?)
into some location(s?) in a container."))


(defmethod handle ((event container-insert))
  (let ((container (container-of (container-of event)))) ;; (or VIEW-BASE  -> MODEL (Sw-MVC:CONTAINER)
    (prog1 (container-insert event container)            ;;     DLIST-NODE -> DLIST)
      (dolist (observable (observables-of event))
        ;; Notify stuff observing the container and the objects being inserted.
        (when (typep observable 'event-router)
          (event-router-notify observable event))))))


(defmethod insert (object &rest args &key
                   (before nil before-supplied-p)
                   (after nil after-supplied-p)
                   (in nil in-supplied-p))
  "If :IN is given OBJECT will be inserted at what is determined to be the most
suitable or natural position in IN."
  (assert (= 1 (count t (list before-supplied-p after-supplied-p in-supplied-p))) nil
          ":BEFORE, :AFTER or :IN is needed (only one of them), got: ~S" args)
  ;; TODO: Proper objects with class-hierarchy to specify 'positions'?
  (handle (multiple-value-call #'make-instance
            'container-insert
            :objects object
            (cond
              (before-supplied-p
               (values :container before
                       :relative-position :before
                       :relative-object before))
              (after-supplied-p
               (values :container after
                       :relative-position :after
                       :relative-object after))
              (in-supplied-p
               (values :container in))))))
