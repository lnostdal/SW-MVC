;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container-insert-mvc ()
  ((insert-event :reader insert-event-of
                 :cellp t
                 :initform nil))

  (:metaclass mvc-class))



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
  (let ((container (container-of event)))
    (container-insert event container)
    (dolist (observable (observables-of event))
      ;; Notify stuff observing the container and the objects being inserted.
      (when (typep observable 'container-insert-mvc)
        (with-object observable
          (pulse ¤insert-event event))))))


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

    (handle (apply #'make-instance 'container-insert
                   :objects object
                   `(,@(if (eq t result)
                           `(:container ,(container-of in))
                           `(:container ,(container-of (cdr result))
                             :relative-position ,(car result)
                             :relative-object ,(cdr result))))))))
