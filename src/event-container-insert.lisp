;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container-insert (container-event)
  ((relative-position :reader relative-position-of :initarg :relative-position
                      :type symbol
                      :initform nil)

   (relative-node :reader relative-node-of :initarg :relative-node
                  :type (or null node)
                  :initform nil))

  (:documentation "
This represents the various ways of inserting an object (or a set of objects?)
into some location(s?) in a container."))


(defmethod handle ((event container-insert))
  (prog1 (container-insert event (container-of event))
    (dolist (observable (observables-of event))
      ;; Notify stuff observing the container and the objects being inserted.
      (when (typep observable 'event-router)
        (event-router-notify observable event)))))


(defun insert (object &rest args &key
               (before nil before-supplied-p)
               (after nil after-supplied-p)
               (in nil in-supplied-p))
  "If :IN is given OBJECT will be inserted at what is determined to be the most
suitable or natural position in IN."
  (declare (optimize speed))
  (assert (xor before-supplied-p after-supplied-p in-supplied-p) nil
          ":BEFORE, :AFTER or :IN is needed (only one of them). Got: ~S" args)
  (flet ((check-objects ()
           (let ((all-ok nil))
             (dolist (object (setf object (ensure-list object)))
               (etypecase object
                 (view-base
                  (assert in-supplied-p nil
                          "When inserting a VIEW-BASE object, the :IN argument must be supplied.")
                  (assert (typep in 'view-base) nil
                          "When inserting a VIEW-BASE object, the :IN argument must also be a VIEW-BASE object.")
                  (push λλ(setf (view-in-context-of in (model-of object)) object)
                        all-ok))
                 (model)))
             (mapc #'funcall all-ok))))

    ;; TODO: Proper objects with class-hierarchy to specify 'positions'?
    (handle (multiple-value-call #'make-instance 'container-insert
             :objects object
             (cond
              (before-supplied-p
               (check-type before node)
               (check-objects)
               (values :container (container-of before)
                       :relative-position :before
                       :relative-node before))

              (after-supplied-p
               (check-type after node)
               (check-objects)
               (values :container (container-of after)
                       :relative-position :after
                       :relative-node after))

              (in-supplied-p
               (check-type in (or multiple-value-model view-base))
               (check-objects)
               (values :container in)))))))
