;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass slot-set (event)
  ((instance :reader instance-of :initarg :instance
             :initform (error ":INSTANCE needed."))

   (slot-name :reader slot-name-of :initarg :slot-name
              :initform (error ":SLOT-NAME needed."))

   (old-value :reader old-value-of :initarg :old-value)

   (new-value :reader new-value-of :initarg :new-value
              :initform (error ":NEW-VALUE needed."))))


(defmethod print-object ((slot-set slot-set) stream)
  (print-unreadable-object (slot-set stream :type t :identity t)
    (format stream ":INSTANCE ~A :SLOT-NAME ~A :NEW-VALUE ~A"
            (instance-of slot-set)
            (slot-name-of slot-set)
            (new-value-of slot-set))))


(defmethod handle :around ((event slot-set))
  (if (typep (instance-of event) 'view-base)
      (when-commit () (call-next-method))
      (call-next-method)))


(defmethod handle ((event slot-set))
  ;; NOTE: CLOS already sets the slot for us; we're only interested in the side-effect of notifying
  ;; any observers of what happened.
  )


(defmethod observables-of append ((event slot-set))
  (list (instance-of event)))


(defmethod propagate ((event slot-set))
  (dolist (observable (observables-of event))

    ;; To callbacks that should be called for "any slot" of the object in question.
    (with-callbacks (observable :slot-name t)
      (if (typep observer 'view-base) ;; TODO: This could be a method (source target event).
          (when-commit () (funcall callback event))
          (funcall callback event)))
    
    ;; To callbacks that should be called for only the specific slot in question.
    (with-callbacks (observable :slot-name (slot-name-of event))
      (if (typep observer 'view-base) ;; TODO: This could be a method (source target event).
          (when-commit () (funcall callback event))
          (funcall callback event)))))
  
