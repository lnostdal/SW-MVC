;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass slot-set-mvc ()
  ((slot-set-event :reader slot-set-event-of
                   :type (or null slot-set)
                   :initform nil))

  (:metaclass mvc-stm-class)
  (:documentation "
Classes who want enable their users to listen for \"any\" SLOT-SET event can
inherit from this class."))



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
    (format stream ":INSTANCE ~S :SLOT-NAME ~S :NEW-VALUE ~S"
            (instance-of slot-set)
            (slot-name-of slot-set)
            (new-value-of slot-set))
    (when (slot-boundp slot-set 'old-value)
      (format stream " :OLD-VALUE ~S" (slot-value slot-set 'old-value)))))


(defmethod handle ((event slot-set))
  ;; CLOS already sets the slot for us; we're only interested in the
  ;; side-effect of notifying any observers of what happened.
  (let ((instance (instance-of event)))
    (when (typep instance 'slot-set-mvc)
      (with-object instance
        (setf ¤slot-set-event event
              ¤slot-set-event nil)))

    ;; To callbacks that should be called for "any slot" of the object in question.
    (with-callbacks (instance :slot-name t)
      (funcall callback event))

    ;; To callbacks that should be called for only the specific slot in question.
    (with-callbacks (instance :slot-name (slot-name-of event))
      (funcall callback event))))
