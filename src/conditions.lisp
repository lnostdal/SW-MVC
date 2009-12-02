;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
=common-headers=


(define-condition mvc-condition ()
  ((condition :reader condition-of :initarg :condition
              :initform nil #|(error ":CONDITION needed.")|#)))


(defmethod print-object ((obj mvc-condition) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (when (slot-boundp obj 'condition)
      (muffle-compiler-note
        (format stream " :CONDITION ~S" (slot-value obj 'condition))))))


(define-condition mvc-error (mvc-condition simple-error)
  ()

  (:documentation "
These are not safe to ignore when in a production environment; go for the ABORT-TRANSACTION restart."))


(define-condition mvc-cell-error (mvc-error)
  ((cell :initarg :cell
         :initform nil #|(error ":CELL needed.")|#)))


(define-condition mvc-signal (mvc-condition simple-condition)
  ()

  (:documentation "
This is used to wrap what would normally be considered an error in a signal that can be ignored (default) or handled
in higher-level code."))


(define-condition mvc-cell-signal (mvc-signal)
  ((cell :initarg :cell
         :initform nil)))


(define-condition mvc-input-handler-signal (mvc-cell-signal)
  ((input-value :initarg :input-value)))


(define-condition mvc-cell-assign-signal (mvc-cell-signal)
  ((new-value :initarg :new-value)))
