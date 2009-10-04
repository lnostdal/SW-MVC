;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defmacro pulse (place &optional (value nil value-supplied-p))
  "Sets PLACE to T (or VALUE if supplied), then NIL.
Used for dataflow \"event pulses\"."
  `(prog1 ,(if value-supplied-p
               `(setf ,place ,value)
               `(tf ,place))
          (nilf ,place)))


(defun feedback-event-of (model)
  (declare (type model model)) ;; TODO: Type doesn't exist yet!
  ~(metadata-of model 'feedback-event #~nil))


(defun (setf feedback-event-of) (arg model)
  (declare (type model model)) ;; TODO: Type doesn't exist yet!
  ;; TODO: Before changing this, get rid of the setf expansion macro for METADATA-OF.
  (let ((cell (metadata-of model 'feedback-event #~nil)))
    (setf ~cell arg)))
