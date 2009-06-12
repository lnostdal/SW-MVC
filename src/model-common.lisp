;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)



(defmethod (setf translator-of) (translator-fn model)
  (setf (metadata-of model 'translator)
        translator-fn))


(defmethod translator-of (model)
  (metadata-of model 'translator))


(defmethod (setf validator-of) ((formula formula) model)
  (setf (metadata-of model 'validator) #~formula))


(defmethod validator-of (model)
  (formula-of ~(metadata-of model 'validator)))


(defmacro feedback-event-of (model)
  `~(metadata-of ,model 'feedback-event #~nil))


(defmacro pulse (place &optional (value nil value-supplied-p))
  "Sets PLACE to T, then NIL.
Used for dataflow event pulses."
  `(progn ,(if value-supplied-p
               `(setf ,place ,value)
               `(tf ,place))
          (nilf ,place)))
(export 'pulse)
