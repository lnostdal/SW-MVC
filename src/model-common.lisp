;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defmethod (setf input-translator-of) (translator-fn model)
  (setf (metadata-of model 'input-translator)
        translator-fn))


(defmethod input-translator-of (model)
  (metadata-of model 'input-translator))


(defmethod (setf input-validator-of) ((formula formula) model)
  (setf (metadata-of model 'input-validator)
        #~formula))


(defmethod input-validator-of (model)
  (formula-of ~(metadata-of model 'input-validator)))


(defmacro feedback-event-of (model)
  `~(metadata-of ,model 'feedback-event #~nil))


(defmacro pulse (place &optional (value nil value-supplied-p))
  "Sets PLACE to T (or VALUE), then NIL.
Used for dataflow event pulses."
  `(prog1 ,(if value-supplied-p
               `(setf ,place ,value)
               `(tf ,place))
          (nilf ,place)))


(defun integer-input-translator (input)
  (typecase input
    (integer input)
    (string (parse-integer input))
    (t (error "INTEGER-INPUT-TRANSLATOR: Don't know what to do with ~S~%" input))))


(defun number-input-translator (input)
  (typecase input
    (number input)
    (string (parse-number input))
    (t (error "NUMBER-INPUT-TRANSLATOR: Don't know what to do with ~S~%" input))))