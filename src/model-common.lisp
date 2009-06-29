;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defmacro pulse (place &optional (value nil value-supplied-p))
  "Sets PLACE to T (or VALUE), then NIL.
Used for dataflow event pulses."
  `(prog1 ,(if value-supplied-p
               `(setf ,place ,value)
               `(tf ,place))
          (nilf ,place)))


(defmacro feedback-event-of (model)
  `~(metadata-of ,model 'feedback-event #~nil))


(defmethod (setf input-translator-of) (translator-fn model)
  (setf (metadata-of model 'input-translator)
        (lambda (input)
          (handler-case
              (unwind-protect-case ()
                  (funcall translator-fn input)
                (:abort
                 (unless (eq t (feedback-event-of model))
                   (tf (feedback-event-of model))))
                (:normal
                 (unless (eq nil (feedback-event-of model))
                   (nilf (feedback-event-of model)))))
            (error (c)
              (if-let (restart (find-restart 'input-translator-restart))
                (invoke-restart restart model c)
                ~model))))))


(defmethod input-translator-of (model)
  (or (metadata-of model 'input-translator)
      #'identity))


(defmethod translate-input (model input-value)
  (funcall (input-translator-of model) input-value))


(defmethod (setf input-validator-of) ((formula formula) model)
  (setf (metadata-of model 'input-validator)
        #~formula))


(defmethod input-validator-of (model)
  (formula-of ~(metadata-of model 'input-validator)))


(defmethod model-equal-p ((model single-value-model) input-value)
  (and (slot-boundp model 'value)
       (equal (slot-value model 'value)
              input-value)))


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
