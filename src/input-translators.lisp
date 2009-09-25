;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defun mk-input-handler (input-cell translator-fn &key feedback-cell equal-p-fn)
  (declare (cell input-cell)
           (function translator-fn))
  (when (eq t feedback-cell)
    (setf feedback-cell (feedback-event-of input-cell)))
  (letp1 ((cell λi(funcall translator-fn ~input-cell)))
    (when equal-p-fn
      (setf (equal-p-fn-of cell) equal-p-fn))))


(defun mk-validator (test-fn input-cell &optional feedback-cell)
  (declare (function test-fn)
           (cell input-cell)
           ((or null cell) feedback-cell))
  (mk-input-handler input-cell
                    (lambda (input)
                      (let ((result (funcall test-fn input)))
                        (if result
                            input
                            (error "MK-VALIDATOR (lambda): Validation of input failed: ~S" input))))
                    :feedback-cell feedback-cell))


(defun mk-integer-parser (input-cell &key (feedback-cell t) (max-input-length 50))
  "Returns a CELL that'll represent the result of sending the content of
INPUT-CELL through AMX:PARSE-INTEGER."
  (declare (cell input-cell)
           (fixnum max-input-length))
  (mk-input-handler input-cell
                    (lambda (input)
                      (typecase input
                        (string
                         (assert (> max-input-length (length input)) nil
                                 "MK-INTEGER-PARSER: Input too long to parse.")
                         (parse-integer input))
                        (integer input)
                        (t (error "MK-INTEGER-PARSER (lambda): Don't know what to do with input: ~S~%" input))))
                    :equal-p-fn #'=
                    :feedback-cell feedback-cell))


(defun mk-number-parser (input-cell &key (feedback-cell t) (max-input-length 50))
  "Returns a CELL that'll represent the result of sending the content of
INPUT-CELL through AMX:PARSE-NUMBER."
  (declare (cell input-cell)
           (fixnum max-input-length))
  (mk-input-handler input-cell
                    (lambda (input)
                      (typecase input
                        (string
                         (assert (> max-input-length (length input)) nil
                                 "MK-NUMBER-PARSER: Input too long to parse.")
                         (parse-number input))
                        (number input)
                        (t (error "MK-NUMBER-PARSER (lambda): Don't know what to do with input: ~S~%" input))))
                    :equal-p-fn #'=
                    :feedback-cell feedback-cell))


(defun add-input-handler (view mk-input-handler-fn)
  "It is important that this is called after the network of dependencies on \"the
model end\" has already been setup.

Before:

  MODEL -> VIEW


After:

    ---------------------
    |                   |
    v                   |
  MODEL -> IT -> INPUT-TRANSLATOR
           ^
           |
           v
          VIEW


MODEL is extracted from VIEW via the MODEL-OF method, and IT is an internally
constructed CELL instance."
  (declare (view-base view)
           (function mk-input-handler-fn))
  (let ((model (model-of view)))
    (setf (model-of view)
          (with1 #λ~model
            (forward-cell (funcall mk-input-handler-fn it)
                          model)))))
