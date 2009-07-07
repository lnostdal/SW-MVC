;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(declaim (ftype (function (cell function &key
                                (:feedback-cell (or null cell))
                                (:equal-p-fn (or null function)))
                          (values cell &optional))
                mk-input-handler))
(defun mk-input-handler (input-cell translator-fn &key feedback-cell equal-p-fn)
  (letp1 ((cell λi(let ((input ~input-cell)
                        (old-value =cell-old-value=))
                    (handler-case
                        (funcall translator-fn input)
                      (error (c)
                        (if (and feedback-cell
                                 (cell-observedp feedback-cell))
                            (progn
                              (pulse ~feedback-cell (cons c input))
                              old-value)
                            (progn
                              (cerror "Return old value ~S and continue."
                                      (fmtn "MK-INPUT-HANDLER (lambda): Got condition ~S~%on input ~S" c input)
                                      old-value)
                              old-value)))))))
    (when equal-p-fn
      (setf (equal-p-fn-of cell) equal-p-fn))))



(declaim (ftype (function (function cell &optional cell) (values cell &optional))
                mk-validator))
(defun mk-validator (test-fn input-cell &optional feedback-cell)
  (mk-input-handler input-cell
                    (lambda (input)
                      (let ((result (funcall test-fn input)))
                        (if result
                            input
                            (error "MK-VALIDATOR (lambda): Validation of input failed: ~S" input))))
                    :feedback-cell feedback-cell))




(declaim (ftype (function (cell &optional cell) (values cell &optional))
                mk-integer-parser))
(defun mk-integer-parser (input-cell &optional feedback-cell)
  "Returns a CELL that'll represent the result of sending the content of
INPUT-CELL through AMX:PARSE-INTEGER."
  (mk-input-handler input-cell
                    (lambda (input)
                      (typecase input
                        (string (parse-integer input))
                        (integer input)
                        (t (error "MK-INTEGER-PARSER (lambda): Don't know what to do with input: ~S~%" input))))
                    :equal-p-fn #'=
                    :feedback-cell feedback-cell))
(export 'mk-integer-parser)


(declaim (ftype (function (cell &optional cell) (values cell &optional))
                mk-number-parser))
(defun mk-number-parser (input-cell &optional feedback-cell)
  "Returns a CELL that'll represent the result of sending the content of
INPUT-CELL through AMX:PARSE-NUMBER."
  (mk-input-handler input-cell
                    (lambda (input)
                      (typecase input
                        (string (parse-number input))
                        (number input)
                        (t (error "MK-NUMBER-PARSER (lambda): Don't know what to do with input: ~S~%" input))))
                    :equal-p-fn #'=
                    :feedback-cell feedback-cell))
(export 'mk-number-parser)
