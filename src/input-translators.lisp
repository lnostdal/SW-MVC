;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(declaim (ftype (function (cell function &key
                                (:feedback-cell t)
                                (:equal-p-fn (or null function)))
                          (values cell &optional))
                mk-input-handler))
(defun mk-input-handler (input-cell translator-fn &key feedback-cell equal-p-fn)
  (when (eq t feedback-cell)
    (setf feedback-cell (feedback-event-of input-cell)))
  (letp1 ((cell λi(let ((input ~input-cell)
                        (old-value =cell-old-value=))
                    (handler-case
                        (prog1 (funcall translator-fn input)
                          (when (and feedback-cell ~feedback-cell)
                            (nilf ~feedback-cell)))
                      (error (c)
                        (prog1 old-value
                          (if (and feedback-cell
                                   (cell-observedp feedback-cell))
                              (setf ~feedback-cell (cons c input))
                              (cerror "Return old value ~S and continue."
                                      (fmtn "MK-INPUT-HANDLER (lambda): Got condition ~S~%on input ~S" c input)
                                      old-value))))))))
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


(declaim (ftype (function (cell &key (:feedback-cell t) (:max-input-length fixnum))
                          (values cell &optional))
                mk-integer-parser))
(defun mk-integer-parser (input-cell &key (feedback-cell t) (max-input-length 50))
  "Returns a CELL that'll represent the result of sending the content of
INPUT-CELL through AMX:PARSE-INTEGER."
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


(declaim (ftype (function (cell &key (:feedback-cell t) (:max-input-length fixnum))
                          (values cell &optional))
                mk-number-parser))
(defun mk-number-parser (input-cell &key (feedback-cell t) (max-input-length 50))
  "Returns a CELL that'll represent the result of sending the content of
INPUT-CELL through AMX:PARSE-NUMBER."
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
