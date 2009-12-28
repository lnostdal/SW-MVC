;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
=common-headers=


(defun mk-input-handler (input-cell translator-fn &key equal-p-fn)
  "Returns a new CELL instance."
  (declare (cell input-cell)
           (function translator-fn))
  (letp1 ((cell λi(let ((input-value ~input-cell))
                    (restart-case
                        (handler-case
                            (unwind-protect-case ()
                                (funcall translator-fn input-value)
                              (:abort
                               (setf (feedback-event-of input-cell)
                                     ;; TODO: A condition object? Or something?
                                     (fmtn "MK-INPUT-HANDLER got ~S" input-value)))
                              (:normal
                               ;; TODO: What if the cell can contain multiple errors or "feedback entries"? *SIGH*
                               (nilf (feedback-event-of input-cell))))
                          (error (c)
                            (signal 'mvc-input-handler-signal
                                    :condition c
                                    :input-value input-value
                                    :format-control "SW-MVC: The INPUT-HANDLER for ~A failed."
                                    :format-arguments (list input-cell))
                            input-value))
                      (continue ()
                        :report "SW-MVC: (INPUT-HANDLER) assign new most likely invalid value."
                        input-value)
                      (skip-cell ()
                        :report "SW-MVC: (INPUT-HANDLER) keep old value."
                        =cell-old-value=)))))
    (when equal-p-fn
      (setf (equal-p-fn-of cell) equal-p-fn))))


(defun mk-validator (test-fn input-cell)
  (declare (function test-fn)
           (cell input-cell))
  (mk-input-handler input-cell
                    (lambda (input)
                      (let ((result (funcall test-fn input)))
                        (if result
                            input
                            (error "MK-VALIDATOR (lambda): Validation of input failed: ~S" input))))))


(defun mk-integer-parser (input-cell &key (max-input-length 50))
  "Returns a CELL that'll represent the result of sending the content of
INPUT-CELL through AMX:PARSE-INTEGER."
  (declare (cell input-cell)
           (fixnum max-input-length))
  (warn "TODO: Update MK-INTEGER-PARSER!")
  (mk-input-handler input-cell
                    (lambda (input)
                      (etypecase input
                        (string
                         (assert (> max-input-length (length input)) nil
                                 "MK-INTEGER-PARSER: Input too long to parse.")
                         (parse-integer input))

                        (integer input)))
                    :equal-p-fn #'=))


(defun mk-number-parser (input-cell &key (max-input-length 50))
  "Returns a CELL that'll represent the result of sending the content of
INPUT-CELL through AMX:PARSE-NUMBER."
  (declare (cell input-cell)
           (fixnum max-input-length))
  (mk-input-handler input-cell
                    (lambda (input)
                      (etypecase input
                        (string
                         (assert (> max-input-length (length input)) nil
                                 "MK-NUMBER-PARSER: Input too long to parse.")
                         (parse-number input))

                        (number input)))
                    :equal-p-fn #'=))


(defun add-input-handler (view mk-input-handler-fn)
  "It is important that this is called after the network of dependencies has
already been setup.

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
          (with1 λI~model
            (forward-cell (funcall mk-input-handler-fn it)
                          model)))))
