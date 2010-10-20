;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
=common-headers=


(defun mk-input-handler (input-cell translator-fn &key equal-p-fn)
  "Returns a new CELL instance."
  (declare (cell input-cell)
           (function translator-fn))
  (letp1 ((cell λI(let ((input-value ~input-cell))
                    (restart-case
                        (unwind-protect-case ()
                            (funcall translator-fn input-value)
                          (:normal
                           (when-let (it (on-feedback-event-fn-of input-cell))
                             (funcall it nil))))
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
  "TEST-FN: A function taking one argument, input, and returning T when input was valid or NIL when input was invalid.

INPUT-CELL: A CELL instance which must(?) contain an initially valid value.

Returns a new CELL which will only hold the valid values sent to INPUT-CELL.

To get notifications of and handle invalid input, an \"observer cell\" can be created which watches (FEEDBACK-EVENT-OF INPUT-CELL).
Within the handler, only the value of the CELL returned by MK-VALIDATOR can be changed; INPUT-CELL stays as it is."
  (mk-input-handler input-cell
                    (lambda (input)
                      (let ((result (funcall test-fn input)))
                        (if result
                            input
                            (error 'mvc-input-handler-signal
                                   :input-value input
                                   :cell input-cell
                                   :format-control "MK-VALIDATOR: Validation of input, ~S, using ~A failed."
                                   :format-arguments (list input test-fn)))))))


(defun mk-number-parser (input-cell &key (max-input-length 50))
  "Returns a new CELL that'll represent the result of sending the content of INPUT-CELL through AMX:PARSE-NUMBER.

To get notifications of and handle invalid input, an \"observer cell\" can be created which watches (FEEDBACK-EVENT-OF INPUT-CELL)."
  (declare (cell input-cell)
           (fixnum max-input-length))
  (mk-input-handler input-cell
                    (lambda (input)
                      (etypecase input
                        (string
                         (unless (> max-input-length (length input))
                           (error 'mvc-input-handler-signal
                                  :input-value input
                                  :cell input-cell
                                  :format-control "MK-NUMBER-PARSER: Input too long to parse:~%~S~%"
                                  :format-arguments (list input)))
                         (handler-case (parse-number input)
                           ((or invalid-number parse-error) (c)
                             (error 'mvc-input-handler-signal
                                    :input-value input
                                    :cell input-cell
                                    :condition c
                                    :format-control "MK-NUMBER-PARSER: Parsing of number ~S failed."
                                    :format-arguments (list input)))))

                        (number input)))
                    :equal-p-fn #'=))


(defun add-input-handler (view mk-input-handler-fn)
  "It is important that this is called after the network of dependencies has been setup.

Before:

  MODEL <-> VIEW
    ^
    |
    v
   xxx


After:

    ---------------------
    |                   |
    v                   |
  MODEL -> IT -> INPUT-TRANSLATOR
    ^       ^
    |       |
    |       |
    v       v
   xxx     VIEW


MODEL is extracted from VIEW via the MODEL-OF method, and IT is an internally constructed CELL instance. IT is also the return value of this function.

xxx is targets of MODEL; what VIEW previously \"talked to directly\".

An example of this in use:

SW> (let* ((x-view (text-input (:model λI0)))
           (square (with ~x-view λI(* ~it ~it)))) ;; Note how SQUARE must always refer to the old model of X-VIEW.
      (add-input-handler x-view #'mk-number-parser)
      (setf ~~x-view \"2\")
      (dbg-prin1 (list ~~x-view ~square)))
\(LIST (DEREF (DEREF X-VIEW)) (DEREF SQUARE)) => (\"2\" 4)"
  (declare (view-base view)
           (function mk-input-handler-fn))
  (let ((old-model (model-of view)))
    (setf (model-of view)
          (with1 λI~old-model
            (forward-cell (funcall mk-input-handler-fn it)
                          old-model)))))