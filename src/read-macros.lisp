;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


;; Shortcut for CL:LAMBDA.
(eval-now
  (set-dispatch-macro-character #\λ #\λ
                                (lambda (stream char arg)
                                  (declare (ignore char arg))
                                  `(lambda () ,(read stream)))))



;; Value.
(eval-now
  (set-dispatch-macro-character #\# #\~
                                (lambda (stream char arg)
                                  (declare (ignore char arg))
                                  `(make-instance 'cell
                                                  :formula λλ,(read stream)
                                                  :input-evalp nil :output-evalp nil
                                                  :init-evalp t))))


;; Formula,
(eval-now
  (set-dispatch-macro-character #\# #\λ
                                (lambda (stream char arg)
                                  (declare (ignore char arg))
                                  `(make-instance 'cell
                                                  :formula λλ,(read stream)
                                                  :input-evalp t :output-evalp nil))))


;; Lambda or lazy-eval type semantics.
(eval-now
  (set-dispatch-macro-character #\# #\l
                                (lambda (stream char arg)
                                  (declare (ignore char arg))
                                  `(make-instance 'cell
                                                  :formula λλ,(read stream)
                                                  :input-evalp nil :output-evalp t))))



;; Value.
(eval-now
  (set-dispatch-macro-character #\λ #\v
                                (lambda (stream char arg)
                                  (declare (ignore char arg))
                                  `#~,(read stream))))


;; Formula, depending on inputs.
(eval-now
  (set-dispatch-macro-character #\λ #\i
                                (lambda (stream char arg)
                                  (declare (ignore char arg))
                                  `#λ,(read stream))))


;; Lambda or laze-eval type semantics.
(eval-now
  (set-dispatch-macro-character #\λ #\o
                                (lambda (stream char arg)
                                  (declare (ignore char arg))
                                  `(make-instance 'cell
                                                  :formula λλ,(read stream)
                                                  :input-evalp nil :output-evalp t))))


;; Lambda type semantics, but cached; only evaled once.
(eval-now
  (set-dispatch-macro-character #\λ #\c
                                (lambda (stream char arg)
                                  (declare (ignore char arg))
                                  `(make-instance 'cell
                                                  :formula λλ,(read stream)
                                                  :input-evalp nil
                                                  :output-evalp :cached))))
