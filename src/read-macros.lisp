;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


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


;; Wraps an ICELL in a "pointer" for use when initializing a MVC-CLASS class' slots.
(eval-now
  (set-dispatch-macro-character #\λ #\f
                                (lambda (stream char arg)
                                  (declare (ignore char arg))
                                  `#&(mk-icell ,(read stream)))))