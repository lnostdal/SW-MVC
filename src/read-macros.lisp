;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defreadtable sw-mvc
  (:merge sw-stm)

  ;; Value.
  (:dispatch-macro-char #\# #\~
                        #'(lambda (stream char arg)
                            (declare (ignore char arg))
                            `(mk-vcell ,(read stream))))

  ;; Formula.
  (:dispatch-macro-char #\# #\λ
                        #'(lambda (stream char arg)
                            (declare (ignore char arg))
                            `(mk-icell ,(read stream))))

  ;; Lambda or lazy-eval type semantics.
  (:dispatch-macro-char #\# #\l
                        #'(lambda (stream char arg)
                            (declare (ignore char arg))
                            `(mk-ocell ,(read stream))))


  ;; Value.
  (:dispatch-macro-char #\λ #\v
                        #'(lambda (stream char arg)
                            (declare (ignore char arg))
                            `(mk-vcell ,(read stream))))

  ;; Formula, depending on inputs.
  (:dispatch-macro-char #\λ #\i
                        #'(lambda (stream char arg)
                            (declare (ignore char arg))
                            `(mk-icell ,(read stream))))

  ;; Lambda or lazy-eval type semantics.
  (:dispatch-macro-char #\λ #\o
                        #'(lambda (stream char arg)
                            (declare (ignore char arg))
                            `(mk-ocell ,(read stream))))

  ;; Lambda type semantics, but cached; only evaled once.
  (:dispatch-macro-char #\λ #\c
                        #'(lambda (stream char arg)
                            (declare (ignore char arg))
                            `(mk-ccell ,(read stream))))

  ;; Wraps an ICELL in a "pointer" for use when initializing a MVC-CLASS class' slots.
  (:dispatch-macro-char #\λ #\f
                        #'(lambda (stream char arg)
                            (declare (ignore char arg))
                            `(mk-ptr (mk-icell ,(read stream)))))
  )
(export 'sw-mvc)
