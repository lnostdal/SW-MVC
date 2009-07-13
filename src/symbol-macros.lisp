;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(define-symbol-macro =cell=
    *target-cell*)

(define-symbol-macro =cell-old-value=
    (value-of *target-cell*))


(define-symbol-macro =cell-value=
    ~*target-cell*)


(define-symbol-macro =cell-boundp=
    (init-eval-p-of *target-cell*))
