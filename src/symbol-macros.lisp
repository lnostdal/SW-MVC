;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(define-symbol-macro =cell=
    *target-cell*)

(define-symbol-macro =cell-old-value=
    (value-of *target-cell*))


(define-symbol-macro =cell-value=
    ~*target-cell*)


(define-symbol-macro =cell-boundp=
    (init-evalp-of *target-cell*))
