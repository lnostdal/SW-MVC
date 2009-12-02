;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)


(eval-now
  (shadow '=common-headers=))

(eval-now
  (define-symbol-macro =common-headers=
      (progn
        (in-readtable sw-mvc)
        (declaim #.(optimizations)))))