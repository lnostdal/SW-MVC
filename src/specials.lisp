;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


;;; CELL ;;;
;;;;;;;;;;;;

(define-variable *get-cell-p*
    :value nil
    :type (member t nil))


(define-variable *source-cells*
    :value nil
    :type list)


(define-variable *target-cell*
    :value nil
    :type (or null cell))
