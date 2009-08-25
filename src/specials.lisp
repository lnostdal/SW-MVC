;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


;;; CELL ;;;
;;;;;;;;;;;;

(define-variable *get-cell-p*
    :value nil
    :type (member t nil)
    :always-boundp t)

(define-variable *source-cells*
    :value nil
    :type list
    :always-boundp t)

(define-variable *target-cell*
    :value nil
    :type (or null cell)
    :always-boundp t)
