;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(define-symbol-macro =cell-old-value=
    (value-of *target-cell*))


(define-symbol-macro =cell-value=
    ~*target-cell*)


(define-symbol-macro =cell-boundp=
    (init-eval-p-of *target-cell*))





;; Note that the MK-FORMULA macro actually lexically shadows =EVENT=.
;; Also note that when a FORMULA is being constructed or initialized,
;; this will refer to that FORMULA instance; i.e., not an event!
(define-symbol-macro =event=
    (car *event-stack*))


(define-symbol-macro =slot-set-event-p=
    (typep =event= 'slot-set))


;; User code should check if =SLOT-SET-EVENT-P= is T before using these.

(define-symbol-macro =old-value=
    (slot-value =event= 'old-value))

(define-symbol-macro =new-value=
    (slot-value =event= 'new-value))


(define-symbol-macro =event-model=
    (model-of =event=))
