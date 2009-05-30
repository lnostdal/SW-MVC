;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


;; Creates a CELL and places the value given in that cell.
;; The ~ read macro extracts the value stored in the cell.
(set-dispatch-macro-character #\# #\~
                              (lambda (stream char arg)
                                (declare (ignore char arg))
                                `(mk-cell ,(read stream))))


;; Creates a CELL instance with a FORMULA as value.
;; The ~ read macro extracts the computed result of the FORMULA.
(set-dispatch-macro-character #\# #\λ
                              (lambda (stream char arg)
                                (declare (ignore char))
                                `(mk-fcell (,@(when arg
                                               `(:concurrency ,arg)))
                                   ,(read stream))))


;; Creates a CELL instance with a FORMULA as value. The formula is \"static\".
;; The ~ read macro extracts the computed result of the FORMULA.
(set-dispatch-macro-character #\# #\@
                              (lambda (stream char arg)
                                (declare (ignore char))
                                `(mk-fcell (,@(when arg
                                               `(:concurrency ,arg)))
                                   ,(read stream))))


;; Creates a FORMULA instance.
(set-macro-character #\λ
                     (lambda (stream char)
                       (declare (ignore char))
                       (let* ((first (read stream))
                              (concurrency (when (integerp first) first))
                              (rest (when concurrency (read stream nil nil)))
                              (body (if concurrency
                                     `(,rest)
                                     `(,first ,@rest))))
                         `(mk-formula (,@(when concurrency
                                          `(:concurrency ,concurrency)))
                            ,@body)))
                     t)
