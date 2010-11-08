;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
(in-readtable sw-mvc)


(let* ((counter 0)
       (x λV2)
       (square λI(progn
                   (incf counter)
                   (* ~x ~x)))
       (str-input (with1 λI"hello"
                    (setf (equal-p-fn-of it) #'string=)))
       (str-upcase λI(progn
                         (incf counter)
                         (string-upcase ~str-input))))
  (setf ~x 2)
  (assert (= counter 2))
  (assert (= ~x 2))
  (assert (= ~square 4))
  (setf ~str-input (copy-seq "world"))
  (assert (= counter 3))
  (setf ~str-input (copy-seq "world"))
  (assert (= counter 3))
  ~str-upcase)
