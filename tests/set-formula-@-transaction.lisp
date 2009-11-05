;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)


(let* ((i λV2)
       (x λI(+ 40 ~i)))
  (catch :abort
    (with-sync ()
      (setf (formula-of x) (lambda () 1))
      (throw :abort nil)))

  (incf ~i) ;; The transaction was aborted, so we're back to the old formula.
  (assert (= 43 ~x)))