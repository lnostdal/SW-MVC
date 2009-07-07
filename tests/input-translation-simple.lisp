;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(let* ((input #~"0")
       (output (mk-validator #'evenp (mk-number-parser input)))
       (confirm 0)
       (update-ui #λ(progn
                      #|(format t "Update UI based on new OUTPUT: ~A~%" ~output)|#
                      (assert (and (= confirm ~output)
                                   (evenp confirm))))))
  (declare (ignorable update-ui))
  (always-continue
    (loop :for i :from 1 :upto 25
       :do (setf confirm i
                 ~input (format nil "~D" i)
                 ~input "junk")))
  ~output)


(let* ((input #~"1")
       (output (mk-validator #'oddp (mk-number-parser input)))
       (confirm 1)
       (update-ui #λ(progn
                      #|(format t "Update UI based on new OUTPUT: ~A~%" ~output)|#
                      (assert (and (= confirm ~output)
                                   (oddp confirm))))))
  (declare (ignorable update-ui))
  (always-continue
    (loop :for i :from 1 :upto 25
       :do (setf confirm i
                 ~input (format nil "~D" i)
                 ~input "junk")))
  ~output)
