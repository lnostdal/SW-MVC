;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)
(in-readtable sw-mvc)


(let* ((input λI"0")
       (output (mk-validator #'evenp (mk-number-parser input)))
       (confirm 0)
       (update-ui λI(progn
                      #|(format t "Update UI based on new valid OUTPUT: ~A~%" ~output)|#
                      (assert (= confirm ~output))
                      (assert (evenp confirm)))))
  (declare (ignorable update-ui))
  (handler-bind ((mvc-input-handler-signal (iambda
                                             (when (find-restart 'skip-cell)
                                               (invoke-restart 'skip-cell)))))

    (loop :for i :from 1 :upto 25
       :do (setf confirm i
                 ~input (format nil "~D" i)
                 ~input "junk")))
  ~output)



(let* ((x λI2)
       (valid-x (mk-validator #'evenp x)))

  (setf (on-feedback-event-fn-of x)
        (lambda (c)
          (declare (ignore c))
          (setf ~valid-x 42)))

  (handler-bind ((mvc-input-handler-signal
                  (lambda (c)
                    (invoke-restart (find-restart 'execute-feedback-event c)))))
    (setf ~x 3))

  (assert (= ~x 3))
  (assert (= ~valid-x 42)))