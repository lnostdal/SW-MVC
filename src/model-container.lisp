;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass container (multiple-value-model)
  ((test-fn :accessor test-fn-of :initarg :test-fn
            ;;:type function
            :initform #'eql)

   (key-fn :accessor key-fn-of :initarg :key-fn
           ;;:type function
           :initform #'identity))

  (:metaclass mvc-class))
