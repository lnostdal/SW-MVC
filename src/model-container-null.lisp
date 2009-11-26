;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass null-container (container)
  ()

  (:metaclass mvc-class)
  (:documentation "
All container operations/events vs. an instance of this container Model will have no effect by default."))


(defmethod container-insert ((event container-insert) (container null-container))
  )


(defmethod container-remove ((event container-remove) (container null-container))
  )


(defmethod container-exchange ((event container-exchange) (container null-container))
  )
