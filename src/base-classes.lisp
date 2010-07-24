;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass model ()
  ())

(defgeneric handle-event (event model-or-observer)
  )


(defclass observer ()
  ((model :reader model-of :initarg :model
          :type model)))



(defclass view (observer)
  ())



(defclass event ()
  ())

(defclass container-event (event)
  ())

(defgeneric handle-container-event (event model-or-observer container-event)
  )

(defmethod handle-event ((event container-event) model-or-observer)
  (handle-container-event event model-or-observer (container-of event)))
