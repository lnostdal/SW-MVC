;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass node ()
  ((container :accessor container-of
              :initform nil))

  (:metaclass mvc-class)
  (:documentation "
This represents a single entity in some container structure."))



(defclass model ()
  ()

  (:metaclass mvc-class)
  (:documentation "
Common base class for all Models."))


(defmethod model-of ((model model))
  model)



(defclass single-value-model (model)
  ()

  (:metaclass mvc-class)
  (:documentation "
A Model which is a subtype of this is a simpler kind of model in that it only
represents or holds a \"single value\" in some way.
This means that DEREF or ~ will most likely work as expected on this model."))



(defclass multiple-value-model (model)
  ((nodes-in-context :initform (make-hash-table :test #'equal :weakness :value)))

  (:metaclass mvc-class)
  (:documentation "
This usually means this is or represents some sort of container.
DEREF or ~ will most likely return a list of values, or further models in turn."))


(defmethod node-in-context-of ((container multiple-value-model) (model model))
  (with-slots (nodes-in-context) container
    (let ((signature (cons container model)))
      (sb-ext:with-locked-hash-table (nodes-in-context)
        (gethash signature nodes-in-context)))))


(defmethod (setf node-in-context-of) ((node node) (container multiple-value-model) (model model))
  (with-slots (nodes-in-context) container
    (let ((signature (cons container model)))
      (sb-ext:with-locked-hash-table (nodes-in-context)
        (setf (gethash signature nodes-in-context) node)))))
