;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)
(in-readtable sw-mvc)

(declaim #.(optimizations))


(defclass node ()
  ((container :accessor container-of
              :initform nil))

  (:metaclass mvc-class)
  (:documentation "
This represents a single entity in some container structure."))



(defclass model ()
  ()

  (:documentation "
Common base class for all Models."))


(defmethod model-of ((model model))
  model)


(defmethod ensure-model ((arg model))
  arg)



(defclass single-value-model (model)
  ()

  (:documentation "
A Model which is a subtype of this is a simpler kind of model in that it only
represents or holds a \"single value\" in some way.
This means that DEREF or ~ will most likely work as expected on this model."))



(defclass multiple-value-model (model)
  ((nodes-in-context :initform (make-hash-table :test #'equal :weakness :value)))

  (:documentation "
This usually means this is or represents some sort of container.
DEREF or ~ will most likely return a list of values, or further models in turn."))


(defmethod ensure-container ((arg multiple-value-model))
  arg)


(defun node-in-context-of (container model)
  (declare (multiple-value-model container)
           (model model))
  (with-slots (nodes-in-context) container
    (let ((signature (cons container model)))
      (sb-ext:with-locked-hash-table (nodes-in-context)
        (gethash signature nodes-in-context)))))


(defun (setf node-in-context-of) (node container model)
  (declare (node node)
           (multiple-value-model container)
           (model model))
  (with-slots (nodes-in-context) container
    (let ((signature (cons container model)))
      (sb-ext:with-locked-hash-table (nodes-in-context)
        (setf (gethash signature nodes-in-context) node)))))


(defmethod initialize-instance :after ((node node) &key (container nil container-supplied-p))
  (when container-supplied-p
    (check-type container multiple-value-model)
    (setf (container-of node) container)))


(defmethod (setf container-of) :after ((container multiple-value-model) (node node))
  (setf (node-in-context-of container ~node) node))
