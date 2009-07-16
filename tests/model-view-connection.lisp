;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defclass test-model ()
  ((x :initarg :x
      :initform #λ0))

  (:metaclass mvc-class))


(defclass test-view (self-ref view-base)
  ((confirm :initform 0)
   (x :initform ↑(lambda (data) (incf ¤confirm data)))))


(defmethod (setf model-of) ((model test-model) (view test-view))
  (list #λ(funcall (slot-value view  'x)
                   (slot-value model 'x))))


(let* ((omodel (make-instance 'test-model))
       (view (make-instance 'test-view :model omodel)))
  (with-slots (confirm) view
    (symbol-macrolet ((ox (slot-value omodel 'x)))
      (assert (= 0 confirm))
      (incf ox)
      (assert (= 1 confirm))
      (let ((imodel (make-instance 'test-model :x #λ2)))
        (symbol-macrolet ((ix (slot-value imodel 'x)))
          (setf (model-of view) imodel)
          (assert (= 3 confirm))
          (setf ox :this-should-not-trigger-flow)
          (assert (= 3 confirm))
          (incf ix)
          (assert (= 6 confirm))))
      (setf ox :this-should-not-trigger-flow)
      (assert (= 6 confirm)))))



(remove-method #'(setf model-of)
               (find-method #'(setf model-of) nil (mapcar #'find-class '(test-model test-view))))
(setf (find-class 'test-model) nil
      (find-class 'test-view) nil)
(unintern 'test-model)
(unintern 'test-view)