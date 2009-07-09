;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defclass test-view (view-base)
  ())


(let ((worksp nil))

  (defmethod (setf model-of) ((container dlist) (view test-view))
    (add-formulas
     view
     #λ(when-let (insert-event (insert-event-of container))
         (tf worksp))))

  (let* ((model (dlist))
         (view (make-instance 'test-view :model model)))
    (insert 42 :in model)
    (assert worksp)
    view))


(remove-method #'(setf model-of)
               (find-method #'(setf model-of) nil (mapcar #'find-class '(dlist test-view))))
(setf (find-class 'test-view nil) nil)
(unintern 'test-view)