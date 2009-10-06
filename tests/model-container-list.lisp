;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)
(in-readtable sw-mvc)


(defclass test-view (view-base)
  ())


(let ((worksp nil))

  (defmethod (setf model-of) ((container dlist) (view test-view))
    (with-formula view
      (when-let (event (event-of container))
        (tf worksp))))

  (let* ((model (dlist))
         (view (make-instance 'test-view :model model)))
    (insert #λ42 :in model)
    (assert worksp)
    view))


(remove-method #'(setf model-of)
               (find-method #'(setf model-of) nil (mapcar #'find-class '(dlist test-view))))
(setf (find-class 'test-view nil) nil)
(unintern 'test-view)
