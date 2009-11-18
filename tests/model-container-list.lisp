;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)


(flet ((test ()
         (defclass test-view (view-base) ())

         (let ((worksp nil))

           (defmethod (setf model-of) ((container dlist) (view test-view))
             (with-formula view
               (when-let (event (event-of container))
                 (tf worksp))))

           (let* ((model (dlist))
                  (view (make-instance 'test-view :model model)))
             (insert λV42 :in model)
             (assert worksp)
             view))


         ;; TODO: Ho-hum. I think this actually manages to race with the call to INSERT above.
         (remove-method #'(setf model-of)
                        (find-method #'(setf model-of) nil (mapcar #'find-class '(dlist test-view))))
         (setf (find-class 'test-view nil) nil)
         (unintern 'test-view)))
  (test))
