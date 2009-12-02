;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
=common-headers=


(defclass view-base (mvc-class-observer)
  (;; [SIGNATURE (context-view . model) -> VIEW]
   (views-in-context :type hash-table
                     :initform (make-hash-table :test #'equal :weakness :value :synchronized t))))


(defun view-in-context-of (context-view model &optional create-if-not-found-p)
  "Returns a View (some sub-instance of VIEW-BASE) of MODEL in context of
CONTEXT-VIEW.

A second value, FOUND-P, is also returned. This is T if an already existing View
was found, :CREATED if a new View was constructed and NIL if no View was found
or constructed."
  (declare (view-base context-view)
           (model model)
           ((member t nil) create-if-not-found-p))
  (with-slots (views-in-context) context-view
    (sb-ext:with-locked-hash-table (views-in-context)
      (when (typep model 'view-base)
        (with (view-in-context-of context-view (model-of model))
          (if (eq model it)
            (return-from view-in-context-of model)
            (assert (null it))))
        (setf (view-in-context-of context-view (model-of model)) model)
        (return-from view-in-context-of model))
      (let ((signature (cons context-view model)))
        (multiple-value-bind (view found-p)
            (gethash signature views-in-context)
          (if found-p
              (values view t)
              (if create-if-not-found-p
                  (values (setf (gethash signature views-in-context)
                                (view-constructor context-view model))
                          :created)
                  (values nil nil))))))))


(defun (setf view-in-context-of) (view context-view model)
  (declare (view-base view context-view)
           (model model))
  (with-slots (views-in-context) context-view
    (let ((signature (cons context-view model)))
      (sb-ext:with-locked-hash-table (views-in-context)
        (setf (gethash signature views-in-context) view)))))
