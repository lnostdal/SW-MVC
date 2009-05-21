;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(declaim (optimize speed))


(defgeneric mk-view (model context &key)
  (:documentation "
Creates a view (might be a widget or another model) of MODEL in some CONTEXT.
The view is automatically set as an observer of MODEL."))


(defmethod mk-view :around (model (context view-base) &key)
  (let ((view (call-next-method)))
    (setf (model-of view) model)
    view))


(defmethod mk-view (model (context-view view-base) &rest args &key)
  (apply #'view-constructor context-view model args))


(defmethod view-in-context-of ((context-view view-base) model)
  "Returns a \"view\" (some sub-instance of VIEW-BASE) of MODEL in context of CONTEXT-VIEW."
  (with-slots (views-in-context) context-view
    (let ((signature (cons context-view model)))
      (sb-ext:with-locked-hash-table (views-in-context)
        (multiple-value-bind (view found-p)
            (gethash signature views-in-context)
          (if found-p
              view
              (setf (gethash signature views-in-context)
                    (mk-view model context-view))))))))
