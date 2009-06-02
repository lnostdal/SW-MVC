;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass view-base ()
  ((model :reader model-of)

   (formula)

   ;; [SIGNATURE (context-view . model) -> VIEW]
   (views-in-context :type hash-table
                     :initform (make-hash-table :test #'equal :weakness :value))

   (view-constructor-fn :accessor view-constructor-fn-of :initarg :view-constructor-fn
                        :initform (lambda (context-view model &key)
                                    (declare (ignore model))
                                    (error "SW-MVC: The VIEW-CONSTRUCTOR-FN slot in ~A has not been assigned."
                                           context-view))
                        :documentation "
Function with lambda-list (CONTEXT-VIEW MODEL &REST ARGS). Must return an
object which is a sub-type of VIEW-BASE.")))


(defmethod initialize-instance :after ((view view-base) &key (model nil model-supplied-p))
  (if model-supplied-p
      (setf (model-of view) model)
      (when (slot-boundp view 'model)
        (setf (model-of view) (slot-value view 'model)))))


(defmethod (setf model-of) :before (new-model (view view-base))
  (setf (slot-value view 'model) new-model))


(defmethod (setf model-of) (new-model (view view-base))
  )


(defmethod deref ((view view-base))
  (model-of view))


(defmethod (setf deref) (new-model (view view-base))
  (setf (model-of view) new-model))


(defmethod view-constructor ((context-view view-base) model &rest args)
  "This is called to construct a new View based on MODEL in context of
CONTEXT-VIEW. The default method calls the function held in the slot
VIEW-CONSTRUCTOR-FN in CONTEXT-VIEW if there is a function there."
  (when-let ((view-constructor-fn (view-constructor-fn-of context-view)))
    (apply (the function view-constructor-fn) context-view model args)))


(defmethod mk-view :around (model (context view-base) &key)
  (let ((view (call-next-method)))
    (setf (model-of view) model)
    view))


(defmethod mk-view (model (context-view view-base) &rest args &key)
  (apply #'view-constructor context-view model args))


(defmethod view-in-context-of ((context-view view-base) model)
  "Returns a View (some sub-instance of VIEW-BASE) of MODEL in context of
CONTEXT-VIEW.
A second value FOUND-P is also returned. This is T if an already existing View
was found based on MODEL and CONTEXT-VIEW and NIL if a new View was
constructed, stored and returned."
  (with-slots (views-in-context) context-view
    (let ((signature (cons context-view model)))
      (sb-ext:with-locked-hash-table (views-in-context)
        (multiple-value-bind (view found-p)
            (gethash signature views-in-context)
          (if found-p
              (values view t)
              (values (setf (gethash signature views-in-context)
                            (mk-view model context-view))
                      nil)))))))
