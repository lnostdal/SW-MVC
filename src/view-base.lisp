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
  (when model-supplied-p
    (setf (model-of view) model)))


(defmethod print-object ((view-base view-base) stream)
  (print-unreadable-object (view-base stream :type t :identity t)
    (print-slots view-base stream)))


(defmethod print-slots progn ((view-base view-base) stream)
  (when (slot-boundp view-base 'model)
    (format stream " :MODEL ~S" (slot-value view-base 'model))))


(defmethod deref ((view view-base))
  (model-of view))


(defmethod (setf deref) (new-model (view view-base))
  (setf (model-of view) new-model))


(defmethod (setf model-of) :before (new-model (view view-base))
  (setf (slot-value view 'model) new-model))


(defmethod view-constructor ((context-view view-base) model)
  "This is called to construct a new View based on MODEL in context of
CONTEXT-VIEW. The default method calls the function held in the slot
VIEW-CONSTRUCTOR-FN in CONTEXT-VIEW if there is a function there."
  (if-let ((view-constructor-fn (view-constructor-fn-of context-view)))
    (funcall (the function view-constructor-fn) context-view model)
    (error "No suitable VIEW-CONSTRUCTOR method found for ~A and ~A,
and VIEW-CONSTRUCTOR-FN in ~A was NIL." context-view model context-view)))


(defun view-in-context-of (context-view model)
  "Returns a View (some sub-instance of VIEW-BASE) of MODEL in context of
CONTEXT-VIEW.
A second value FOUND-P is also returned. This is T if an already existing View
was found based on MODEL and CONTEXT-VIEW and NIL if a new View was
constructed, stored and returned."
  (declare (view-base context-view))
  (with-slots (views-in-context) context-view
    (let ((signature (cons context-view model)))
      (sb-ext:with-locked-hash-table (views-in-context)
        (multiple-value-bind (view found-p)
            (gethash signature views-in-context)
          (if found-p
              (values view t)
              (values (setf (gethash signature views-in-context)
                            (view-constructor context-view model))
                      nil)))))))
