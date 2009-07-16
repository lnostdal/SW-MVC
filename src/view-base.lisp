;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass view-base ()
  ((model :reader model-of
          :initform nil)

   ;; Dataflow: MODEL -> MODEL-OBSERVERS => VIEW-BASE (some widget in SW).
   ;; This slot is set by the :AROUND (SETF MODEL-OF) method.
   (model-observers :reader model-observers-of
                   ;;:type (or cell null)
                   :initform nil)

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


(defmethod initialize-instance :after ((view view-base) &key model)
  (when model (setf (model-of view) model)))


(defmethod (setf model-of) :around (new-model (view view-base))
  (let ((old-model-observers (model-observers-of view)))
    (prog1 new-model
      (with-object view
        (setf ¤model-observers (with1 (ensure-list (call-next-method))
                                 (dolist (model-observer it)
                                   (check-type model-observer cell)))
              ¤model new-model))
      (dolist (old-model-observers old-model-observers)
        ;; TODO: Should probably have a designated method for disabling CELLs, or perhaps it is time to add
        ;; a SOURCE-CELLS slot to the CELL class and do things proper.
        (nilf (slot-value old-model-observers 'input-evalp))))))


(defmethod deref ((view view-base))
  (model-of view))


(defmethod (setf deref) (new-model (view view-base))
  (setf (model-of view) new-model))


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


(defmethod print-object ((view-base view-base) stream)
  (print-unreadable-object (view-base stream :type t :identity t)
    (print-slots view-base stream)))


(defmethod print-slots progn ((view-base view-base) stream)
  (when (slot-boundp view-base 'model)
    (format stream " :MODEL ~S" (slot-value view-base 'model))))
