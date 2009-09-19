;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass view-base ()
  ((model :reader model-of :reader container-of
          :initform nil)

   ;; Dataflow: MODEL -> MODEL-OBSERVERS => VIEW-BASE (some widget in SW).
   ;; This slot is set by the :AROUND (SETF MODEL-OF) method.
   (model-observers :reader model-observers-of
                    :type list
                    :initform nil)

   ;; [SIGNATURE (context-view . model) -> VIEW]
   (views-in-context :type hash-table
                     :initform (make-hash-table :test #'equal :weakness :value))

   (view-constructor-fn :accessor view-constructor-fn-of :initarg :view-constructor-fn
                        :type function
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
                                   (check-type model-observer cell)
                                   (assert (input-evalp-of model-observer))))
              ¤model new-model))
      (dolist (old-model-observer old-model-observers)
        (cell-mark-as-dead old-model-observer)))))


(defmethod deref ((view view-base))
  (model-of view))


(defmethod (setf deref) (new-model (view view-base))
  (setf (model-of view) new-model))


(defmethod view-constructor ((context-view view-base) model)
  "This is called to construct a new View based on MODEL in context of
CONTEXT-VIEW (probably a container). The default method calls the function held
in the slot VIEW-CONSTRUCTOR-FN in CONTEXT-VIEW if there is a function there."
  (if-let ((view-constructor-fn (view-constructor-fn-of context-view)))
    (funcall (the function view-constructor-fn) context-view model)
    (error "No suitable VIEW-CONSTRUCTOR method found for ~A and ~A,
and VIEW-CONSTRUCTOR-FN in ~A was NIL." context-view model context-view)))


(defun view-in-context-of (context-view model &optional create-if-not-found-p)
  "Returns a View (some sub-instance of VIEW-BASE) of MODEL in context of
CONTEXT-VIEW.

A second value, FOUND-P, is also returned. This is T if an already existing View
was found, :CREATED if a new View was constructed and NIL if no View was found
or constructed."
  (declare (view-base context-view)
           ((or model view-base) model))
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
  (declare (view-base view context-view))
  (setf model (model-of model))
  (with-slots (views-in-context) context-view
    (sb-ext:with-locked-hash-table (views-in-context)
      (let ((signature (cons context-view model)))
        (setf (gethash signature views-in-context) view)))))


(defmethod print-object ((view-base view-base) stream)
  #| NOTE: We're not printing identity here because it is assumed that any sub-class of VIEW-BASE will have its own
  ID slot or mechanism and print this. |#
  (print-unreadable-object (view-base stream :type t :identity nil)
    (print-slots view-base stream)))


(defmethod print-slots progn ((view-base view-base) stream)
  (when (slot-boundp view-base 'model)
    (format stream " :MODEL ~S" (slot-value view-base 'model))))
