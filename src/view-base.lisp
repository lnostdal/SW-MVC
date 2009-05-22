;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


;; TODO: Typecheck wrt. model type.


(defclass view-base ()
  ((model :accessor model-of
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
object which is a sub-type of VIEW-BASE.")

   (view-callback))

  (:metaclass mvc-stm-class))


(defmethod initialize-instance :after ((view view-base) &key (model nil model-supplied-p))
  (if model-supplied-p
      (setf (model-of view) model)
      (when (slot-boundp view 'model)
        (setf (model-of view) (slot-value view 'model)))))


(defmethod deref ((view view-base))
  (model-of view))


(defmethod view-constructor ((context-view view-base) model &rest args)
  "This is called to construct a new View based on MODEL in context of
CONTEXT-VIEW. The default method calls the function held in the slot
VIEW-CONSTRUCTOR-FN in CONTEXT-VIEW if there is a function there."
  (when-let ((view-constructor-fn (view-constructor-fn-of context-view)))
    (apply (the function view-constructor-fn) context-view model args)))


(defmethod (setf model-of) :before (model (view view-base))
  "Connect MODEL with VIEW. This makes VIEW an observer of MODEL and all slots
in/of MODEL."
  (dbg-princ model)
  (when-let (old-model (model-of view))
    (when (eq old-model model)
      (return-from model-of))
    (setf (model-of view) nil))
  (with-slots (view-callback) view
    (setf view-callback
          (lambda (event)
            (typecase event
              (slot-set
               (handle-model-slot-set-event view model (slot-name-of event) event))
              (otherwise
               (handle-model-event view model event)))))
    (add-object-callback view model view-callback)
    (add-slot-callback view t model view-callback)))


(defmethod (setf model-of) :before ((model (eql nil)) (view view-base))
  "Remove the connected model from VIEW."
  (when-let (old-model (model-of view))
    (when (slot-boundp view 'view-callback)
      (with-slots (view-callback) view
        (remove-object-callback view model view-callback)
        (remove-slot-callback view t model view-callback))
      (slot-makunbound view 'view-callback))))


(defmethod handle-model-event ((view view-base) model
                               (event event))
  "This method is called when MODEL somehow changes.
Users are meant to add methods to this gf based on their VIEW classes.
The default method does nothing."
  )


#| TODO: I got to think about the SLOT-NAME thing here as a class hierachy can
have multiple nodes wherein the same slot-name is used multiple places. Maybe
SLOT-DEFINITION is a better idea or name. |#
(defmethod handle-model-slot-set-event ((view view-base) model (slot-name symbol)
                                        (event slot-set))
  "This method is called when the slot designated by SLOT-NAME in MODEL is set.
Users are meant to add methods to this gf for ther VIEW classes.
The default method does nothing."
  )


(defmethod handle-view-set-object-model (view model)
  "This method is called when VIEW is set to present MODEL.
Users are meant to add methods to this gf for their VIEW classes.
The default method does nothing."
  )


#| TODO: I got to think about the SLOT-NAME thing here as a class hierachy can
have multiple nodes wherein the same slot-name is used multiple places. Maybe
SLOT-DEFINITION is a better idea or name. |#
(defmethod handle-view-set-slot-model (view model slot-name)
  "This method is called when VIEW is set to present SLOT-NAME of MODEL.
Users are meant to add methods to this gf for their VIEW classes.
The default method does nothing."
  )
