;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass view-base ()
  ((model :reader model-of :initarg :model
          :initform λVnil)

   ;; Dataflow: MODEL -> MODEL-OBSERVERS => VIEW-BASE (some widget in SW).
   (model-observers :reader model-observers-of
                    :type list
                    :initform nil)

   ;; [SIGNATURE (context-view . model) -> VIEW]
   (views-in-context :type hash-table
                     :initform (make-hash-table :test #'equal :weakness :value :synchronized t))))


(defmethod initialize-instance :after ((view view-base) &key)
  (setf (model-of view) (model-of view)))


(defgeneric set-model (view-base model)
  (:method-combination nconc :most-specific-last)
  (:documentation "Assign MODEL as Model for VIEW-BASE."))


(defmethod (setf model-of) (new-model (view view-base))
  (prog1 new-model
    (let ((old-model-observers (model-observers-of view)))
      (with-object view
        (setf ¤model-observers (with1 (set-model view new-model)
                                 (dolist (model-observer it)
                                   (check-type model-observer cell)))
              ¤model new-model))
      (dolist (old-model-observer old-model-observers)
        (cell-mark-as-dead old-model-observer)))))


(defmethod ensure-container ((arg view-base))
  (with1 (model-of arg)
    (assert (typep it 'multiple-value-model))))


(defmethod ensure-model ((arg view-base))
  (model-of arg))


(add-deref-type 'view-base
                :get-expansion (λ (arg-sym) `(model-of ,arg-sym))
                :set-expansion t)


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


(defmethod print-object ((view-base view-base) stream)
  #| NOTE: We're not printing identity here because it is assumed that any sub-class of VIEW-BASE will have its own
  ID slot or mechanism and print this. |#
  (print-unreadable-object (view-base stream :type t :identity nil)
    (print-slots view-base stream)))


(defmethod print-slots progn ((view-base view-base) stream)
  (when (slot-boundp view-base 'model)
    (format stream " :MODEL ~S" (slot-value view-base 'model))))
