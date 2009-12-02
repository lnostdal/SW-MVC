;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass mvc-class-observer ()
  ((model :reader model-of :initarg :model
          :initform λVnil)

   ;; Dataflow: MODEL => MODEL-OBSERVERS -> VIEW-BASE (some widget in SW).
   (model-observers :reader model-observers-of
                    :type list
                    :initform nil)))


(defmethod initialize-instance :after ((observer mvc-class-observer) &key)
  (setf (model-of observer) (slot-value observer 'model)))


(defmethod empty-p-of ((observer mvc-class-observer))
  (empty-p-of (model-of observer)))


(defgeneric set-model (observer model)
  (:method-combination nconc :most-specific-last)
  (:documentation "Assign MODEL as Model for OBSERVER."))


(defmethod (setf model-of) (new-model (observer mvc-class-observer))
  (prog1 new-model
    (let ((old-model-observers (model-observers-of observer)))
      (with-object observer
        (setf ¤model-observers (with1 (nconc (set-model observer new-model)
                                             (typecase new-model
                                               (container-proxy
                                                ;; Combine (NCONC) this SET-MODEL call and the one above.
                                                (set-model observer (model-of new-model)))
                                               (t nil)))
                                 (dolist (model-observer it)
                                   (check-type model-observer cell)))
              ¤model new-model))
      (dolist (old-model-observer old-model-observers)
        (cell-mark-as-dead old-model-observer)))))


(defmethod ensure-model ((observer mvc-class-observer))
  (model-of observer))


(add-deref-type 'mvc-class-observer
                :get-expansion (λ (arg-sym) `(model-of ,arg-sym))
                :set-expansion t)


(defmethod print-object ((observer mvc-class-observer) stream)
  #| NOTE: We're not printing identity here because it is assumed that any sub-class of VIEW-BASE will have its own
  ID slot or mechanism and print this. |#
  (print-unreadable-object (observer stream :type t :identity nil)
    (print-slots observer stream)))


(defmethod print-slots progn ((observer mvc-class-observer) stream)
  (when (slot-boundp observer 'model)
    (format stream " :MODEL ~S" (slot-value observer 'model))))
