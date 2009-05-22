;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim (optimize speed))


#|(class-forward-reference formula
  (:metaclass mvc-stm-class))|#


(mk-meta-slot object-observers-of :weakness :key)
;; [OBJECT -> [OBSERVER -> CALLBACKS]]
;;
;; OBJECT and OBSERVER are weak.


#| TODO
It would be nice to have an API that would enable one to "chain" hash-tables
like this more easily. Making :weakness and :test configurable on a pr.
link (hash-table) basis would be great.
|#

(mk-meta-slot slot-observers-of :weakness :key)
;; [OBJECT -> [SLOT-NAME -> [OBSERVER -> CALLBACKS]]]
;;
;; OBJECT and OBSERVER are weak.


(defparameter *touched-observers* nil
  "Used to detect circularity as stuff propagate.")


(defparameter *get-formula-p*
  nil)


;; TODO: This thing could be improved a great deal.
(defmethod report-circularity (observer touched-observers
                               object callback)
  (error "SW-MVC: Circularity in state propagation detected.
  Source object in question: ~A (with callback: ~A).
  Trying to propagate to: ~A.
  ..while the previous, still active, targets for propagation are:
~A" object callback observer touched-observers))


(defmacro with-callbacks ((object &key
                                  (slot-name nil slot-name-supplied-p)
                                  (callback-sym 'callback)
                                  (observer-sym 'observer))
                          &body body)
  "If SLOT-NAME is supplied this refers to callbacks for a (that) slot, or any slot if T was given.
If SLOT-NAME is not supplied this refers to callbacks for the object \"itself\"."
  (with-gensyms (observers)
    (if slot-name-supplied-p
        (with-gensyms (slot-names)
          `(when-let (,slot-names (slot-observers-of ,object))
             (when-let (,observers (gethash ,slot-name ,slot-names))
               (loop :for ,observer-sym :being :each :hash-key :in ,observers
                     :for ,callback-sym :being :each :hash-value :in ,observers
                  :do (dolist (,callback-sym ,callback-sym)
                        (declare (function ,callback-sym))
                        ,@body)))))
        `(when-let (,observers (object-observers-of ,object))
           (loop :for ,observer-sym :being :each :hash-key :in ,observers
                 :for ,callback-sym :being :each :hash-value :in ,observers
              :do (dolist (,callback-sym ,callback-sym)
                    (declare (function ,callback-sym))
                    ,@body))))))
  

(defun get-object-callbacks (observer object)
  (when-let ((observers (object-observers-of object)))
    (gethash observer observers)))


(defun get-object-observers (object)
  "Returns a list of objects observing OBJECT."
  (when-let ((observers (object-observers-of object)))
    (loop :for observer :being :each :hash-key :in observers
       :collect observer)))


(defun add-object-callback (observer object callback)
  "OBJECT is what is to be \"monitored\" by the OBSERVER.
OBSERVER can be the same object as OBJECT. It can be used to control the
extent (GC) of the connection between OBJECT and CALLBACK."
  (declare ((function (event)) callback))
  (let ((observers (object-observers-of object)))
    (unless observers
      (allf (make-hash-table :test #'eq :weakness :key)
            observers
            (object-observers-of object)))
    (let ((callbacks (gethash observer observers)))
      (push (lambda (event)
              (when (member observer *touched-observers* :test #'eq)
                (report-circularity observer *touched-observers*
                                    object callback))
              (let ((*touched-observers* (cons observer *touched-observers*)))
                (funcall callback event)))
            callbacks)
      (setf (gethash observer observers)
            callbacks))
    (handle-view-set-object-model observer object)))


(defun add-simple-object-callback (object callback)
  "Calls ADD-OBJECT-CALLBACK with OBJECT as argument for OBSERVER."
  (add-object-callback object object callback))


(defun remove-object-callback (observer object &optional
                               (callbacks nil callbacks-supplied-p))
  "Removes callbacks \"pointing from\" OBJECT to OBSERVER.
If CALLBACKS is supplied (it can be a single item or a list), only those
callbacks will be removed."
  (when-let ((observers (object-observers-of object)))
    (if callbacks-supplied-p
        (let ((existing-callbacks (gethash observer observers)))
          (setf (gethash observer observers)
                (set-difference existing-callbacks (mklst callbacks))))
        (remhash observer observers))))


(defun get-slot-callbacks (observer slot-name object)
  (declare (symbol slot-name))
  (when-let ((slot-names (slot-observers-of object)))
    (when-let ((observers (gethash slot-name slot-names)))
      (gethash observer observers))))


(defun get-slot-observers (slot-name object)
  "Returns a list of objects observing the slot designated by SLOT-NAME for OBJECT."
  (declare (symbol slot-name))
  (when-let ((slot-names (slot-observers-of object)))
    (when-let ((observers (gethash slot-name slot-names)))
      (loop :for observer :being :each :hash-key :in observers
         :collect observer))))


(defun add-slot-callback (observer slot-name object callback)
  "OBJECT is what is to be \"monitored\" by the OBSERVER.
OBSERVER can be the same object as OBJECT. It can be used to control the
extent (GC) of the connection between OBJECT and CALLBACK.
A SLOT-NAME T has special meaning in that the given CALLBACK is to be called
for all SLOT-SET events, regardless of slot (or SLOT-NAME)."
  (declare (symbol slot-name)
           ((function (event)) callback))
  (let ((slot-names (slot-observers-of object)))
    (unless slot-names
      (allf (make-hash-table :test #'eq)
            slot-names
            (slot-observers-of object)))
    (multiple-value-bind (observers found-p)
        (gethash slot-name slot-names)
      (let ((callbacks (if found-p
                           (gethash observer observers)
                           (prog1 nil
                             (allf (make-hash-table :test #'eq :weakness :key)
                                   (gethash slot-name slot-names)
                                   observers)))))
        (push (lambda (event)
                (when (member (cons observer slot-name) *touched-observers* :test #'equal)
                  (report-circularity (cons observer slot-name) *touched-observers*
                                      object callback))
                (let ((*touched-observers* (cons (cons observer slot-name) *touched-observers*)))
                  (funcall callback event)))
              callbacks)
        (setf (gethash observer observers)
              callbacks)))
    (handle-view-set-slot-model observer object slot-name)))


(defun add-simple-slot-callback (slot-name object callback)
  "Calls ADD-SLOT-CALLBACK with OBJECT as argument for OBSERVER."
  (declare (symbol slot-name)
           ((function (event)) callback))
  (add-slot-callback object slot-name object callback))


(defun remove-slot-callback (observer slot-name object &optional
                             (callbacks nil callbacks-supplied-p))
  "Removes callbacks \"pointing from\" SLOT-NAME of OBJECT to OBSERVER.
If CALLBACKS is supplied (it can be a single item or a list), only those
callbacks will be removed.
A SLOT-NAME of T can be given here; the meaning of T in this context is
explained in ADD-SLOT-CALLBACK."
  (declare (symbol slot-name))
  (when-let ((slot-names (slot-observers-of object)))
    (multiple-value-bind (observers found-p)
        (gethash slot-name slot-names)
      (when found-p
        (if callbacks-supplied-p 
            (let ((existing-callbacks (gethash observer observers)))
              (setf (gethash observer observers)
                    (set-difference existing-callbacks callbacks)))
            (remhash observer observers))))))



(defclass mvc-class (standard-class)
  ()

  (:documentation "
This metaclass propagates SLOT-SET (event-slot-set.lisp) instances to any
observers of the slot(s) in question."))


(defmethod validate-superclass ((class mvc-class) (superclass standard-class))
  t)


(defclass mvc-stm-class (mvc-class stm-class)
  ()

  (:documentation "
Combines the characteristics of the MVC-CLASS and STM-CLASS metaclasses.
See their doc-strings for info."))


(defmethod (setf slot-value-using-class) :around (new-value (class mvc-class) instance slot-definition)
  (let ((event (make-instance 'slot-set
                              :instance instance
                              :slot-name (slot-definition-name slot-definition)
                              :new-value new-value)))
    (with-sync ()
      ;; Set OLD-VALUE slot of EVENT if slot of INSTANCE is bound.
      ;; We might call S-V-U-C here and we don't want that to call FORMULA-ADD-SOURCE.
      (let ((*creating-formula* nil))
        (when (slot-boundp-using-class class instance slot-definition)
          (setf (slot-value event 'old-value)
                (slot-value-using-class class instance slot-definition))
          (let* ((*get-formula-p* t)
                 (value-or-formula (slot-value-using-class class instance slot-definition)))
            (when (and (typep value-or-formula 'formula)
                       (static-p-of value-or-formula))
              (error "SW-MVC: An attempt was made at mutating static formula ~A
in ~A (slot ~A) to new value ~S."
                     value-or-formula
                     instance
                     (closer-mop:slot-definition-name slot-definition)
                     new-value)))))

      (prog1 (call-next-method)
        (when (typep new-value 'formula)
          (let ((*creating-formula* nil))
            (formula-add-target new-value instance (slot-definition-name slot-definition))))
        (handle event)))))


(defmethod slot-value-using-class :around ((class mvc-class) instance slot-definition)
  (with-sync ()
    (let ((value (handler-case (call-next-method)
                   (unbound-slot (c)
                     (unless *creating-formula*
                       (error c))
                     ;; TODO: It is perhaps possible to append this information to the string in C?
                     (warn "UNBOUND-SLOT while initializing formula: ~A" *creating-formula*)
                     (error c)))))
      (when *creating-formula*
        (let ((formula *creating-formula*)
              (*creating-formula* nil))
          (formula-add-source formula instance (slot-definition-name slot-definition))))
      (if (and (typep value 'formula) (not *get-formula-p*))
          (value-of value)
          value))))
