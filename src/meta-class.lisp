;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


#| TODO: I'm wondering if this :WEAKNESS stuff is a bit more flexible than I
need most of the time. I suspect it'll add quite a bit of overhead,
eventually. |#


#| This stuff is too specific. There should be two hash-tables, EQ and EQUAL
based as now, but stuff here shouldn't assume we're working on or vs. CLOS
instances only. I.e., it should be generalized and talk about \"memory
locations\" or resources in the abstract. |#


(mk-meta-slot object-observers-of :weakness :key)
;; [OBJECT -> [OBSERVER -> CALLBACKS]]   or   [MODEL -> [VIEW -> FORMULAS]]
;;
;; OBJECT and OBSERVER are weak.


#| TODO
It would be nice to have an API that would enable one to "chain" hash-tables
like this more easily. Making :weakness and :test configurable on a pr.
link (hash-table) basis would be great.
|#

(mk-meta-slot slot-observers-of :weakness :key)
;; [OBJECT -> [SLOT-NAME -> [OBSERVER -> CALLBACKS]]]   or   [MODEL -> [SLOT-NAME -> [VIEW -> FORMULAS]]]
;;
;; OBJECT and OBSERVER are weak.


(defmethod handle-circularity (observer touched-observers
                               object callback)
  ;; Dodge getting stuck in a loop by taking a single step back.
  (invoke-restart 'abort-mvc-event))


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
                (handle-circularity observer *touched-observers*
                                    object callback))
              (let ((*touched-observers* (cons observer *touched-observers*)))
                (funcall callback event)))
            callbacks)
      (setf (gethash observer observers)
            callbacks))))


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
                  (handle-circularity (cons observer slot-name) *touched-observers*
                                      object callback))
                (let ((*touched-observers* (cons (cons observer slot-name) *touched-observers*)))
                  (funcall callback event)))
              callbacks)
        (setf (gethash observer observers)
              callbacks)))))


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
  ;; TODO: Optimize this. There is no need to construct a SLOT-SET instance and call HANDLE
  ;; when INSTANCE itself is being constructed. The event is sent into the "void".
  (let* ((slot-name (slot-definition-name slot-definition))
         (old-value nil) (old-value-p nil)
         (event (make-instance 'slot-set
                               :instance instance
                               :slot-name slot-name
                               :new-value new-value)))
    ;; Possibly set OLD-VALUE slot.
    (when (slot-boundp-using-class class instance slot-definition)
      (allf (cell-of (slot-value-using-class class instance slot-definition))
            (slot-value event 'old-value)
            old-value)
      (tf old-value-p))

    (when *simulate-slot-set-event-p*
      (handle event)
      (return-from slot-value-using-class))

    (prog1
        (cond
          ((and old-value-p (typep old-value 'cell) (not *get-cell-p*))
           (setf (slot-value old-value 'value) new-value))

          (t
           (call-next-method)))
      (when (typep new-value 'formula)
        (formula-add-target new-value instance slot-name))
      (handle event))))


(defmethod slot-value-using-class :around ((class mvc-class) instance slot-definition)
  (let ((value (handler-case (call-next-method)
                 (unbound-slot (c)
                   (unless *formula*
                     (error c))
                   ;; TODO: It is perhaps possible to append this information to the string in C?
                   (warn "UNBOUND-SLOT while initializing formula: ~A" *formula*)
                   (error c)))))

    (when (and *formula* (member :input-eval (mode-of *formula*) :test #'eq))
      (formula-add-source *formula* instance (slot-definition-name slot-definition)))

    (cond
      ((and (typep value 'formula) (not *get-formula-p*))
       (let ((formula value))
         (when (member :output-eval (mode-of formula) :test #'eq)
           ;; TODO: Think about the argument here, and *EVENT-STACK*.
           (funcall (closure-of formula) :output-eval))
         (ref-value-of (slot-value formula 'value))))

      ((and (typep value 'cell) (not *get-cell-p*))
       (slot-value value 'value))

      (t
       value))))
