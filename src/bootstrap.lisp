;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(declaim #.(optimizations))


(defmacro mk-vcell (&body body)
  "Creates a \"value CELL\". Basically just lazy-eval."
  `(make-instance 'cell
                  :formula (lambda () ,@body)
                  :input-evalp nil :output-evalp nil
                  :init-evalp t))


(defmacro mk-icell (&body body)
  "Creates an input-triggered CELL."
  `(make-instance 'cell
                  :formula (lambda () ,@body)
                  :input-evalp t :output-evalp nil))


(defmacro mk-ocell (&body body)
  "Creates an output-triggered CELL."
  `(make-instance 'cell
                  :formula (lambda () ,@body)
                  :input-evalp nil :output-evalp t))


(defmacro mk-ccell (&body body)
  "Creates an output-treggered cached CELL."
  `(make-instance 'cell
                  :formula (lambda () ,@body)
                  :input-evalp nil
                  :output-evalp :cached))


(declaim (inline as-formula))
(defun as-formula (cell)
  (cons '%formula cell))


#| TODO: Perhaps this macro should be generalized/renamed to something else and simply expand to the following:

  (standard-instance-access instance (slot-definition-location eslotd))
|#
(defmacro cell-of (arg &key warnp errorp)
  "This is used to extract a CELL instance from \"something\".
This tends to mean CELL instances stored in (used to represent) CLOS slots of MVC-CLASS classes.

User code should probably not use this as it is considered \"dangerous\" wrt. thread safety."
  (with-gensyms (result)
    `(let ((,result (let ((*get-cell-p* t)) ,arg)))
       (typecase ,result
         (cell ,result)
         (mvc-condition (slot-value ,result 'cell))
         (otherwise
          (prog1 ,result
            (if (or ,warnp ,errorp)
                (let ((args (list "CELL-OF (~A) returning something not a CELL: ~S"
                                  =lex-function-name= ,result)))
                  (when ,warnp
                    (apply #'warn args))
                  (when ,errorp
                    (apply #'cerror "Continue; return the value anyway" args))))))))))


;; TODO: Think about what means (probably nothing) wrt. already existing dependencies etc.
(defmacro setf-cell-of (place new-value)
  (once-only (new-value)
    `(cell-of (setf ,place ,new-value))))
(defsetf cell-of setf-cell-of)


(defmacro formula-of (arg &key warnp errorp)
  "This is used to extract a FUNCTION (formula) instance from \"something\".
This tends to mean FORMULA instances stored in CLOS slots of MVC-CLASS classes."
  `(slot-value (cell-of ,arg ,warnp ,errorp)
               'formula))


;; TODO: This does not currently update/swap dependencies etc.
(defmacro setf-formula-of (place new-value)
  `(cell-set-formula (cell-of ,place) ,new-value))
(defsetf formula-of setf-formula-of)