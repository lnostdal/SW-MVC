;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defmacro cell-of (arg &key warnp errorp)
  "This is used to extract a CELL instance from \"something\".
This tends to mean CELL instances stored in CLOS slots of MVC-CLASS classes."
  (with-gensyms (result)
    `(let* ((*get-cell-p* t)
            (,result ,arg))
       (typecase ,result
         (cell ,result)
         (otherwise
          (prog1 ,result
            (if (or ,warnp ,errorp)
                (let ((args (list "CELL-OF (~A) returning something not a CELL: ~S"
                                  =lex-function-name= ,result)))
                  (when ,warnp
                    (apply #'warn args))
                  (when ,errorp
                    (apply #'cerror "Continue; return the value anyway" args))))))))))


(defmacro setf-cell-of (place new-value)
  `(cell-of (setf ,place ,new-value)))


(defsetf cell-of setf-cell-of)


(defmacro formula-of (arg &key warnp errorp)
  "This is used to extract a FUNCTION (formula) instance from \"something\".
This tends to mean FORMULA instances stored in CLOS slots of MVC-CLASS classes."
  (with-gensyms (result)
    (once-only (warnp errorp)
      `(let* ((*get-formula-p* t)
              (,result ,arg))
         (typecase ,result
           (function ,result)
           ;; This'll enable one to do: (let ((x #λ42)) (formula-of x))
           (cell (slot-value ,result 'formula))
           (otherwise
            (prog1 ,result
              (if (or ,warnp ,errorp)
                  (let ((args (list "FORMULA-OF (~A) returning something not a FUNCTION (\"formula\"): ~S"
                                    =lex-function-name= ,result)))
                    (when ,warnp
                      (apply #'warn args))
                    (when ,errorp
                      (apply #'cerror "Continue; return the value anyway" args)))))))))))


;; TODO: This does not currently update dependencies etc.
(defmacro setf-formula-of (place new-value)
  `(cell-set-formula (cell-of ,place) ,new-value))
(defsetf formula-of setf-formula-of)