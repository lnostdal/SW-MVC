;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defmacro cell-of (arg &key warn-p error-p)
  "This is used to extract a CELL instance from \"something\". This tends to
mean CELL instances stored in CLOS slots (:METACLASS MVC-CLASS)."
  (with-gensyms (result)
    `(let* ((*get-cell-p* t)
            (,result ,arg))
       (prog1 ,result
         ,(when warn-p
           `(unless (typep ,result 'cell)
              (warn "~A, ~A returning something not a CELL; ~S" =lex-function-name= 'cell-of ,result)))
         ,(when error-p
           `(unless (typep ,result 'cell)
              (error "~A, ~A returning something not a CELL; ~S" =lex-function-name= 'cell-of ,result)))))))


(defmacro formula-of (arg &key warn-p error-p)
  "This is used to extract a FORMULA instance from \"something\". This tends to
mean FORMULA instances stored in CLOS slots (:METACLASS MVC-CLASS)."
  (with-gensyms (result)
    `(let* ((*get-formula-p* t)
            (,result ,arg))
       (prog1 ,result
         ,(when warn-p
           `(unless (typep ,result 'formula)
              (warn "~A, ~A returning something not a FORMULA; ~A" =lex-function-name= 'formula-of ,result)))
         ,(when error-p
           `(unless (typep ,result 'formula)
              (error "~A, ~A returning something not a FORMULA; ~A" =lex-function-name= 'formula-of ,result)))))))
