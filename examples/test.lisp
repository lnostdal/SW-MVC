(in-package :sw-mvc)


(defun test-cell-3 ()
  (let* ((a1 #~0)
         (a2 #~1)
         (a3 #λ(let ((sum (+ ~a1 ~a2)))
                 (format t "cell a3 changing value to: ~A~%" sum)
                 sum))
         (a4 #λ(prog1 ~a3
                 (format t "cell a4 changing value to: ~A~%" ~a3))))

    #| WITH-CELLS sets up a couple of symbol macros which remove the need for one
    to explicitly dereference (~ character) stuff. |#
    (with-cells (a1 a2 a3 a4)
      (assert (= 1 a3 a4))
      
      #λ(format t "anonymous cell still alive (a1: ~A)~%" a1)

      (setf a1 2
            a2 3)
      (assert (= 5 a3 a4))
      
      (sb-ext:gc :full t)
      (sb-ext:gc :full t)

      (setf a1 4
            a2 5)
      (assert (= 9 a3 a4)))))


(defun test-cell-and-transaction ()
  (let* ((a #~2)
         (a-count 0)
         (a-count-io 0)
         (a-square #λ(let ((a-square (* ~a ~a)))
                       (incf a-count)
                       (when-commit ()
                         (format t "~A: A-SQUARE: ~A~%"
                                 (ignore-errors (tr-name aromyxo::*current-transaction*))
                                 a-square)
                         (incf a-count-io))
                       a-square))
         
         (b (mk-cell 3))
         (b-count 0)
         (b-count-io 0)
         (b-square #λ(let ((b-square (* ~b ~b)))
                       (incf b-count)
                       (when-commit ()
                         (format t "~A: B-SQUARE: ~A~%"
                                 (ignore-errors (tr-name aromyxo::*current-transaction*))
                                 b-square)
                         (incf b-count-io))
                       b-square)))

    #| WITH-CELLS sets up a couple of symbol macros which remove the need for one
    to explicitly dereference (~ character) stuff. This is not needed for
    transactions to work. |#
    (with-cells (a a-square b b-square)
      (assert (and (= a 2)
                   (= a-square 4)
                   (= b 3)
                   (= b-square 9)))
      (write-line "## BEGIN ##")
      (let ((thread-a (with-thread (:thread-a)
                        (with-sync (:name :thread-a)
                          (dbg-princ (setf a 3) "THREAD-A")
                          (sleep 0.5) ;; This and the sleep in THREAD-B will ensure a deadlock situation.
                          (dbg-princ (setf b 4) "THREAD-A"))))
            
            (thread-b (with-thread (:thread-b)
                        (with-sync (:name :thread-b)
                          (dbg-princ (setf b 6) "THREAD-B")
                          (sleep 0.5) ;; This and the sleep in THREAD-A will ensure a deadlock situation.
                          (dbg-princ (setf a 5) "THREAD-B")))))
        
        (join-thread thread-a)
        (join-thread thread-b)
        (write-line "## END ##")
        
        ;; The use of WHEN-COMMIT means this check will never fail no matter
        ;; how many tries the transaction has to go through to succeed.
        (assert (and (= a-count-io 3)
                     (= b-count-io 3)))
        
        ;; Show how I/O-type stuff or side-effects would fail if they had not been
        ;; wrapped in a WHEN-COMMIT.
        (dbg-princ a-count)
        (dbg-princ b-count)
        
        ;; In general there is no guarantee which of the threads finish last,
        ;; hence the use of OR here.
        (assert (or (and (= a-square 9)
                         (= b-square 16))
                    (and (= a-square 25)
                         (= b-square 36))))))))


(defun test-circularity ()
  (let* ((a #~0)
         (b #λ(incf ~a)))
    (declare (ignore b))
    (incf ~a)))



(defun test-sheet (size &optional (num-tests 1))
  (let ((sheet (make-array (list size))))
    (declare (optimize speed))

    ;; Set value cell and formula cells.
    (setf (svref sheet 0) #~0)
    (loop :for i :from 1 :below (array-dimension sheet 0)
       :do (setf (svref sheet i)
                 (let ((prev (svref sheet (1- i))))
                   #λ(1+ ~prev))))

    (time
     (dotimes (i num-tests)
       (time
        (incf ~(svref sheet 0)))))))
    

            
  


#|
(defclass person ()
  ((first-name :accessor first-name-of :initarg :first-name
               :initform "")

   (last-name :accessor last-name-of :initarg :last-name
              :initform ""))

  (:metaclass mvc-class))


(defclass location ()
  ((name :accessor name-of :initarg :name
         :initform ""))
  
  (:metaclass mvc-class))


(defun test-object-callback ()
  (let ((container (make-instance 'lisp-list))
        (person (make-instance 'person)))
    (add-simple-object-callback container
                                (lambda (event)
                                  (format t "event on container: ~A~%" event)))
    (add-simple-object-callback person 
                                (lambda (event)
                                  (format t "event on person: ~A~%" event)))
    (add person container)
    (terpri)
    (remove person container)
    (content-of container)))


(defun test-slot-callback ()
  (let ((person (make-instance 'person)))
    (add-slot-callback person 'first-name person
                       (lambda (event)
                         (declare (ignore event))
                         (format t "FIRST-NAME of ~A was changed~%" person)))
    (add-slot-callback person 'last-name person
                       (lambda (event)
                         (declare (ignore event))
                         (format t "LAST-NAME of ~A was changed~%" person)))
    (add-slot-callback person 'last-name person
                       (lambda (event)
                         (declare (ignore event))
                         (format t "whot, multiple callbacks ..~%")))
    (setf (first-name-of person) "lars")
    (setf (last-name-of person) "nøstdal")))
|#