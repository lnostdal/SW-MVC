;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defun feedback-event-of (model)
  (declare (model model))
  ~(metadata-of model 'feedback-event #~nil))


(defun (setf feedback-event-of) (arg model)
  (declare (model model))
  ;; TODO: Before changing this, get rid of the setf expansion macro for METADATA-OF.
  (let ((cell (metadata-of model 'feedback-event #~nil)))
    (setf ~cell arg)))
