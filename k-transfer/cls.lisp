
(defparameter g nil)
(defparameter f0 nil)
(defparameter f1 nil)
(defparameter p nil)
(defparameter f0-p nil)
(setf k1 'global-var)
(defparameter k3 'global-var)

(let ((p nil) (k0 3) (k1 4) (k2 5) f0 f1 p)
  (defun f0 (a0 a1 a2)
    (incf k0))

  (setf f0
        (flet ((f0 (a0 a1 a2)
                   (setf k2 k0)
                   (setf k0 (+ a0 a1 a2))
                   (let ((k1 'masked)
                         (k2 'closure)
                         (k3 'closure))
                     (flet ((p ()  (print `(:f0-p ,k0 ,k1 ,k2 ,k3))))
                       #'p))))
          #'f0))
  (setf k2 10)
  (setf p
        (flet ((p () (print `(,k0 ,k1 ,k2 ,k3))))
          #'p))
  (setf f1
        (flet ((f1 (a0 a1 a2) (setf k0 a0 k1 a1 k2 a2)))
          #'f1))
  (setf g `(,f0 ,f1 ,p)))

(setf f0 (car g))
(setf f1 (cadr g))
(setf p (caddr g))

(funcall p)
(funcall f1 1 2 3)
(funcall p)
(setf f0-p (funcall f0 4 5 6))
(funcall p)
(funcall f0-p)
(print `(k3 ,k3))
