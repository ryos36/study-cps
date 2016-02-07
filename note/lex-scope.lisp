(setf make-func
(let (a b)
  (labels ((make-func () (defun g (x) (setf b x) (format t "g a:‾a b:‾a‾%" a b)))) (setf b #'make-func))
  (defun f (x) (setf a x) (format t "f a:‾a b:‾a‾%" a b))
  b))
