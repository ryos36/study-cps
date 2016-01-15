(setf *print-circle* t)

(defparameter *test-script-dir* nil)
(defparameter *test-ext* nil)
(defparameter *test-name* "test-")
(defparameter *test-result-dir* "result/")
(defparameter *test-files* nil)
(defparameter *test-parse-func* nil)
(defparameter *test-save* t)
(defparameter *test-print* nil)
(defparameter *test-reset-func* nil)
(defparameter *test-success-n* 0)

(defun min-max-no-list (min-max-list)
  (let ((min-no (car min-max-list))
        (max-no (cdr min-max-list)))
    (labels ((make-list (n rv) (if (< n min-no) rv
                                 (make-list (- n 1)
                                            (push n rv)))))
      (make-list max-no '()))))

(defun easy-flatten (lst)
  (labels ((easy-flatten0 (lst0 rv)
                          (if (null lst0) rv
                            (let ((top-elm (car lst0)))
                              (easy-flatten0 
                                (cdr lst0)
                                (if (listp top-elm)
                                  (easy-flatten0 top-elm rv)
                                  (push top-elm rv)))))))
    (nreverse (easy-flatten0 lst '()))))

;(1 (2 . 5) 10)
(defun test-name (no)
  (if (numberp no)
    (format nil "~a~3,'0d" *test-name* no)
    (multiple-value-bind (new-no len) (parse-integer no :junk-allowed t)
      (if (and new-no (= (length no) len))
        (format nil "~a~3,'0d" *test-name* no)
        (format nil "~a~a" *test-name* no)))))

(defun set-test-files (max-no-or-no-list)
  (setf *test-files* 
        (if (listp max-no-or-no-list)
          (let ((no-list max-no-or-no-list))
            (if (stringp (car no-list)) `(,(test-name (car no-list)))
              (mapcar #'(lambda (no) (test-name no))
                      (easy-flatten 
                        (mapcar #'(lambda (no) 
                                    (if (listp no)
                                      (min-max-no-list no)
                                      no))
                                no-list)))))

          (let ((max-no max-no-or-no-list))
            (incf max-no)
            (nreverse 
              (let (test-files)
                (do ((x 0 (+ x 1)))
                  ((= x max-no))
                  (push (test-name x) test-files))
                test-files))))))

;(setf test-files (mapcar #'(lambda (no) (format nil "test~a" no)) *test-no*))

(defun read-file (name)
  (if (probe-file name)
    (with-open-file (str name :direction :input)
      (let ((buf (make-string (file-length str))))
        (read-sequence buf str)
        buf))))

(setf trim-string (string-concat
                    (string #\return)
                    (string #\newline)))
(defun do-test ()
  (if (null (directory *test-result-dir*)) (ext:make-directory *test-result-dir*))
  (mapcar 
    #'(lambda (name)
        (if *test-reset-func*
          (funcall *test-reset-func*))
        (let 
          ((scm-file (string-concat *test-script-dir* name *test-ext*))
           lisp-test-list
           result
           (result-file (string-concat *test-result-dir* name ".txt"))
           result-txt)

          (setf lisp-test-list
                (with-open-file (in scm-file)
                  (read in)))
          (if *test-save*
            (setf result-txt (read-file result-file)))

          (setf result
                (with-output-to-string (str)
                  (dolist (i lisp-test-list)
                    (let ((rv (funcall *test-parse-func* i *env*)))
                      (format str "~s~%:~%~s~%" i rv)
                      (if (equal i rv) 
                        (format str "SAME~%" i rv))))))

          #|
          (if result-txt 
            (setf result-txt (string-right-trim trim-string result-txt)))
          (setf result (string-right-trim trim-string result))
          (setf result (string-concat result (string #\Newline)))
          (do ((i 1 (incf i))) ((= (min (length result) (length result-txt)) i))
            (let ((s0 (substring result i (+ i 1)))
                  (s1 (substring result-txt i (+ i 1))))
              (format t "<~a:~a:~a:~a>~%" i
                      (char s0 0)
                      (char s1 0)
                      (string= s0 s1))))
          (format t "Length ~a:~a~a~%" (length result) (length result-txt) result-txt)
          |#

          (if (or *test-print* (not result-txt) (= (length *test-files*) 1))
            (progn
              (format t  "--------------Result:-----------~%")
              (format t "~a~%" result)
              (format t  "--------------------------------~%")))

          (if result-txt
            (let ((ok? (string= 
                         (string-right-trim trim-string result)
                         (string-right-trim trim-string result-txt))))
              (format t "~a:~a~%" name
                      (if ok? "Passed" "Failed"))

              (if ok?
                (incf *test-success-n*)))
            (when *test-save*
              (with-open-file (out result-file :if-does-not-exist :create :direction :output)
                (format out "~a" result))
              (format t "~a:Saved~%" name)))
          ))
    *test-files*)
  (format t "~%~a/~a~%" *test-success-n* (length *test-files*)))

