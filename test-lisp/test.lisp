(setf *print-circle* t)

(defparameter *test-script-dir* nil)
(defparameter *test-ext* nil)
(defparameter *test-result-dir* "result/")
(defparameter *test-files* nil)
(defparameter *test-parse-func* nil)
(defparameter *test-save* t)

(defun set-test-files (max-no-or-no-list)
  (setf *test-files* 
        (if (listp max-no-or-no-list)
          (let ((no-list max-no-or-no-list))
            (mapcar #'(lambda (no) (format nil "test~a" no)) no-list))

          (let ((max-no max-no-or-no-list))
            (incf max-no)
            (nreverse 
              (let (test-files)
                (do ((x 0 (+ x 1)))
                  ((= x max-no))
                  (push (format nil "test~a" x ) test-files))
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
        (cps-gensym 0) ; reset
        (let 
          ((scm-file (string-concat *test-script-dir* name *test-ext*))
           lisp-test-list
           result
           (result-file (string-concat *test-result-dir* name ".txt"))
           result-txt
           )

          (setf lisp-test-list
                (with-open-file (in scm-file)
                  (read in)))
          (if *test-save*
            (setf result-txt (read-file result-file)))

          (setf result
                (with-output-to-string (str)
                  (dolist (i lisp-test-list)
                    (format str "~a:~a~%" i 
                            (funcall *test-parse-func* i *env*)))))

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

          (if result-txt
            (format t "~a:~a~%" name
                    (if (string= 
                          (string-right-trim trim-string result)
                          (string-right-trim trim-string result-txt))
                      "Passed" "Failed"))
            (progn
              (when *test-save*
               (with-open-file (out result-file :if-does-not-exist :create :direction :output)
                (format out "~a" result))

               (format t "~a:Saved~%" name))
              (format t  "--------------Result:-----------~%")
              (format t "~a~%" result)
              (format t  "--------------------------------~%")))
          ))
    *test-files*))

