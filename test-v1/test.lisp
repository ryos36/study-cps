(load "../mini-scheme.lisp")
(setf test-files '( "test0" ))

(defun read-file (name)
  (if (probe-file name)
    (with-open-file (str name :direction :input)
      (let ((buf (make-string (file-length str))))
        (read-sequence buf str)
        buf))))

(setf trim-string (string-concat
                    (string #\return)
                    (string #\newline)))
(mapcar 
  #'(lambda (name)
      (let 
        ((scm-file (string-concat name ".scm"))
         scheme-test-list
         result
         (temp-file "temp.out")
         (result-file (string-concat "result/" name ".out"))
         result-txt
         )

        (setf scheme-test-list
              (with-open-file (in scm-file)
                (read in)))
        (setf result-txt (read-file result-file))

        (setf result
              (with-output-to-string (str)
                (dolist (i scheme-test-list)
                  (format str "~a:~a~%" i 
                          (parse-mini-scheme i *env*)))))

        (setf result-txt (string-right-trim trim-string result-txt))
        (setf result (string-right-trim trim-string result))

        #|
        (setf result (string-concat result (string #\return)))
        (do ((i 1 (incf i))) ((= 26 i))
              (let ((s0 (substring result i (+ i 1)))
                    (s1 (substring result-txt i (+ i 1))))
                (format t "<~a:~s:~s:~a>~%" i
                        (char s0 0)
                        (char s1 0)
                        (string= s0 s1))))
        |#

        (if result-txt
          (format t "~a:~a" name
                  (if (string= result-txt result) "Passed" "Failed"))
          (progn
            (with-open-file (out result-file :if-does-not-exist :create :direction :output)
              (format out "~a" result))
            (format t "~a:Saved" name)))
        ))
  test-files)

