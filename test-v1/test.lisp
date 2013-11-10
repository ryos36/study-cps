(load "mini-lisp.lisp")
(setf test-files '( "test0" "test1" "test2" "test3" "test4" "test5" "test6"))

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
        ((scm-file (string-concat "script/" name ".scm"))
         lisp-test-list
         result
         (result-file (string-concat "result/" name ".txt"))
         result-txt
         )

        (setf lisp-test-list
              (with-open-file (in scm-file)
                (read in)))
        (setf result-txt (read-file result-file))

        (setf result
              (with-output-to-string (str)
                (dolist (i lisp-test-list)
                  (format str "~a:~a~%" i 
                          (parse-mini-lisp i *env*)))))

        #|
        (if result-txt 
         (setf result-txt (string-right-trim trim-string result-txt)))
        (setf result (string-right-trim trim-string result))
        (setf result (string-concat result (string #\Newline)))
        (do ((i 1 (incf i))) ((= (min (length result) (length result-txt)) i))
              (let ((s0 (substring result i (+ i 1)))
                    (s1 (substring result-txt i (+ i 1))))
                (format t "<~a:~s:~s:~a>~%" i
                        (char s0 0)
                        (char s1 0)
                        (string= s0 s1))))
        (format t "Length ~a:~a~s~%" (length result) (length result-txt) result-txt)
        |#

        (if result-txt
          (format t "~a:~a~%" name
                  (if (string= 
                        (string-right-trim trim-string result)
                        (string-right-trim trim-string result-txt))
                    "Passed" "Failed"))
          (progn
            (with-open-file (out result-file :if-does-not-exist :create :direction :output)
              (format out "~a" result))

            (format t "~a:Saved~%" name)
            (format t  "--------------Result:-----------~%")
            (format t "~a~%" result)
            (format t  "--------------------------------~%")))
        ))
  test-files)

