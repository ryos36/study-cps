(defmethod get-ordered-func-name ((parser closure-converter) r-func-names r-env-list)
  (let (ref-vars)
    (labels ((get-cmp-n (remain)
                        (- 
                          (reduce #'(lambda (r x) (max r (length x))) 
                                  remain
                                  :initial-value 0) 1)) 
             (get-ordered-func-name0 (r-free-vars-list0 rv remain)
               ;(print `(:get-name0 ,r-free-vars-list0 :rv ,rv))
               (if (null r-free-vars-list0) (values rv
                                                    (nreverse remain))

                 (let* ((top-elm (car r-free-vars-list0))
                        (func-name (car top-elm))
                        (ref-vars-list (set-difference (cdr top-elm) rv)))

                   (if (null ref-vars-list)
                     (push func-name rv)
                     (push (cons func-name ref-vars-list) remain))
                   (get-ordered-func-name0 (cdr r-free-vars-list0) rv remain))))

             (get-ordered-func-name1 (r-free-vars-list1 src-rv old-len)
                  (multiple-value-bind (rv remain) 
                    (get-ordered-func-name0 r-free-vars-list1 src-rv '())

                    (if (null remain)
                      rv

                      (let ((rv-len (length rv)))
                        (if (= old-len rv-len)
                          (get-ordered-func-name-next1 remain rv 0 (get-cmp-n remain))
                          (get-ordered-func-name1 remain rv rv-len))))))

             (get-ordered-func-name-next0 (r-free-vars-list-next0 rv remain cmp-n)

               (if (null r-free-vars-list-next0) (values rv
                                                         (nreverse remain))
                 (let* ((top-elm (car r-free-vars-list-next0))
                        (func-name (car top-elm))
                        (ref-vars-list (set-difference (set-difference (cdr top-elm) rv) ref-vars)))

                   (if (and (null remain) (= (length ref-vars-list) cmp-n))
                     (let ((ref-sym (car ref-vars-list)))
                       (push func-name rv)
                       (mapc #'(lambda (ref-sym)
                                 (push ref-sym ref-vars)) ref-vars-list))

                     (push (cons func-name ref-vars-list) remain))
                   (get-ordered-func-name-next0 (cdr r-free-vars-list-next0) rv remain cmp-n))))

             (get-ordered-func-name-next1 (r-free-vars-list-next1 src-rv old-len cmp-n)
                  (multiple-value-bind (rv remain) 
                    (get-ordered-func-name-next0 r-free-vars-list-next1 src-rv '() cmp-n)

                    (if (null remain)
                      rv

                      (let ((rv-len (length rv)))
                        (if (= old-len rv-len)
                          (get-ordered-func-name-next1 remain rv 0 (get-cmp-n remain))
                          (get-ordered-func-name1 remain rv 0)))))))

      (let* ((r-free-vars-list
               (mapcar #'(lambda (all-variables)
                           (filter-free-variables all-variables)) r-env-list))
             (r-ref-func-list
               (mapcar #'(lambda (vars self-name)
                           (cons self-name
                                 (remove self-name
                                         (intersection vars r-func-names))))
                       r-free-vars-list
                       r-func-names))
             ;(x (print `(:r-ref-func-list ,r-ref-func-list)))
             (rv (get-ordered-func-name1 r-ref-func-list '() 0)))
        (values rv (nreverse ref-vars))))))
