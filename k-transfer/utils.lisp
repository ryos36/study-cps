(defun error-exit (expr env)
   (error 'parse-error :expr expr :env env))
