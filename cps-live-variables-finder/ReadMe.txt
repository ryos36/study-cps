bind が最初になる。

top環境は
    `(:op (:declare ,@declare-vars) 
          (:use ,@use-vars-ignore-not-symbol)
          (:live ))) top-env)
が push されたリストになる。
つまり (car env) は
((:op (:declare ...) (:use ...) (:live ...))
 (:op (:declare ...) (:use ...) (:live ...)))
となりうえに行くほど( car でとれる方が)新しい。

それがさらに、環境として bind ごとに生成される。

