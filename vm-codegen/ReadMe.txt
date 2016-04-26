------------------------------------------
pop だけ (:INTEGER <val>) を val として扱っている。
popi8 にしているけど pop にすべきかも。
------------------------------------------
codegen-tagged-list
レジスタをどの変数で使っているかを表したリスト +  app インフォ
app info はちょっとした分岐予測になっている
 (:CODEGEN (:REGISTER (NIL NIL NIL NIL NIL NIL NIL NIL G0 K)) (:APP-INFO)))

(new-args (update-register-usage codegen codegen-tagged-list args env op))
(not-used-reg (update-register-not-used codegen codegen-tagged-list live-vars-tagged-list))
(new-result (update-register-usage codegen codegen-tagged-list result env))


最初の update-register-usage で args をみる。
この時点でそもそも codegen-tagged-list にないのであれば
それはグローバル関数である。
グローバル変数はすべて closure-converter で解決しているため。
なので、わからない変数はすべてグローバル関数であると解釈する。
