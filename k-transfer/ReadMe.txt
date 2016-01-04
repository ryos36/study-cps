free-variable-finder.lisp
  指定された cps のしたを単純にサーチし
    env の直下に
    (v0 . t)
    (v1)
  　のリストをつくる。 t なら宣言されている。
    nil なら宣言されていない = free
    env はネストしない
  つまり env 内の変数リストは
   ((v0 . t) (v1) (v2) (v3 .t))
  のようになる。
  子供に FIX があるかどうか問題にしない。
  親の FIX に関連してがかならず heap あるいは stack をつくるため。
    
closure-converter.lisp
  上とは違う方法で変数を管理
  FIX[S/H] がでてくるとまず単純に管理下の free-vars を数え上げる。
  しかし、これは上の FIX[S/H] ですでに heap 済みかもしれない
  そこで、いままで登録された FIX[S/H] を env から逆順にたどり
  そこにはいっていたら free-vars から削る
  結果として該当する FIX での free-vars のみがわかる。
  (同じ変数は上位の FIX で共有する。違う階層の共有)
　これを fixh なら fix の上位に
  fixs なら app の上位に作る
  
  使う際には fixs ではすぐに bind のなかでレジスタに復元して pop する。

  fixh なら使われる直前で毎回 record-ref する。(スレッド対応)
  シンボルを評価する際にうえをたどっていき、先に出てきた 
    FIXH によってつくられたクロージャーか
    宣言された変数かFIXS でつくられたくろーじゃ
  のどちらかを判定し、FIXH のくろーじゃのときだけ record-ref する。

  また fixh では該当する fixh に複数の bind があった場合に共有する
  (同階層の共有)
　そのため fixh では heap が二段階になっている。
