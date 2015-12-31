何やっているのか忘れた、、、
2015年 12月 31日 木曜日 12:10:12 JST

make-env ()
  関数の登録として :exit をしている気がする。
  呼ぼうと思っていたのか？ cps-interp の残骸？のようだ。

check-eta-reduction 
    関数 check-eta-reduction をより厳密にチェックすべきか？
    　　昨日作った optimize.lisp の記述の方が正確
        optimize.lisp は walk-cps していないのでチェックが厳しい。
        walk-cps しているので cps の構文が崩れていないという前提に
        たてば今のままでよいはず。つまり optimize.lisp はいらない。
        その場合、optimize.lisp のチェックが必要ないことを再確認して
        optimize.lisp を削除する
　っていうかチェック用の汎用的なマクロ化関数を作るべきだな。
　つまりチェック言語。

  hit したら旧関数名と新関数名を
    (cons func-name op-func-name) 
  で返す。hit してなかったら nil

  cps-fix で check-eta-reduction をすべてのバインドで調べている。
　
normalize-reduction-list (re-list)
  上の check-eta-reduction で得た結果の list (binds を mapcar で生成) を
  の再帰を削除している。
  例えば、 a -> b で b -> c なら a -> c になるべき。推移律を最適化している。
  引数の re-list は binds の mapcar なので hit しなかったときの nil も含む。
  結果も nil を含む

cps-fix
  うえの re-list を table にして env の先頭につけて渡している。
  env は関数の変換リストになっている模様
  cps-walk でシンボルにあたると最終的にこの table を使って変換する。

  re-list は nil も含む。nil を有効利用している。
  つまり binds の list は re-list と対応していて re-list で nil が
　あるところが変換の必要がある箇所で、list になっているところは
  eta-reduction での形式になっているので変換の必要もないし、
  最終的には削除。そのため remove-if で 削除している。

  binds -> re-list 作成
  binds と re-list から新しい binds を作成する(mapの処理)
    新 binds ではシンボルが変換された上に削除すべき binds は nil になっている
  新 binds から remove-if で nil を削除すると new-binds となる

n-reduction という関数があるが eta-reduction の旧バージョンで
　現在は使われていない。n-reduction0 も使われていない。
