=============================================
ブロックの定義と anchor の導入

(op (args) (result) ((....))
において result がないということは
最後の演算であるということとした。
最後の演算を anchor とした。
いまのところ result がないのは
* 二項比較子
 (:= (a0 a1) () ((....
* pop
 (:pop (3) () ((....
* app (そもそも結果がないし形式も違う)
 (:app func (f0 a0 a1))

なので、anchor を上記定義にするのは問題なし。
(意味的にどうかは見当の余地あり)

別のanchor の判断基準では
次の cps が
  ない => センチネル
  1 => 続けられる
  2 以上 => センチネル。複数のあらたなブロックの始まり
ではあるが、その基準は利用していない。

=============================================
resource-scheduler に anchor を導入した。

build-connection で最後に登録された演算を anchor とし
全体のノードから引くことにした。anchor は位置として
変わらないので、scheduler からとれる nodes に anchor が
入っていなくとも意味的には矛盾もなく、処理も問題ない。

cps-block-analyzer の結果には anchor は現れない。
cps-reorder で復元するときに特に矛盾はない。

anchor はあまり素晴らしいアイデアではない。
    schedule のヒントになるように successors には
        anchor ノードが入ることがある。
        (レジスタを見て依存関係があれば successors に入る)
    しかし、入れ替えの対象にはならない。

=============================================
cps-block-analyzer では block の終了を発見したら
それ以降の walk をやめる。

一方、reorder の方では block の終了を見つけたときには
すでにすべての置き換えるべき演算は pop している
ので assert にした。
=============================================
すべての insn をチェックし発火可能なものを見つける。
ある重みづけをして優先する insn を決定
    連結するインストラクション深さが長い
    関連するレジスタが多い
    出現順
insn を発火する
リソースを埋める
時間経過によるリソース開放を反映
発火可能なものがあれば最初に戻る
すべておわっていないなら
    発火可能なものがでてくるまで順に
    時間経過によるリソース開放を反映
発火可能なものが出てきたので最初に戻る

リソースは何か？
    レジスタ(無限にあるけど)
    ACC
    memory
