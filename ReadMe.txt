2016年 1月 19日 火曜日 21:35:29 JST
    cps-transfer  決着
    eta-reduction 決着
    k-transfer 決着

    cps-interp で動作させる
        => cps callback をつくるべき
    結果として NEON を含む ARM のオブジェクトを生成し
    ARM Linux で動かすのがここの目標。

    cps-interp の closure
        fix がきたら えせ closure を作って table に登録している
        ちゃんと自由変数を見ていない=>bug.
        closure 変換をしているわけではない
        fix が来たらちゃんと自由変数をみて
        closure を作るのが closure 
        いまは fix の引数をつくっているだけ。

----------------------------------------------------------------
ISSUES
copy-tree/list はなるべく使わない方がよい。
    copy-tree/list に対して書き換える関数が定義されていると
    copy された瞬間にリンクを失う。
    例えば
        ... (:+ (<ARG0> <ARG1>) (<RV>) (....))) ...
        があり <ARG0> を置き換える関数があったとすると
        こぴされて
        ... (:+ (<ARG0> <ARG1>) (<RV>) (....))) ...
        と構成は同じものができるが先の関数はコピー前のリストに対する
        変換関数であるため。
        実際に let-transfer で問題が発生した
        let-transfer の延長で fix-transferがよばれ
        fix-transfer がコピー(copy-tree)してしまったために
        let-transfer の変数置き換えが機能しなかった。
        いまは fix-transfer の最後で copy するのをやめた

  copy-tree は必須
    `(a b c) は必要ないが
    `(a (b c)) は (b c) が共通になる
    `(a ,`(b c)) => okそう
    `(a (,b c))  => okそう
    clisp にバグ(?)があるようだ。
    `(a (,b 0))
　　定数が入ったている時に
    b/-> 0/nil
    の 0/nil を共有する。仕様を確認すると
　　`((,a b)...
     は (append (list a) (list 'b) nil) である。あるいは
     (append (list a) (list 'b)) あるいは
     (append (list a) '(b)) あるいは
     (cons a '(b)) あるいは
     (cons a (list 'b)) あるいは
     (cons a '(b))
     となっている。
     (append (list a) '(b)) が選択された場合
     '(b) を共有することになる。実装依存ということか？
     sbcl でも clisp でもそうだった。
     [1]> (defun f (a) `(a (,a z)))
     F
     [2]> (setf x0 (f 'x0))
     (A (X0 Z))
     [3]> (setf x1 (f 'x1))
     (A (X1 Z))
     [4]> (eq (cdadr x0) (cdadr x1))
     T
     なお、scheme では規定されていない模様。
     そうなるとますます copy-tree は必須。

scheme の複雑な closure にちゃんと対応しているか疑問
  closure の覚書参照の事。
　いまは fix で closure の元を env に関数名と共にポイントして置き
  app で copy-list で環境をコピーしている。
  copy-list であるため、情報は共有する。

scheme には /= がなかった
    mini-scheme では
    set! とかができないが
        set! は構文糖衣でできそうな気がする。
    なにを最小構成にするのかは scheme の規約で確認すべき。
    本格的な scheme を目指す。

define はスコープ破り
set! はスコープ破り(現時点の mini scheme は set! には対応していないが)
define は
    グローバル変数に対応しているのか？
    レジスタに対応しているのか？

closure ？イギリス的には clousure かも。

IDEA
    (let ((io (heap-io 0xc0000000 0x1000)))
      (fixh ((get-uart-status () (record-ref io 0))
             (putc (c) (record-set! io 1 c))
             (getc () (record-ref io 1))
             (puts0 (str pos)
                    (let ((c (record-ref str pos)))
                      (if (= c 0)
                        :#t
                        (begin
                          (putc c)
                          (puts (str (+ pos 1)))))))
             (puts (str) (puts0 str 0))))
        ....
        ....
        (let ((hello (heap "Hello")))
            (puts hello)))
        ....

コンパイル時に登録された io を変換。
どうよ？

IDEA その２
    シンタックス・チェック用の汎用的なマクロ化関数を作るべき？
    現時点では cps になった時点でシンタックスに間違いがない
    という仮定をしている。これはこれでよい（スピードが上がるから)
    scheme 側でシンタックスを見るべき。
    デバッグ用にシンタックスをみる。
    シンタックス・チェック言語って
        構文定義を入れたら、そのコールバックが呼ばれる
        トップ関数をつくる関数だよね。

        なんかどっかにありそうなんだけど、、、

----------------------------------------------------------------
----------------------------------------------------------------
CPS についての覚書(すぐ忘れるから)

(:+ (1 3) (x) ((:+ (x 4) (rv) ((....)))))
              ^^              ^^
に括弧が多い気がするがこれでよい。
CPS は 
(PRIMSYM (OP*) (ID*) (CPS*))
となっている。OP が比較でないとき(+ などのとき)
は 
(PRIMSYM (OP*) (ID*) (CPS))
                     ^^^^^
となりCPS はひとつ。CPS の外側に() が余計にある。
比較の時は
(PRIMSYM (OP*) (ID*) (CPS0 CPS1))
                     ^^^^^^^^^^^
となり CPS は２つ。比較の時とそうでない時で
統一するために括弧が一つ多い。

----------------------------------------------------------------
closure についての覚書(すぐ忘れるから)

FIX を評価した時に (scheme なら define を評価した時に)
その時点の自由変数を捕まえて closure のひな形としてセーブする。
コンパイラでないのならその時点の環境保存して関数名に関連付ける。
(cps-interp ではそうしている)
コンパイラなら真面目に自由変数だけ(上のだと全環境のになるので)
を選び出しておく

たとえば csi で
(let ((x 3)) (define (f0 y) x) (set! x 5) (f0 3))
を評価すると 答えは 5 になる。つまり(set! x 5) はクロージャーに
影響している。f0 を実行されたときにコピーされる。
 (let ((x 3)) (define (f0 y) (let ((rv x)) (set! x y) rv )) (set! x 5) (f0 3
 ))
 これも答えは 5
二回目の f0 はどうなるか？
答えは 3 。
つまり f は共通のクローじゃを使う。
(let ((x 3)) (define (f0 y) (let ((rv x)) (set! x y) rv )) (set! x 5) (f0 3 ) (set! x 10) (f0 4))
これは 10 になる。つまり set! でみているのも同じ x
だから
 (let ((x 3)) (define (f0 y) (let ((rv x)) (set! x y) rv )) (set! x 5) (f0 3 ) (set! x 10) (f0 4) x)
 これは 4 になる

ところが次のように define を複数回通すと状況は違ってくる。
この場合は実行するたびに define の階層が違うのでちがう f0 を生成する。

(define (f a b n)
  (if (= n 0)
    a
    (let ((x '(b)) (z 2))
      (define (f0 y)
        (if (symbolp y) (set! x (cons x y)))
        (set! b (+ b 4))
        (list a b x z))
      (set! a (cons f0 a))
      (f a (+ b b) (- n 1)))))

(define x (f '() 2 2))

----------------------------------------------------------------
----------------------------------------------------------------
ほんとの ReadMe.txt のはじまり

----------------------------------------------------------------
とりあえず自分の CPS の勉強用に作る。

まずは lisp インタプリタを作り、
cps インタプリタを作り、
その後 cps 変換器をつくる。
「コンパイラ作成実験資料」という 1996 年のとある大学の 実習(?)資料を
元に作っている。とはいえ、勝手な解釈をして変更もしている。

----------------------------------------------------------------

lisp-v1 普通に作ったらダイナミックスコープになってしまった。
lisp-v2 クロージャを入れた。
        v1 と v2 で環境(*env* 本当は context)の形式が違うのに注意
        v1 は (a b) で v2 が (a . b)

----------------------------------------------------------------
cps-interp: cps の インタプリタ
    クロージャーに対応したつもり。
    テストが不十分
    cps 変換が出来たら検証用に必要だと思って作った。
    がいまだ必要とされず。

cps-transfer-naive: 効率の悪いバージョンかつ let および define を実装せず
    ある程度で来た
        -> if が重複を作る
        -> id がいっぱいできる
        -> let の実装がない
    とはいえここまでで終わり。
    lisp のマクロの使い方がおかしい。
    後を cps-transfer に引き継ぐ。

    fix も 関数内の cps が 1 cps と限定的。
    なお lisp の fix は begin を意味的に含むが
    cps の方は begin を含まない(やや混乱気味)
    heap もないや、、、
    だいぶあやしい
    let がない。
    id がいっぱいできる。
    lisp のマクロを理解していないころのものなので不必要な copy-tree が
    随所にある。

cps-transfer:
    let はトリッキーだがうまくいっている模様
    だいたいで来た
    cps-transfer/ReadMe.txt を参照

eta-reduction:
    ちゃんと walk-cps して確認している。
    関数 check-eta-reduction をより厳密にチェックすべきか？
        optimize.lisp は walk-cps していないのでチェックが厳しい。
        walk-cps しているので cps の構文が崩れていないという前提に
        たてば今のままでよいはず。つまり optimize.lisp はいらない。
    簡単バージョン(n-reduction) を発展させて完成させた。
    これはこれでおしまい。
    関数 n-reduction は削除してよい。

rec-opt
    ;'(FB (0 1) (1 1) (N (+ (FB (- n 1)) (FB (- n 2)))))
    の最適化
    FIBO に似た形の関数を最適化して末尾呼び出しにしようという試み
    アイデアだけ

