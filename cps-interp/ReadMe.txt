test0: で string を評価できるかどうか、
できねーよ
((:exit ("good-bye") () ()))
いま cps-interp で通るようにしている。
eta-reduction ではエラーになるので注意。

test4:
なんとなくあっているきがするが (app k (r)) interp では動かない。
を評価しようとすると f にとび、f の引数の数が合わなくなる。
単純に cps の間違い。optimizer は通る(optimizer 用のcps)。
cps-interp であわないのは仕方がないとする。

((:fix ((f (k x0 x1) (:+ (x0 x1) (r) ((:app k (r))))))
      (:app f (f 3 5))))
修正すべき

test5
g が定義されていないので cps-interp では動かない。

修正すべき
