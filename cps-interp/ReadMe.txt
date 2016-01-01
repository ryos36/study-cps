test0: で string を評価できるかどうか、
できねーよ
((:exit ("good-bye") () ()))

test4:
なんとなくあっているきがするが (app k (r)) interp では動かない。
を評価しようとすると f にとび、f の引数の数が合わなくなる。
単純に cps の間違い。optimizer は通る(optimizer 用のcps)。

((:fix ((f (k x0 x1) (:+ (x0 x1) (r) ((:app k (r))))))
      (:app f (f 3 5))))

