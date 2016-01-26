((:FIXH
  ((MUL3X3-I (|zym2| J0 I a b)
    (:FIXS ((|zym3| (|zym4|) (:APP |zym2| (|zym4|))))
     (:= (I 3) NIL
      ((:APP |zym3| (:|#T|))
       (:+ (I 1) (|zym9|)
        ((:FIXS
          ((|zym7| (|zym8|)
            (:FIXS ((|zym5| (|zym6|) (:APP |zym3| (|zym6|))))
             (:APP MUL3X3-I (|zym5| J0 |zym9|)))))
          (:APP MUL3X3-J (|zym7| J0 I))))))))))

  (:+ (a b) (c) ((:* (d e) (f) ((:+ (g h) (i) ( (:* (j k) (l) ((:+ (m n) (o) ((:+ (c f) (p) ((:+ (i l) (q) ((:* (o p) (r) ((:* (r q) (s) ((:+ (a s) (t) ((:APP EXIT (t))))))))))))))))))))))))
