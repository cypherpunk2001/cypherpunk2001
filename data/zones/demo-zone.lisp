(:id :demo
 :chunk-size 8
 :width 8
 :height 8
 :layers
 ((:id :floor :collision nil
   :chunks ((:x 0 :y 0 :fill 0)))
  (:id :walls :collision t
   :chunks
   ((:x 0 :y 0
     :fill 0
     :overrides
     ((0 0 1) (1 0 1) (2 0 1) (3 0 1) (4 0 1) (5 0 1) (6 0 1) (7 0 1)
      (0 1 1) (7 1 1)
      (0 2 1) (7 2 1)
      (0 3 1) (7 3 1)
      (0 4 1) (7 4 1)
      (0 5 1) (7 5 1)
      (0 6 1) (7 6 1)
      (0 7 1) (1 7 1) (2 7 1) (3 7 1) (4 7 1) (5 7 1) (6 7 1) (7 7 1))))))
 :objects
 ((:id :spawn :x 3 :y 3)))
