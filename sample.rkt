#t
#f
40
(if #t 10 20)
(if #f 400 -500)
(+ 3 4)
(* 3 2)
(equal? 4 3)
(equal? #t #f)
(equal? 3 (+ 2 1))
(< 3 4)
(< (+ 4 (+ 3 1)) 8)
(not (< 4 3))
(not #t)
(not (not (not #t)))
(and #t (< 0 1))
(and #f (< 1 (+ 1 0)))
(or #t #f)
(or (< (+ 1 0) 2) #f)
