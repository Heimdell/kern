
; function
(define [((compose f) g) x]
  (f (g x)))

; list
('list 1 2 3)

; splice
'(foo ,qwqe . ,asdad)