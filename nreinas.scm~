;; "\u2655" "♕"
;; "\u25A0" "■"
;; "\u25A1" "□"

(define repeat
  (lambda (n e)
    (cond ((= n 0) '())
          (#t (append (list e) (repeat (- n 1) e))))))

(define replace-at
  (lambda (l a b)
    (cond ((= a 0) (append (list b) (cdr l)))
          (#t (append (list (car l)) (replace-at (cdr l) (- a 1) b))))))

(define element-at
  (lambda (l k)
    (cond ((<= k 0) (car l))
          (#t (element-at (cdr l) (- k 1))))))

(define print-solution-aux
  (lambda (solution i j)
    (cond ((= j (length solution))
           (display "\n")
           (cond ((and (= i (- (length solution) 1)) (= j (+ i 1))) (display "\n"))
                 (#t (print-solution-aux solution (+ i 1) 0))))
           (#t (display (cond ((equal? (element-at solution j) i) "\u2655 ")
                              ((even? (+ j i)) "\u25A1 ")
                              (#t "\u25A0 ")))
               (print-solution-aux solution i (+ j 1))))))

(define print-solution
  (lambda (solution)
    (print-solution-aux solution 0 0)))



(define evaluate-aux
  (lambda (j index table)
    (cond ((= index j) #t)
          ((equal? (element-at table j) (element-at table index)) #f)
          ((equal? (abs (- j index)) (abs (- (element-at table j) (element-at table index)))) #f)
          (#t (evaluate-aux (+ j 1) index table)))))
(define evaluate
  (lambda (index table)
    (evaluate-aux 0 index table)))

(define a-reinas
  (lambda (table index row a)
    (cond ((= a index) (list table)) ;; Solución
          ((= a row) '()) ;; Se evaluaron todas las filas
          ((evaluate index (replace-at table index row)) (append (a-reinas (replace-at table index row) (+ index 1) 0 a)
                                                                 (a-reinas table index (+ row 1) a)))
          (#t (a-reinas table index (+ row 1) a))
          )))
(for-each print-solution '((1 3 0 2) (2 0 3 1)))
(define backtrackingNReinas
  (lambda (a b)
    '()))