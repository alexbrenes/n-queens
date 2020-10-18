;; "\u2655" "♕"
;; "\u25A0" "■"
;; "\u25A1" "□"

;; Dominio: Un valor natural n y un símbolo e.
;; Codominio: Una lista con n veces el símbolo e.

(define repeat
  (lambda (n e)
    (cond ((= n 0) '())
          (#t (append (list e) (repeat (- n 1) e))))))

;; Dominio: Una lista l, un número natural a y un símbolo b.
;; Codominio: Una lista con el elemento b en la posición a de la lista.

(define replace-at
  (lambda (l a b)
    (cond ((= a 0) (append (list b) (cdr l)))
          (#t (append (list (car l)) (replace-at (cdr l) (- a 1) b))))))

;; Dominio: Una lista l y un número natural k.
;; Codominio: El elemento k-ésimo de la lista l.

(define element-at
  (lambda (l k)
    (cond ((<= k 0) (car l))
          (#t (element-at (cdr l) (- k 1))))))

;; Dominio: Una lista de números desde 0 hasta a-1; dos número naturales i y j menores o iguales al tamaño de la lista.
;; Codominio: #<void>.

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

;; Dominio: Una lista de números naturales desde 0 hasta a - 1.
;; Codominio: #<void>.

(define print-solution
  (lambda (solution)
    (print-solution-aux solution 0 0)))


;; Dominio: Dos índices j e index y una lista de las posiciones de las reinas en el tablero.
;; Codominio: Verdadero o falso, indica si la posición index de la reina es válida.

(define evaluate-aux
  (lambda (j index table)
    (cond ((= index j) #t)
          ((equal? (element-at table j) (element-at table index)) #f)
          ((equal? (abs (- j index)) (abs (- (element-at table j) (element-at table index)))) #f)
          (#t (evaluate-aux (+ j 1) index table)))))

;; Dominio: Un índice index que indica la posición en la que se pretende colocar la reina y la lista con las otras reinas sobre el tablero.
;; Codominio: Verdadero o falso, indica si la posición index de la reina es válida.

(define evaluate
  (lambda (index table)
    (evaluate-aux 0 index table)))

;; Dominio: Una lista de las posiciones de las reinas sobre el tablero, dos índices que indican la posible posición de una nueva reina en el tablero, el tamaño del tablero a, la cantidad soluciones requeridas b y la lista de soluciones encontradas. 
;; Codominio: Una lista de soluciones donde: length(solutions) <= b.

(define a-reinas
  (lambda (table index row a  b solutions)
    (cond ((>= (length solutions) b) solutions)
          ((= a index) (append solutions (list table))) ;; Solución
          ((= a row) solutions) ;; Se evaluaron todas las filas
          ((evaluate index (replace-at table index row)) (a-reinas table index (+ row 1) a b
                                                                   (a-reinas (replace-at table index row) (+ index 1) 0 a b solutions)))
          (#t (a-reinas table index (+ row 1) a b solutions)))))

;; Dominio: Un tamaño del tablero o cantidad de reinas a, con a > 4, siendo a un número natural. Además, una cantidad b de soluciones deseadas.
;; Codominio: Una lista de soluciones donde: length(solutions) <= b.

(define backtrackingNReinas
  (lambda (a b)
    (for-each print-solution (a-reinas (repeat a 0) 0 0 a b '()))))
