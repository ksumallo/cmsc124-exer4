(define (subtractMatrices l1 l2)
    (cond 
        [(or (null? l1) (null? l2)) `()]
        [else (cons (- (car l1) (car l2)) (subtractMatrices (cdr l1) (cdr l2)))]
    )
)

(define (get L i)
    (if (= i 0)
        (car L)
        (get (cdr L) (- i 1))
    )
)

(define (len L i)
    (if (null? L)
        i          ;; return ([Even] . [Odd])
        (len (cdr L) (+ i 1))
    )
)

(define (separate L E O)
    (if (null? (car L))
        (cons E O)
        (if (= (modulo (car (car L)) 2) 0)
            (separate (cdr L) (append E `(car (car L))) O) ;; Even
            (separate (cdr L) E (append O `(car (car L))))  ;; Odd
        ) 
    )
)

(define (sortOddEven L)
    (define mylist (list 1 2 3 4 5))
    (separate mylist (list) (list))
)