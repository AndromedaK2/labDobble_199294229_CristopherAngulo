#lang scheme
(provide cardsSet)
;Implementación del TDA mazo
;Representación:  Lista de Cartas

;Constructor
;Dominio: Elements (list) X numE(int) X maxC(int) X rndFn (fn)
;Recorrido: CardsSet
;Descripción: 
(define cardsSet (lambda (Elements numE maxC rndFn)
    (if (and (>= numE 1)(>= maxC 1) (isValidOrder (- numE 1)) (not(null? Elements)))
        #true
        #false
    ))
)

;Pertenencia
;Dominio: cardsSet
;Recorrido: #true | #false
;Descripción
(define dobble (lambda (cardsSet)
    (if (null? cardsSet)
        #true
        #false
    )
))



;Selectores


;Modificarores


;Otros
; numE - 1 -> orden del plano proyectivo
; maxC

(define isValidOrder(lambda (order)
     (if (isPrimeWrapper order)
         #true
         #false 
     ))
)

(define isPrimeWrapper (lambda (number)    
 (isPrime number 2)
))

(define isPrime ( lambda (number count)
     (if (= number count) #t
     (if (= (remainder number count) 0) #f
     (isPrime number (+ count 1))))))



; ejemplo de uso (isPrimeWrapper 4)
;(define my-list '(1 2 3 4 ))
;(cardsSet my-list 4 13 2)



(define elements (list 1 2 3 4 5 6 7 8 9 10 11 12 13))

(define createFirstCard (lambda (elements countElementPerCard)
                        (if (null? elements)
                            null
                        (if (<= countElementPerCard 1)
                            (cons (car elements) null)
                            (cons (car elements) (createFirstCard (cdr elements) (- countElementPerCard 1)))))))

;(define firstCard (createFirstCard elements 4))

(define addCardToDeck (lambda (cardSet newCard )
                        (if (null? cardSet)
                            (append (list newCard))                      
                            (append (list newCard) cardSet))))


(define getSymbolByPosition (lambda (deck position)
                    (list-ref deck position)))
                              

(define auxiliarNextNCards (lambda (deck card countElementPerCard countTotalCards j k )
                             (if (= countElementPerCard k)
                                 card
                                 (auxiliarNextNCards deck (cons (car card )  (cons getSymbolByPosition deck (+(* countElementPerCard j)(+ k 1))))                                                     
                                                                countElementPerCard countTotalCards j (+ k 1) ))))
                                 
;Dominio: Lista de Simbolos x Cantidad de Simbolos por carta x cantidad total de cartas a generar 
;Recorrido: Lista de la N Cartas
;Tipo de recursión: Recursión Natural
(define createNextNCards (lambda (deck countElementPerCard countTotalCards j)
                           (if (null? deck)
                               null
                           (if (= countElementPerCard j )
                               deck
                               (createNextNCards
                                (auxiliarNextNCards deck (car deck) countElementPerCard countTotalCards (+ j 1) 0 )
                                 countElementPerCard countTotalCards (+ j 1))))))

;(addCardToDeck (addCardToDeck null firstCard) firstCard)
;(cons (car elements) ( cons (car elements)))
(createNextNCards elements 3 13 0) 




