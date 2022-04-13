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
(define my-list '(1 2 3 4 ))
(cardsSet my-list 4 13 2)



(define elements (list 1 2 3 4 5 6 7 8 9 10 11 12 13))

(define createFirstCard (lambda (list countElementPerCard)
                          (if (null? list)
                              null
                          (if (<= countElementPerCard 1)
                              (cons (car list) null)
                              (cons (car list) (createFirstCard (cdr list) (- countElementPerCard 1)))))))

;(define firstCard (createFirstCard elements 4))

(define addCardToDeck (lambda (cardSet newCard )
                        (if (null? cardSet)
                            (append (list newCard))                      
                            (append (list newCard) cardSet))))







(define getSymbolByPosition (lambda (deck position count)
                        (if (null? deck)
                            null
                        (if (= position count)
                            (car deck)
                            (getSymbolByPosition (cdr deck) position (+ count 1))))))
                              

(define auxiliarNextNCards (lambda (deck countElementPerCard countTotalCards count1 count2 )
                             (if (null? deck)
                                 null
                             (if (= countElementPerCard count1)
                                 (cons (getSymbolByPosition deck (+(* countElementPerCard count2)(+ count1 1)) 0) null)
                                 (cons (car deck)(auxiliarNextNCards deck countElementPerCard countTotalCards (+ count1 1) (+ count2 1) ))))))
                                 
;Dominio: Lista de Simbolos x Cantidad de Simbolos por carta x cantidad total de cartas a generar 
(define createNextNCards (lambda (deck countElementPerCard countTotalCards count)
                           (if (null? deck)
                               null
                           (if (= countElementPerCard count )
                               deck
                               (createNextNCards
                                (auxiliarNextNCards deck countElementPerCard countTotalCards 1 (+ count 1))
                                 countElementPerCard countTotalCards (+ count 1))))))



;(addCardToDeck (addCardToDeck null firstCard) firstCard)
(createNextNCards elements 3 13 0) 







