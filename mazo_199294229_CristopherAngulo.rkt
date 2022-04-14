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

;Dominio: Mazo de Cartas X Carta
;Recorrido: Mazo de Cartas
(define addCardToDeck (lambda (cardSet newCard )
                        (if (null? cardSet)
                            (append (list newCard))                      
                            (append  cardSet (list newCard)))))


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



(define elements (list  1 2 3 4 5 6 7 8 9 10 11 12 13))

;Dominio: Simbolos X Cantidad de Simbolos por carta
;Recorrido: Carta
;Tipo de Recursión: Recursión Natural
(define createFirstCard (lambda (elements countElementPerCard)
                        (if (null? elements)
                            null
                        (if (<= countElementPerCard 1)
                            (cons (car elements) null)
                            (cons (car elements) (createFirstCard (cdr elements) (- countElementPerCard 1)))))))

;(define firstCard (createFirstCard elements 4))




;Dominio: Mazo de Cartas X posición
;Recorrido: Simbolo
;Descripción: Obtener el Simbolo de la posición entragada en el mazo de cartas
(define getSymbolByPosition (lambda (deck position)
                    (list-ref deck position)))
                              

;Dominio: Mazo de Cartas X Carta X Cantidad de Elementos por Carta X Cantidad total de Cartas X Auxiliar Entero X Auxiliar Entero
;Recorrido: Carta
;Tipo de Recursión: Recursión de Cola
(define auxiliarNextNCards (lambda (deck card countElementPerCard countTotalCards j k )
                             (if (= countElementPerCard k)
                                 card
                                 (auxiliarNextNCards deck (append card (list (getSymbolByPosition deck (+(* countElementPerCard j)(+ k 1)))))                                                     
                                                                countElementPerCard countTotalCards j (+ k 1) ))))
                                 
;Dominio: Lista de Simbolos x Cantidad de Simbolos por carta x cantidad total de cartas a generar 
;Recorrido: Mazo de las N  Cartas
;Tipo de recursión: Recursión de Cola
(define createNextNCards (lambda (elements deck countElementPerCard countTotalCards j)
                           (if (= countElementPerCard j )
                               deck
                               (createNextNCards elements
                                (addCardToDeck deck (auxiliarNextNCards elements (list(car elements)) countElementPerCard countTotalCards (+ j 1) 0 ))
                                 countElementPerCard countTotalCards (+ j 1)))))

;(addCardToDeck (addCardToDeck null firstCard) firstCard)
;(createNextNCards elements (list firstCard) 3 13 0) 




(define calculateValueToDrawACard (lambda (n j i k)
                           (- (+(+ n 2) (* n (- k 1)) (modulo(+(*(- i 1)(- k 1))(- j 1))n)) 1)))
                                   
                              
(define secondAuxiliarCreateLastNCards (lambda (elements card n countTotalCards j i k)
                                         (if (> k n)
                                             card
                                             (secondAuxiliarCreateLastNCards elements
                                              (append card (list (getSymbolByPosition elements (calculateValueToDrawACard n j i k ))))
                                               n countTotalCards j i (+ k 1)))))
                                            
(define firstAuxiliarCreateLastNCards (lambda ( elements deck n countTotalCards j i )
                                        (if (> j n)
                                            deck
                                            (firstAuxiliarCreateLastNCards elements (addCardToDeck deck
                                              (secondAuxiliarCreateLastNCards elements (list (getSymbolByPosition elements i))
                                                n countTotalCards j i 1))                                                                                                           
                                                n countTotalCards (+ j 1) i))))
  

(define createLastNCards (lambda (elements deck n countTotalCards i)
                           (if (> i n)
                               deck
                               (createLastNCards elements (firstAuxiliarCreateLastNCards elements deck n countTotalCards 1 i)
                                                 n countTotalCards (+ i 1)))))




(createLastNCards elements null 3 13 1)





