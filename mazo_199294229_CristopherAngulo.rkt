#lang scheme
(require "simbolo_199294229_CristopherAngulo.rkt")
(provide cardsSet)
;Implementación del TDA mazo
;Representación:  Lista de Cartas

(define elements (list  1 2 3 4 5 6 7 8 9 10 11 12 13))

;Constructor
;Dominio: elementos 
;Recorrido: mazo de cartas
;Descripción: retorna un mazo de cartas
(define cardsSet (lambda (elements numE maxC rndFn)
    (if (and (>= numE 1)(>= maxC 1) (isValidOrder (- numE 1)) (elements? elements))
        (createValidCardsSet elements numE maxC)
        emptyCardsSet
    ))
)


;Constructor
;Dominio: elements X numE(int) X maxC(int)
;Recorrido: mazo de cartas
;Descripción: mazo de cartas válido
(define createValidCardsSet (lambda (elements numE maxC)
    (createLastNCards elements
      (createNextNCards elements
         (addCardToDeck emptyCardsSet (createFirstCard elements numE)) (- numE 1) maxC 0) (- numE 1) maxC 1)))
                              
;Constructor
;Dominio: no recibe parámetros
;Recorrido: lista vacia
;Descripción:  retorna una lista vacía
(define emptyCardsSet null )

;Pertenencia
;Dominio: cardsSet
;Recorrido: #true | #false
;Descripción
(define dobble? (lambda (cardsSet)
    (if (null? cardsSet)
        #true
        #false
    )
))


;Selectores

;Modificador
;Dominio: Mazo de Cartas X Carta
;Recorrido: Mazo de Cartas
(define addCardToDeck (lambda (cardSet newCard )
                        (if (null? cardSet)
                            (append (list newCard))                      
                            (append  cardSet (list newCard)))))


;Otros:
;Dominio:
;Recorrido:
;Descripción:
(define isValidOrder(lambda (order)
     (if (= order 1) #f 
     (if (isPrimeWrapper order)
         #true
         #false 
     ))
))

;Otros:
;Dominio:
;Recorrido:
;Descripción:
(define isPrimeWrapper (lambda (order)    
 (isPrime order 2)
))

;Otros:
;Dominio:
;Recorrido:
;Descripción:

(define isPrime ( lambda (order count)
     (if (= order count) #t
     (if (= (remainder order count) 0) #f
     (isPrime order (+ count 1))))))

(define calculateValueToDrawACard (lambda (n j i k) (- (+(+ n 2) (* n (- k 1)) (modulo(+(*(- i 1)(- k 1))(- j 1))n)) 1)))

; ejemplo de uso (isPrimeWrapper 4)
;(define my-list '(1 2 3 4 ))
;(cardsSet my-list 4 13 2)


;Dominio: Simbolos X Cantidad de Simbolos por carta
;Recorrido: Carta
;Tipo de Recursión: Recursión Natural
(define createFirstCard (lambda (elements n)
                        (if (null? elements)
                            null
                        (if (<= n 1)
                            (cons (getFirstElement elements) null)
                            (cons (getFirstElement elements) (createFirstCard (getTailElements elements) (- n 1)))))))

;(define firstCard (createFirstCard elements 4))

;Dominio: Mazo de Cartas X posición
;Recorrido: Simbolo
;Descripción: Obtener el Simbolo de la posición entragada en el mazo de cartas
(define getSymbolByPosition (lambda (cardsSet position)
                    (list-ref cardsSet position)))
                              

;Dominio: Mazo de Cartas X Carta X Cantidad de Elementos por Carta X Cantidad total de Cartas X Auxiliar Entero X Auxiliar Entero
;Recorrido: Carta
;Tipo de Recursión: Recursión de Cola
(define auxiliarNextNCards (lambda (cardsSet card n countTotalCards j k )
                             (if (= n k)
                                 card
                                 (auxiliarNextNCards cardsSet (append card (list (getSymbolByPosition cardsSet (+(* n j)(+ k 1)))))                                                     
                                                                n countTotalCards j (+ k 1) ))))
                                 
;Dominio: Lista de Simbolos x Cantidad de Simbolos por carta x cantidad total de cartas a generar 
;Recorrido: Mazo de las N  Cartas
;Tipo de recursión: Recursión de Cola
(define createNextNCards (lambda (elements cardsSet n countTotalCards j)
                           (if (= n j )
                               cardsSet
                               (createNextNCards elements
                                (addCardToDeck cardsSet (auxiliarNextNCards elements (list(getFirstElement elements)) n countTotalCards (+ j 1) 0 ))
                                 n countTotalCards (+ j 1)))))

;(addCardToDeck (addCardToDeck null firstCard) firstCard)

;Otros:
;Dominio:
;Recorrido:
;Descripción:
(define secondAuxiliarCreateLastNCards (lambda (elements card n countTotalCards j i k)
                                         (if (> k n)
                                             card
                                             (secondAuxiliarCreateLastNCards elements
                                              (append card (list (getSymbolByPosition elements (calculateValueToDrawACard n j i k ))))
                                               n countTotalCards j i (+ k 1)))))

;Otros:
;Dominio:
;Recorrido:
;Descripción:
(define firstAuxiliarCreateLastNCards (lambda ( elements cardsSet n countTotalCards j i )
                                        (if (> j n)
                                            cardsSet
                                            (firstAuxiliarCreateLastNCards elements (addCardToDeck cardsSet
                                              (secondAuxiliarCreateLastNCards elements (list (getSymbolByPosition elements i))
                                                n countTotalCards j i 1))                                                                                                           
                                                n countTotalCards (+ j 1) i))))

;Otros:
;Dominio:
;Recorrido:
;Descripción:
(define createLastNCards (lambda (elements cardsSet n countTotalCards i)
                           (if (> i n)
                               cardsSet
                               (createLastNCards elements (firstAuxiliarCreateLastNCards elements cardsSet n countTotalCards 1 i)
                                                 n countTotalCards (+ i 1)))))  





(define elementoss (list (element "A") (element 2) (element "D") (element "C") (element "3") (element 8) (element 10)))
(cardsSet elementoss 3 7 3)






