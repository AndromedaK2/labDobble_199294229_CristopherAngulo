#lang scheme
(require "simbolo_199294229_CristopherAngulo.rkt")
(provide cardsSet)
;Implementación del TDA mazo
;Representación:  Lista de Cartas

(define elements (list  1 2 3 4 5 6 7 8 9 10 11 12 13))

;Constructor
;Dominio: elementos X número de elementos (number) X máximo número de cartas (number)
;Recorrido: mazo de cartas
;Descripción: retorna un mazo de cartas
(define cardsSet (lambda (elements numE maxC rndFn)
    (if (and (>= numE 1)(>= maxC 1) (isValidOrder (- numE 1)) (elements? elements))
        (createValidCardsSet elements numE maxC)
        emptyCardsSet
    ))
)


;Constructor
;Dominio: elementos X numero de elementos X maximo de cartas 
;Recorrido: mazo de cartas
;Descripción: mazo de cartas válido
(define createValidCardsSet (lambda (elements numE maxC)
    (createLastNCards elements
      (createNextNCards elements
        (addCardToDeck emptyCardsSet (createFirstCard elements numE))
      (- numE 1) maxC 0) 
    (- numE 1) maxC 1))
)
                              
;Constructor
;Dominio: no recibe parámetros
;Recorrido: lista vacia
;Descripción:  retorna una lista vacía
(define emptyCardsSet null)

;Pertenencia
;Dominio: mazo de cartas
;Recorrido: true | false
;Descripción
(define dobble? (lambda (cardsSet)
    (if (null? cardsSet)
        #true
        #false
    )
))


;Selector
;Dominio: mazo de cartas
;Recorrido: cantidad de cartas (number)
;Descripción retorna la cantidad de cartas del mazo
(define numCards (lambda (cardsSet) (length cardsSet)))


;Selector
;Dominio: mazo de cartas X posición
;Recorrido: carta
;Descripción retorna la carta solicitada
(define nthCard (lambda (cardsSet position)(list-ref cardsSet position)))         


;Selector
;Dominio: carta
;Recorrido: cantidad total de cartas (number)
;Descripción retorna cantidad total de cartas
(define findTotalCards (lambda (card)(+ (* (- ( length card) 1) (- ( length card) 1))  (- ( length card) 1) +1)))                        


;Selector
;Dominio: mazo de cartas
;Recorrido: carta
;Descripción retorna cantidad de elementos necesarios para construir el mazo
(define requiredElements (lambda(card)(+ (* (- ( length card) 1) (- ( length card) 1))  (- ( length card) 1) +1)))
                           


;Selector
;Dominio
;Recorrido
;Descripción
;(define missingCards )

                           
;Modificador
;Dominio: Mazo de Cartas X Carta
;Recorrido: Mazo de Cartas
(define addCardToDeck (lambda (cardsSet newCard )
                        (if (null? cardsSet)
                            (append (list newCard))                      
                            (append  cardsSet (list newCard)))))

;Otros
;Dominio: orden del mazo
;Recorrido: true | false
;Descripción: 
(define isValidOrder(lambda (order)
     (if (= order 1) #false 
     (if (isPrimeWrapper order)
         #true
         #false ))))

;Otros
;Dominio:
;Recorrido:
;Descripción:
(define isPrimeWrapper (lambda (order)(isPrime order 2)))

;Otros 
;Dominio: orden del mazo (number) X contador (number)
;Recorrido: true | false
;Descripción: Recursión de Cola que retorna un true cuando el valor es primo
(define isPrime ( lambda (order count)
     (if (= order count) #true
     (if (= (remainder order count) 0) #false
     (isPrime order (+ count 1))))))

;Otros
;Dominio: orden del mazo (number) X auxiliar j (number) X auxiliar i (number X auxiliar k (number)
;Recorrido: posición (number)
;Descripción: calcular el valor de la posición que deseamos sacar del conjunto de elementos
(define calculateValueToDrawACard (lambda (n j i k) (- (+(+ n 2) (* n (- k 1)) (modulo(+(*(- i 1)(- k 1))(- j 1))n)) 1)))


;Dominio: elementos X Cantidad de elementos por carta
;Recorrido: Carta
;Tipo de Recursión: Recursión Natural
(define createFirstCard (lambda (elements n)
                        (if (null? elements)
                            null
                        (if (<= n 1)
                            (cons (getFirstElement elements) null)
                            (cons (getFirstElement elements) (createFirstCard (getTailElements elements) (- n 1)))))))


;Dominio: elementos X posición (number)
;Recorrido: Simbolo
;Descripción: Obtener el elemento de la posición dada
(define getElementByPosition (lambda (elements position)
                    (list-ref elements position)))
                              

;Dominio: Mazo de Cartas X Carta X Cantidad de Elementos por Carta X Cantidad total de Cartas X Auxiliar Entero X Auxiliar Entero
;Recorrido: Carta
;Tipo de Recursión: Recursión de Cola
(define auxiliarNextNCards (lambda (elements card n countTotalCards j k )
                             (if (= n k)
                                 card
                                 (auxiliarNextNCards elements (append card (list (getElementByPosition elements (+(* n j)(+ k 1)))))                                                     
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

;Otros:
;Dominio:
;Recorrido:
;Descripción:
(define secondAuxiliarCreateLastNCards (lambda (elements card n countTotalCards j i k)
                                         (if (> k n)
                                             card
                                             (secondAuxiliarCreateLastNCards elements
                                              (append card (list (getElementByPosition elements (calculateValueToDrawACard n j i k ))))
                                               n countTotalCards j i (+ k 1)))))

;Otros:
;Dominio:
;Recorrido:
;Descripción:
(define firstAuxiliarCreateLastNCards (lambda ( elements cardsSet n countTotalCards j i )
                                        (if (> j n)
                                            cardsSet
                                            (firstAuxiliarCreateLastNCards elements (addCardToDeck cardsSet
                                              (secondAuxiliarCreateLastNCards elements (list (getElementByPosition elements i))
                                                n countTotalCards j i 1))                                                                                                           
                                                n countTotalCards (+ j 1) i))))
;Otros:
;Dominio:
;Recorrido:
;Descripción:
(define createLastNCards (lambda (elements cardsSet n countTotalCards i)
                           (if (> i n)
                               cardsSet
                               (createLastNCards elements
                                  (firstAuxiliarCreateLastNCards elements cardsSet n countTotalCards 1 i)
                                                 n countTotalCards (+ i 1)))))  

;Elementos de ejemplo
(define elementoss (list (element "A") (element 2) (element "D") (element "C") (element "3") (element 8) (element 10)))
;Mazo de carta de ejemplo
(cardsSet elementoss 3 7 3)
;Cantidad de cartas del mazo
(numCards (cardsSet elementoss 3 7 3) )
;Retornar Carta
(nthCard (cardsSet elementoss 3 7 3) 2 )
;Encontrar total de cartas
(findTotalCards (nthCard (cardsSet elementoss 3 7 3) 2 ))




