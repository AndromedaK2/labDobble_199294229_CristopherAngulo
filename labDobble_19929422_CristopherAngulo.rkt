#lang scheme
(require "carta_19929422_CristopherAngulo.rkt")
(require "simbolo_19929422_CristopherAngulo.rkt")
(require "mazo_19929422_CristopherAngulo.rkt")
(require "jugador_19929422_CristopherAngulo.rkt")


(define m 22)
(define a 15)
(define c 38)

(define randomFn (lambda (xn)(modulo (+ (* a xn) c) m)))

;Elementos de ejemplo
;(define elements (list  1 2 3 4 5 6 7 8 9 10 11 12 13))
;(define elementoss (list (element "A") (element 2) (element "D") (element "C") (element "3") (element 8) (element 10)))
(define elements (list  1 2 3 4 5 6 7 8 9 10 11 12 13))
(define elementoss (list (element "A") (element 2) (element "D") (element "C") (element "3") (element 8) (element 10)))
;Mazo de carta de ejemplo
(cardsSet elementoss 3 -1 randomFn)
(cardsSet elementoss 3 7  randomFn)
(cardsSet elements   4 13 randomFn)
(cardsSet elements   4 10 randomFn)
;Cantidad de cartas del mazo
(numCards (cardsSet elementoss 3 6 randomFn) )
(numCards (cardsSet elementoss 3 7 randomFn) )
(numCards (cardsSet elementoss 3 5 randomFn) )
;Retornar Carta
(nthCard (cardsSet elements  3 7 randomFn) 2 )
(nthCard (cardsSet elements  4 13 randomFn) 3 )
(nthCard (cardsSet elements  4 10 randomFn) 1 )
;Encontrar total de cartas
(findTotalCards (nthCard (cardsSet elements 4 13 randomFn) 2) )
(findTotalCards (nthCard (cardsSet elementoss 3 7 randomFn) 2) )
(findTotalCards (nthCard (cardsSet elementoss 3 6 randomFn) 2) )
;el juego es valido dobble
(dobble? (cardsSet elements 4 13 randomFn)) 
(dobble? (cardsSet elements 3 7 randomFn)) 
(dobble? (cardsSet elements 2 3 randomFn)) 
;Cartas Faltantes
(missingCards (cardsSet elementoss 3 5 randomFn) elementoss randomFn) 
(missingCards (cardsSet elementoss 3 6 randomFn) elementoss randomFn) 
(missingCards (cardsSet elementoss 3 7 randomFn) elementoss randomFn) 
;Mostrar las cartas del mazo
(display (cardsSet->string (cardsSet elements 4 13 randomFn)))
(display (cardsSet->string (cardsSet elements 3 7 randomFn)))  
(display (cardsSet->string (cardsSet elements 3 5 randomFn)))  




