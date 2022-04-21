#lang scheme
(require "mazo_19929422_CristopherAngulo.rkt")
(require "modo_19929422_CristopherAngulo.rkt")
(provide game)
;Implementación del TDA Juego
;Representación: numPlayers(int) X cardsSet X mode (fn) X rndFn (fn)


;Constructor
;Dominio: Número de Jugadores X Mazo de Cartas X Modo de Juego X Función Random
;Recorrido: juego (lista)
;Descripción: Función que crea el juego
(define game (lambda (numPlayers cardsSet mode rndFn)
 (if (and (number? numPlayers) (dobble? cardsSet))
     (list numPlayers (list "mazo de cartas" cardsSet) mode  rndFn)
      null)))



(define numPlayers 2)
(define elements (list  1 2 3 4 5 6 7 8 9 10 11 12 13 ))
(define dobbleCards (cardsSet elements 4 13 3))

;(define game1  (game numPlayers dobbleCards "stackMode" (lambda (x) (* x x))))

(define game1  (game numPlayers dobbleCards stackMode  2))
;ejemplo de ejecución de juego
 game1 


