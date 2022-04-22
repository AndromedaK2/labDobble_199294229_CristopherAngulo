#lang scheme
(require "mazo_19929422_CristopherAngulo.rkt")
(provide stackMode)
;Implementación  el TDA Modos de Juego
;Representación: Función del Modo de juego(fn)


;Constructor
;Dominio: cardsSet
;Recorrido: Función de StackMode 
;Descripción: Devuelve el Modo de juego Stack
(define stackMode (lambda (cardsSet)
   (list (getFirstCard cardsSet) (getSecondCard cardsSet))) 
)



;Constructor
;Dominio: No recibe parámetros 
;Recorrido: Función de EmptyHandsMode 
;Descripción: Devuelve el Modo de juego Stack
(define emptyHandsMode (lambda(cardSet)#true ))


