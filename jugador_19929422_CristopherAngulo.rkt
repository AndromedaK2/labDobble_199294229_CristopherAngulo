#lang scheme
(require "carta_19929422_CristopherAngulo.rkt")
(provide player? player)
;Implementación del TDA Jugador
;Representación: Nombre del Jugador X Cartas



;Constructor
;Dominio: nombre del jugador X Cartas x Puntaje
;Recorrido: jugador
(define player (lambda (name)
    (if (and (string? name))
        (list name initialCards initialPoints)
        null)))




(define initialCards (list))
(define initialPoints 0)


(define player? (lambda (name)
            (if (string? name)
                #true
                #false)))



;(define player1 (player "cristopher"  (list "cards" (list 1 2 3 4) (list 1 5 6 7)) 2))
(define player2 (player "cristian"))
;player2