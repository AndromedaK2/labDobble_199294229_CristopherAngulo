#lang scheme
(require "carta_19929422_CristopherAngulo.rkt")
;Implementación del TDA Jugador
;Representación: Nombre del Jugador X Cartas



;Constructor
;Dominio: nombre del jugador X Cartas
;Recorrido: jugador
(define player (lambda (name cards)
    (if (and (string? name)(cards? cards))
        (list name cards)
        null)))







(define player1 (player "cristopher"  (list "cards" (list 1 2 3 4) (list 1 5 6 7))))

player1