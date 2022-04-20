#lang scheme
;Implementación del TDA Jugador
;Representación: Nombre del Jugador 


(define player (lambda (name)
    (if (string? name))
        name
        null))

