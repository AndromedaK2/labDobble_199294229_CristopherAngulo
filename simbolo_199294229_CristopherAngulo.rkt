#lang scheme
;Implementación del TDA simbolo
;Representación: String

;Constructores
(define (element value)
                         (if (or (string? value) (number? value))
                             value
                             null))

