#lang scheme
;Implementación del TDA simbolo
;Representación: String

;Constructores
(define (simbolo value)
                         (if (string? value)
                             value
                             null))

