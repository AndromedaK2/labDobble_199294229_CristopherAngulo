#lang scheme
(require "simbolo_19929422_CristopherAngulo.rkt")
(provide createCard getlengthCard cards? )
;Implementación del TDA carta
;Representación: Lista de Simbolos

;Constructor
;Dominio: Primer Elemento X Elementos | NULL
;Recorrido null | Elemento X Elementos
(define createCard (lambda( firstElement elements )
        (cons firstElement elements)))


;Pertenencia
;Dominio: Cartas
;Recorrido: True | False
(define cards? (lambda (cards)
                 (if (list? cards)
                     #true
                     #false)))

;Selector
;Dominio:  Carta
;Recorrido:  Largo
;Descripción: Retorna el largo de la carta 
(define getlengthCard (lambda (card) (length card)))
                 
