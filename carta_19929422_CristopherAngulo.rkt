#lang scheme
(require "simbolo_19929422_CristopherAngulo.rkt")
(provide createCard getlengthCard)
;Implementación del TDA carta
;Representación: Lista de Simbolos

;Constructor
;Dominio: Primer Elemento X Elementos | NULL
;Recorrido null | Elemento X Elementos
(define createCard (lambda( firstElement elements )
        (cons firstElement elements)))



;Selector
;Dominio:  Carta
;Recorrido:  Largo
;Descripción: Retorna el largo de la carta 
(define getlengthCard (lambda (card) (length card)))
                 
