#lang scheme
(require "simbolo_19929422_CristopherAngulo.rkt")
(provide createCard getlengthCard cards? card->string )
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

;Pertenencia
;Dominio: Carta
;Recorrido: True | False
;Descripción: Retorna verdadero si el valor entrante es una carta sino retorna falso
;(define card? (lambda (card)
;  (and (list? card)
;     (not (null? card))
;    (not (null? (producto (car p) (cdr p))))))              

;Selector
;Dominio:  Carta
;Recorrido:  Largo
;Descripción: Retorna el largo de la carta 
(define getlengthCard (lambda (card) (length card)))


;Selector
;Dominio:  Carta
;Recorrido:  Largo
;Descripción: Retorna el primer simbolo de la carta
(define getFirstElementOfCard (lambda (card) (car card)))

;Selector
;Dominio:  Carta
;Recorrido:  Carta 
;Descripción: Retorna carta sin el primer elemento o simbolo
(define getLastElementsOfCard (lambda (card) (cdr card)))


;Modificador: Carta
;Recorrido: String
;Descripción: Retorna la carta en formato de string
(define card->string (lambda (card position)
   (define card->stringAuxiliar (lambda (card cardToString)
    (if (null? card)
       cardToString
       (card->stringAuxiliar (getLastElementsOfCard card) (string-append cardToString " " (number->string (getFirstElementOfCard card)))) 
     )))
  (card->stringAuxiliar card (string-append "C" (number->string position) ":"))))


;Ejemplo
(card->string (list 2 3 4) 1)

        
                 
