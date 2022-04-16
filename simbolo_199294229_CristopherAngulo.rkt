#lang scheme
;Implementación del TDA símbolo
;Representación:  string  | number 

;Constructores

;Dominio: number | string
;Recorrido: element
;Descripción: crea un elemento a partir de un número o string
(define element (lambda (element)
                         (if (or (string? element) (number? element))
                             element
                             null)))
;Dominio: element
;Recorrido: true | false
;Descripción:   verfica si el elemento de entrada es es válido
(define element? (lambda (element)
                    (if (or (string? element) (number? element))
                        #true
                        #false)))

;Dominio: elements
;Recorrido: element
;Descripción: Retorna el primer elemento de una lista de elementos
(define getFirstElement (lambda (elements)
                    (car elements)))
