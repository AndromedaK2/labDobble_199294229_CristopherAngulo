#lang scheme
(provide element getFirstElement getTailElements)
(provide elements?)

;Implementación del TDA símbolo
;Representación:  string  | number 

;Constructor
;Dominio: number | string
;Recorrido: element
;Descripción: crea un elemento a partir de un número o string
(define element (lambda (element)
                         (if (or (string? element) (number? element))
                             element
                             null)))


;Pertenencia
;Dominio: element
;Recorrido: true | false
;Descripción:   verfica si el elemento de entrada es es válido
(define element? (lambda (element)
                    (if (or ( number? element) (string? element))
                        #true
                        #false)))


;Pertenencia
;Dominio: elements
;Recorrido: true | false
;Descripción:   verfica si todos los elementos son válidos
(define elements? (lambda (elements)
    (if (and (not(null? elements)) (validateEachElement elements ))
      #true
      #false)
))

;Pertenencia
;Dominio: elements
;Recorrido: true | false
;Descripción: Recorre la lista de elementos
(define validateEachElement (lambda (elements)
    (if ( null? elements )
        #true
    (if (not(element? (getFirstElement elements)))
        #false
        (validateEachElement (getTailElements elements))
))))


;Selector
;Dominio: elements
;Recorrido: element
;Descripción: Retorna la cola de los elementos
(define getTailElements (lambda (elements)
                    (cdr elements)))


;Selector
;Dominio: elements
;Recorrido: element
;Descripción: Retorna el primer elemento de una lista de elementos
(define getFirstElement (lambda (elements)
                    (car elements)))


(define getSecondElement (lambda (elements)
                           (cadr elements)))


;Ejemplo de uso
;(define elements (list "1" 2 3 4 5 5 6 6 7))
;(elements? elements)


