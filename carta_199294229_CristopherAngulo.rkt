#lang scheme
(require "simbolo_199294229_CristopherAngulo.rkt")
(provide createCard isValidCard)
;Implementación del TDA carta
;Representación: Lista de Simbolos

;Constructor
;Dominio: Lista
;Recorrido null | Lista
(define createCard (lambda( list length )
                     (if (null? list)  
                         null 
                         list
                         )))

;Pertenencia
;Dominio: Lista
;Recorrido True o False
(define isValidCard(lambda (list lenght)
                     (if (null? list)
                         #false
                         #true)))

