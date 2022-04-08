#lang scheme
(provide cardsSet)
;Implementación del TDA mazo
;Representación:  lista de Cartas

;Constructor
;Dominio: Elements (list) X numE(int) X maxC(int) X rndFn (fn)
;Recorrido: CardsSet
(define cardsSet (lambda (Elements numE maxC rndFn)
                   (if (and (>= numE 1)(>= maxC 1)
                            (and (not(null? Elements))(list? Elements)))
                       null
                       null)))









