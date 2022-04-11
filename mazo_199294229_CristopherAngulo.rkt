#lang scheme
(provide cardsSet)
;Implementación del TDA mazo
;Representación:  Lista de Cartas

;Constructor
;Dominio: Elements (list) X numE(int) X maxC(int) X rndFn (fn)
;Recorrido: CardsSet
;Descripción: 
(define cardsSet (lambda (Elements numE maxC rndFn)
     (if (and (>= numE 1)(>= maxC 1) (isValidOrder (- numE 1)) (not(null? Elements)))
          null
          null
     ))
)


; numE - 1 -> orden del plano proyectivo
; maxC

(define isValidOrder(lambda (order)
     (if (isPrimeWrapper order)
         #true
         #false 
     ))
)

(define isPrimeWrapper (lambda (number)    
 (isPrime number 2)
))

(define isPrime ( lambda (number count)
     (if (= number count) #t
     (if (= (remainder number count) 0) #f
     (isPrime number (+ count 1))))))



; ejemplo de uso (isPrimeWrapper 4)

