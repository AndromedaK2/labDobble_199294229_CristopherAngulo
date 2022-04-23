#lang scheme
(require "simbolo_19929422_CristopherAngulo.rkt")
(require "carta_19929422_CristopherAngulo.rkt")
(provide cardsSet dobble? getFirstCard getSecondCard)
;Implementación del TDA mazo
;Representación:  Lista de Cartas

;Constructor
;Dominio: no recibe parámetros
;Recorrido: mazo de cartas vacio
;Descripción:  retorna una mazo de cartas vacío
(define emptyCardsSet null)


;Constructor
;Dominio: elementos (lista de símbolos) X número de elementos (number) X máximo número de cartas (number) X Función Random
;Recorrido: mazo de cartas | mazo de cartas vacío | mazo de cartas incompleto
;Descripción: retorna un mazo de cartas
(define cardsSet (lambda (elements numberElements maxCards rndFn)
    (if (and (>= numberElements 1)
             (>= maxCards 1)
             (isValidOrder (getOrderOfCardsSet numberElements))
             (isAValidCardsSetToCreate elements numberElements maxCards)
             (elements? elements))
      (createValidCardsSet elements numberElements maxCards)
    (if (and (>= numberElements 1)
             (= maxCards -1)
             (isValidOrder (getOrderOfCardsSet numberElements))
             (isAValidCardsSetToCreate elements numberElements (getMaxNumberOfCards numberElements))
             (elements? elements))
      (createValidCardsSet elements numberElements (getMaxNumberOfCards numberElements))
    (if (and (>= numberElements 1)
             (>= maxCards 1)
             (isValidOrder (getOrderOfCardsSet numberElements))
             (<= maxCards (getMaxNumberOfCards numberElements) ) 
             (not(isAValidCardsSetToCreate elements numberElements maxCards))
             (elements? elements))
      (createIncompleteCardsSet (createValidCardsSet elements numberElements maxCards) emptyCardsSet  maxCards 0)
      emptyCardsSet        
)))))

;Constructor
;Dominio: elementos X numero de elementos X maximo de cartas 
;Recorrido: mazo de cartas
;Descripción: mazo de cartas válido
(define createValidCardsSet 
    (lambda (elements numberElements maxCards)
        (createNextNSquareCards elements
            (createNextNCards  elements
                (addCardsToDeck emptyCardsSet 
                    (createFirstCard elements numberElements))
            (getOrderOfCardsSet numberElements) maxCards 0) 
        (getOrderOfCardsSet numberElements) maxCards 1)
    )
)

(define createIncompleteCardsSet (lambda (cardsSet incompleteCardsSet maxNumberCards count)
    (if (= maxNumberCards count)
     incompleteCardsSet
     (createIncompleteCardsSet (getLastCards cardsSet)
     (addCardsToDeck incompleteCardsSet (car cardsSet)) maxNumberCards (+ count 1)))                                                                      
))


;Dominio:
;Recorrido:
;Descripción:
(define isAValidCardsSetToCreate (lambda (elements numberElements maxCards)
      (if (and (= (getMaxNumberOfCards numberElements) maxCards) (= (length elements) maxCards))
          #true
          #false)))
                              

;Pertenencia
;Dominio: mazo de cartas
;Recorrido: true | false
;Descripción
(define dobble? (lambda (cardsSet)
    (if (and (allCardsAreOneElementInCommon cardsSet) (allCardsAreDifferentElements cardsSet ))
        #true
        #false
    )
))

;Dominio:
;Recorrido:
;Descripción:
(define allCardsAreOneElementInCommon (lambda (cardsSet)
   (if (compareFirstCardWithTailCards (car cardsSet) (cdr cardsSet))
       #true
       #false)))


;Dominio: Primera Carta X Cartas restantes del mazo
;Recorrido: true | false

(define compareFirstCardWithTailCards (lambda (firstCard tailCards)
   (if (null? tailCards)
       #true
   (if (compareFirstCardWithTailCardsInside firstCard tailCards)
       (compareFirstCardWithTailCards (car tailCards) (cdr tailCards))
       #false))))



;Dominio: Primera Carta X Cartas restantes del mazo
;Recorrido: true | false
(define compareFirstCardWithTailCardsInside(lambda (headCard tailCards)
   (if (null? tailCards)
       #true
   (if (compareTwoCards (append headCard (car tailCards)) 0)
       (compareFirstCardWithTailCardsInside headCard (cdr tailCards))
       #false))))


;Dominio: 2 Cartas Unidas en una sola lista
;Recorrido: true | false
;ejemplo (1,2,3,4,1,5,6,7)
;primera recursión: comparar 1  con la cola (2,3,4,1,5,6,7) 1 elemento en común
;segunda recursión: comparar el 2 con la cola (3,4,1,5,6,7) ningún elemento en común
(define compareTwoCards(lambda (joinCard count)
   (if (and (null? joinCard)(= count 1))
       #true
   (if (< count 2)
       (compareTwoCards (getTailElements joinCard) (+ count(compareElements(getFirstElement joinCard)(cdr joinCard)0)))
       #false))))
  
                      
;Dominio: Primer Elemento de la carta unida X Resto de elementos de la carta unida
;Recorrido: numero de veces que repite un elemento entre las 2 cartas
; ejemplo: (1,2,3,4) (1,5,6,7) -> (1,2,3,4,1,5,6,7)
; tomar  1 y comparlo con el 2 
(define compareElements (lambda (firstElement tailElement count)
   (if (null? tailElement)
       count
   (if (eqv? firstElement (car tailElement))
        (compareElements firstElement (getTailElements tailElement) (+ count 1))
        (compareElements firstElement (getTailElements tailElement) count )))))

        
       
;Dominio: mazo de cartas 
;Recorrido: true | false
;Descripción
(define allCardsAreDifferentElements (lambda (cardsSet)
   (if (null? cardsSet)
       #true
   (if(cardAreDifferentElements (getFirstCard cardsSet ))
      (allCardsAreDifferentElements (getLastCards cardsSet))
      #false
   )
  )
 )
)


;carta (1,8,9,19)
;Dominio:
;Recorrido:
;Descripción:
(define cardAreDifferentElements (lambda (card)
   (if (null? card)
       #true
   (if (compareCardElements (getTailElements card) (getFirstElement card))
       (cardAreDifferentElements (getTailElements card))
       #false))))
                                 

;Dominio:
;Recorrido:
;Descripción:
; carta -> (1,8,9,10)
; cola card -> (8,9,10)
; element -> (1)
(define compareCardElements (lambda (card element)
   (if (null? card)
       #true
   (if (eqv? element (getFirstElement card))
       #false
       (compareCardElements (getTailElements card) element)))))
          
          

;Selector
;Dominio: mazo de cartas
;Recorrido: carta
;Descripción retorna la primera carta del mazo entrante
(define getFirstCard (lambda (cardsSet)(car cardsSet)))

;Selector
;Dominio: mazo de cartas
;Recorrido: carta
;Descripción retorna la primera carta del mazo entrante
(define getSecondCard (lambda (cardsSet)(cadr cardsSet)))


;Selector
;Dominio: mazo de cartas
;Recorrido: mazo de cartas
;Descripción retorna el mazo de carta sin la primera carta
(define getLastCards (lambda (cardsSet)(cdr cardsSet)))

;Selector
;Dominio: mazo de cartas
;Recorrido: cantidad de cartas (number)
;Descripción retorna la cantidad de cartas del mazo
(define numCards (lambda (cardsSet) (length cardsSet)))


;Selector
;Dominio: mazo de cartas X posición
;Recorrido: carta
;Descripción retorna la carta solicitada
(define nthCard (lambda (cardsSet position)(list-ref cardsSet position)))         


;Selector
;Dominio: Carta
;Recorrido: cantidad total de cartas (number)
;Descripción retorna cantidad total de cartas
(define findTotalCards (lambda (card)(+ (* (- ( getlengthCard card) 1) (- ( getlengthCard card) 1))  (- ( getlengthCard card) 1) +1)))                        


;Selector
;Dominio: Mazo de Cartas
;Recorrido: cantidad total de elementos (number)
;Descripción retorna cantidad de elementos necesarios para construir el mazo
(define requiredElements (lambda (card)(+ (* (- ( getlengthCard card) 1) (- ( getlengthCard card) 1))  (- ( getlengthCard card) 1) +1)))
                           
;Selector
;Dominio: elementos X posición (number)
;Recorrido: Simbolo
;Descripción: Obtener el elemento de la posición dada
(define getElementByPosition (lambda (elements position)(list-ref elements position)))


;Selector
;Dominio: elementos X posición (number)
;Recorrido: Simbolo
;Descripción: Obtener el elemento de la posición dada
(define getOrderOfCardsSet (lambda (numberElements)(- numberElements 1)))


;Selector
;Dominio: Orden del Mazo
;Recorrio: Número máximo de cartas
;Descripción:
(define getMaxNumberOfCards (lambda (n) (+ (* (getOrderOfCardsSet n)(getOrderOfCardsSet n)) (getOrderOfCardsSet n) 1)))
       


;Selector
;Dominio
;Recorrido
;Descripción
(define missingCards (lambda (cardsSetToValidate elements)
    (if (= (numCards cardsSetToValidate) (findTotalCards (nthCard cardsSetToValidate 1))) 
      null      
     (outerCardsSet (cardsSet elements (length (nthCard cardsSetToValidate 1)) (findTotalCards (nthCard cardsSetToValidate 1)) 2) cardsSetToValidate )
)))         


;Selector
;Dominio
;Recorrido
;Descripción
(define (outerCardsSet fullCardsSet IncompleteCardsSet)
  (if (null? fullCardsSet)
      emptyCardsSet
  (if (not(containCard? (getFirstCard fullCardsSet) IncompleteCardsSet))
     (cons (getFirstCard fullCardsSet) (outerCardsSet (getLastCards fullCardsSet) IncompleteCardsSet))
     (outerCardsSet (getLastCards fullCardsSet) IncompleteCardsSet))))

(define containCard? member)

                           
;Modificador
;Dominio: Mazo de Cartas X Carta
;Recorrido: Mazo de Cartas
(define addCardsToDeck (lambda (cardsSet newCard )
    (if (null? cardsSet)
        (append (list newCard))                      
        (append  cardsSet (list newCard)))))

;Modificador
;Dominio: Mazo de Cartas
;Recorrido: string con las cartas del mazo
;Descripción: retorna una representación en cadena de caracteres para el mazo de cartas
(define cardsSet->string (lambda (cardsSet)
    (define cardsSet->stringAuxiliar (lambda (cardsSet position cardsSetToString )
       (if (null? cardsSet)
           cardsSetToString
           (cardsSet->stringAuxiliar (getLastCards cardsSet) (+ position 1) (string-append cardsSetToString (card->string (getFirstCard cardsSet) position ) "\n")))))
   (cardsSet->stringAuxiliar cardsSet  1 "")))
           1

                                                             
;Otros
;Dominio: orden del mazo
;Recorrido: true | false
;Descripción: retorna true o false para un orden válido
(define isValidOrder(lambda (order)
    (if (= order 1) #true 
    (if (isPrimeWrapper order)
        #true
        #false ))))


;Otros 
;Dominio: orden del mazo (number)
;Recorrido: true | false
;Descripción: Función que ejcuta una Recursión de Cola que retorna un true cuando el valor es primo
(define isPrimeWrapper (lambda (order)
   (define isPrime ( lambda (order count)
        (if (= order count)
           #true
         (if (= (remainder order count) 0)
           #false
           (isPrime order (+ count 1))))))
  (isPrime order 2)))



;Otros
;Dominio: orden del mazo (number) X auxiliar j (number) X auxiliar i (number X auxiliar k (number)
;Recorrido: posición (number)
;Descripción: calcular el valor de la posición que deseamos sacar del conjunto de elementos
(define calculateValueToDrawACard (lambda (n j i k) (- (+(+ n 2) (* n (- k 1)) (modulo(+(*(- i 1)(- k 1))(- j 1))n)) 1)))

;Constructor
;Dominio: elementos X Cantidad de elementos por carta
;Recorrido: Carta
;Tipo de Recursión: Recursión Natural
(define createFirstCard (lambda (elements n)
    (if (<= n 1)(createCard (getFirstElement elements) null)
        (createCard (getFirstElement elements) (createFirstCard (getTailElements elements) (- n 1))))))


;Otro
;Dominio: Mazo de Cartas X Carta X Cantidad de Elementos por Carta X Cantidad total de Cartas X Auxiliar Entero X Auxiliar Entero
;Recorrido: Carta
;Tipo de Recursión: Recursión de Cola
(define auxiliarNextNCards (lambda (elements card n countTotalCards j k )
    (if (= n k)
        card
        (auxiliarNextNCards elements (append card (list (getElementByPosition elements (+(* n j)(+ k 1)))))                                                     
                                    n countTotalCards j (+ k 1) ))))

;Otro
;Dominio: Lista de Simbolos X Mazo de Cartas X Cantidad de Simbolos por carta x cantidad total de cartas a generar 
;Recorrido: Mazo de las N  Cartas
;Tipo de recursión: Recursión de Cola
(define createNextNCards (lambda (elements cardsSet n countTotalCards j)
    (if (= n j )
        cardsSet
        (createNextNCards elements
        (addCardsToDeck cardsSet (auxiliarNextNCards elements (list(getFirstElement elements)) n countTotalCards (+ j 1) 0 ))
            n countTotalCards (+ j 1)))))

;Otros:
;Dominio:
;Recorrido:
;Descripción:
(define secondAuxiliarcreateNextNSquareCards (lambda (elements card n countTotalCards j i k)
    (if (> k n)
        card
        (secondAuxiliarcreateNextNSquareCards elements
        (append card (list (getElementByPosition elements (calculateValueToDrawACard n j i k ))))
        n countTotalCards j i (+ k 1)))))

;Otros:
;Dominio:
;Recorrido:
;Descripción:
(define firstAuxiliarcreateNextNSquareCards (lambda ( elements cardsSet n countTotalCards j i )
    (if (> j n)
        cardsSet
        (firstAuxiliarcreateNextNSquareCards elements (addCardsToDeck cardsSet
            (secondAuxiliarcreateNextNSquareCards elements (list (getElementByPosition elements i))
            n countTotalCards j i 1))                                                                                                           
            n countTotalCards (+ j 1) i))))
;Otros:
;Dominio:
;Recorrido:
;Descripción:
(define createNextNSquareCards (lambda (elements cardsSet n countTotalCards i)
    (if (> i n)
        cardsSet
        (createNextNSquareCards elements
            (firstAuxiliarcreateNextNSquareCards elements cardsSet n countTotalCards 1 i)
                            n countTotalCards (+ i 1)))))  

;Elementos de ejemplo
;(define elements (list  1 2 3 4 5 6 7 8 9 10 11 12 13))
;(define elementoss (list (element "A") (element 2) (element "D") (element "C") (element "3") (element 8) (element 10)))
(define elements (list  1 2 3 4 5 6 7 8 9 10 11 12 13))
(define elementoss (list (element "A") (element 2) (element "D") (element "C") (element "3") (element 8) (element 10)))
;Mazo de carta de ejemplo
(cardsSet elementoss 3 -1 3)
(cardsSet elementoss 3 7 3)
(cardsSet elements   4 13 3)
(cardsSet elements   4 10 3)
;Cantidad de cartas del mazo
(numCards (cardsSet elementoss 3 7 3) )
(numCards (cardsSet elementoss 3 7 3) )
;Retornar Carta
(nthCard (cardsSet elements  4 13 3) 2 )
(nthCard (cardsSet elements   4 13 3) 2 )
;Encontrar total de cartas
(findTotalCards (nthCard (cardsSet elements 4 13 3) 2) )
(findTotalCards (nthCard (cardsSet elementoss 3 7 3) 2) )
;el juego es valido dobble
;(dobble? (cardsSet elements 4 13 3)) 
;(define elementoss (list (element "A") (element 2) (element "D") (element "C") (element "3") (element 8) (element 10)))
(dobble? (cardsSet elements 4 13 3)) 
(missingCards (cardsSet elementoss 3 5 3) elementoss) 

(display (cardsSet->string (cardsSet elements 4 13 3)))  




 