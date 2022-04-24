#lang scheme
(require "simbolo_19929422_CristopherAngulo.rkt")
(require "carta_19929422_CristopherAngulo.rkt")
(provide cardsSet dobble? numCards nthCard findTotalCards missingCards cardsSet->string getFirstCard getSecondCard )
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
(define cardsSet (lambda (elements numberElements maxCards randomFn)
    (if (and (>= numberElements 1)
             (>= maxCards 1)
             (isValidOrder (getOrderOfCardsSet numberElements))
             (isAValidCardsSetToCreate elements numberElements maxCards)
             (elements? elements))
     (shuffleCardsSet (createValidCardsSet elements numberElements maxCards) randomFn)
    (if (and (>= numberElements 1)
             (= maxCards -1)
             (isValidOrder (getOrderOfCardsSet numberElements))
             (isAValidCardsSetToCreate elements numberElements (getMaxNumberOfCards numberElements))
             (elements? elements))
     (shuffleCardsSet (createValidCardsSet elements numberElements (getMaxNumberOfCards numberElements)) randomFn)
    (if (and (>= numberElements 1)
             (>= maxCards 1)
             (isValidOrder (getOrderOfCardsSet numberElements))
             (<= maxCards (getMaxNumberOfCards numberElements) ) 
             (not(isAValidCardsSetToCreate elements numberElements maxCards))
             (elements? elements))
     (shuffleCardsSet (createIncompleteCardsSet (createValidCardsSet elements numberElements maxCards) emptyCardsSet  maxCards 0) randomFn)
      emptyCardsSet        
)))))

;Helper del Constructor
;Dominio: elementos X numero de elementos X maximo número de cartas 
;Recorrido: Mazo de Cartas
;Descripción: Mazo de Cartas Válido
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

;Helper Constructor
;Dominio: Mazo de Cartas X Mazo de Cartas Vacío X Máximo Número de Cartas X Contador
;Recorrido: Mazo de Cartas Incompleto
;Descripción: Crea un Mazo de cartas incompleto hasta el tope del máximo de cartas ingresado
(define createIncompleteCardsSet (lambda (cardsSet incompleteCardsSet maxNumberCards count)
    (if (= maxNumberCards count)
     incompleteCardsSet
     (createIncompleteCardsSet (getLastCards cardsSet)
     (addCardsToDeck incompleteCardsSet (getFirstCard cardsSet)) maxNumberCards (+ count 1)))                                                                      
))

;Helper del Constructor
;Dominio: elementos X Cantidad de elementos por carta
;Recorrido: Carta
;Tipo de Recursión: Recursión Natural
(define createFirstCard (lambda (elements n)
    (if (<= n 1)(createCard (getFirstElement elements) null)
        (createCard (getFirstElement elements) (createFirstCard (getTailElements elements) (- n 1))))))


;Helper de Constructor
;Dominio: Mazo de Cartas X Carta X Cantidad de Elementos por Carta X Cantidad total de Cartas X Auxiliar Entero X Auxiliar Entero
;Recorrido: Carta
;Tipo de Recursión: Recursión de Cola
(define auxiliarNextNCards (lambda (elements card n maxNumberCards j k )
    (if (= n k)
        card
        (auxiliarNextNCards elements (append card (list (getElementByPosition elements (+(* n j)(+ k 1)))))                                                     
                                    n maxNumberCards j (+ k 1) ))))

;Helper de Constructor
;Dominio: Lista de Simbolos X Mazo de Cartas X Cantidad de Simbolos por carta x cantidad total de cartas a generar 
;Recorrido: Mazo de las N  Cartas
;Tipo de recursión: Recursión de Cola
(define createNextNCards (lambda (elements cardsSet n maxNumberCards j)
    (if (= n j )
        cardsSet
        (createNextNCards elements
        (addCardsToDeck cardsSet (auxiliarNextNCards elements (list(getFirstElement elements)) n maxNumberCards (+ j 1) 0 ))
            n maxNumberCards (+ j 1)))))

;Helper de Constructor
;Dominio:
;Recorrido:
;Descripción:
(define secondAuxiliarcreateNextNSquareCards (lambda (elements card n maxNumberCards j i k)
    (if (> k n)
        card
        (secondAuxiliarcreateNextNSquareCards elements
        (append card (list (getElementByPosition elements (calculateValueToDrawACard n j i k ))))
        n maxNumberCards j i (+ k 1)))))

;Helper de Constructor
;Dominio:
;Recorrido:
;Descripción:
(define firstAuxiliarcreateNextNSquareCards (lambda ( elements cardsSet n maxNumberCards j i )
    (if (> j n)
        cardsSet
        (firstAuxiliarcreateNextNSquareCards elements (addCardsToDeck cardsSet
            (secondAuxiliarcreateNextNSquareCards elements (list (getElementByPosition elements i))
            n maxNumberCards j i 1))                                                                                                           
            n maxNumberCards (+ j 1) i))))

;Constructor
;Dominio:
;Recorrido:
;Descripción:
(define createNextNSquareCards (lambda (elements cardsSet n maxNumberCards i)
    (if (> i n)
        cardsSet
        (createNextNSquareCards elements
            (firstAuxiliarcreateNextNSquareCards elements cardsSet n maxNumberCards 1 i)
              n maxNumberCards (+ i 1)))))  

;Pertenencia
;Dominio: Mazo de cartas
;Recorrido: True | False
;Descripción: Revisa si el mazo de carta es una baraja válida para jugar 
;revisando que cada carta tiene elementos diferentes y todas las cartas tienen un elemento en común
(define dobble? (lambda (cardsSet)
    (if (and (allCardsAreOneElementInCommon cardsSet) (allCardsAreDifferentElements cardsSet ))
        #true
        #false
    )
))

;Validador
;Dominio: Elementos X Número de Elementos X Máximo Número de cartas
;Recorrido: True | False
;Descripción: Valida si el mazo de cartas es válido para crearse 
(define isAValidCardsSetToCreate (lambda (elements numberElements maxCards)
      (if (and (= (getMaxNumberOfCards numberElements) maxCards) (= (length elements) maxCards))
          #true
          #false)))
                              
;Helper de Pertenencia
;Dominio: Mazo de cartas
;Recorrido: True | False
;Descripción: Revisa si todas las cartas del mazo tienen un elemento o símbolo en común
(define allCardsAreOneElementInCommon (lambda (cardsSet)
   (if (compareFirstCardWithTailCards (getFirstCard cardsSet) (getLastCards cardsSet))
       #true
       #false)))

;Helper de Pertenencia
;Dominio: Primera Carta X Cartas restantes del mazo
;Recorrido: True | False
;Descripción: Compara la primera carta con las cartas restantes del mazo 
(define compareFirstCardWithTailCards (lambda (firstCard tailCards)
   (if (null? tailCards)
       #true
   (if (compareFirstCardWithTailCardsInside firstCard tailCards)
       (compareFirstCardWithTailCards (car tailCards) (cdr tailCards))
       #false))))

;Helper de Pertenencia
;Dominio: Primera Carta X Cartas restantes del mazo
;Recorrido: True | False
;Descripción: Compara la primera carta que entra con el resto de cartas 
(define compareFirstCardWithTailCardsInside(lambda (headCard tailCards)
   (if (null? tailCards)
       #true
   (if (compareTwoCards (append headCard (car tailCards)) 0)
       (compareFirstCardWithTailCardsInside headCard (cdr tailCards))
       #false))))

;Helper de Pertenencia
;Dominio: 2 Cartas Unidas en una sola lista
;Recorrido: True | False
;Descripción: Retorna Verdadero si únicamente tiene un elemento en común el las 2 cartas unidas
;ejemplo (1,2,3,4,1,5,6,7)
;primera recursión: comparar 1  con la cola (2,3,4,1,5,6,7) 1 elemento en común
;segunda recursión: comparar el 2 con la cola (3,4,1,5,6,7) ningún elemento en común
(define compareTwoCards(lambda (joinCard count)
   (if (and (null? joinCard)(= count 1))
       #true
   (if (< count 2)
       (compareTwoCards (getTailElements joinCard) (+ count(compareElements(getFirstElement joinCard)(getTailElements joinCard)0)))
       #false))))
  
;Helper de pertenencia                     
;Dominio: Primer Elemento de la carta unida X Resto de elementos de la carta unida
;Recorrido: Número de veces que repite un elemento entre las 2 cartas {0}+ 
;Descripción:  Retorna la cantidad de veces que se repite un elemento
; ejemplo: (1,2,3,4) (1,5,6,7) -> (1,2,3,4,1,5,6,7)
; tomar  1 y comparlo con el 2 
(define compareElements (lambda (firstElement tailElement count)
   (if (null? tailElement)
       count
   (if (eqv? firstElement (getFirstElement tailElement))
        (compareElements firstElement (getTailElements tailElement) (+ count 1))
        (compareElements firstElement (getTailElements tailElement) count )))))


;Helper de Pertenencia    
;Dominio: Mazo de Cartas
;Recorrido: True | False
;Descripción Valida si todas las cartas tiene elementos diferentes
(define allCardsAreDifferentElements (lambda (cardsSet)
   (if (null? cardsSet)
       #true
   (if(cardIsDifferentElements (getFirstCard cardsSet ))
      (allCardsAreDifferentElements (getLastCards cardsSet))
      #false
   )
  )
 )
)


; Helper de Pertenencia 
;Dominio: Carta
;Recorrido: True | False|
;Descripción: Valida si la carta tiene elementos diferentes
(define cardIsDifferentElements (lambda (card)
   (if (null? card)
       #true
   (if (compareCardElements (getTailElements card) (getFirstElement card))
       (cardIsDifferentElements (getTailElements card))
       #false))))
                                 

;Helper de Pertenencia
;Dominio: Carta X Elemento
;Recorrido: True | False
;Descripción: Compara los elementos de una carta dada
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
;Descripción: Obtiene el número máximo de cartas 
(define getMaxNumberOfCards (lambda (n) (+ (* (getOrderOfCardsSet n)(getOrderOfCardsSet n)) (getOrderOfCardsSet n) 1)))
       


;Selector
;Dominio: Mazo de cartas X Elementos X RandomFn
;Recorrido: Null | Mazo de Cartas con las cartas faltantes
;Descripción
(define missingCards (lambda (cardsSetToValidate elements randomFn)
    (if (= (numCards cardsSetToValidate) (findTotalCards (nthCard cardsSetToValidate 1))) 
      null      
     (outerCardsSet (cardsSet elements (length (nthCard cardsSetToValidate 1)) (findTotalCards (nthCard cardsSetToValidate 1)) randomFn) cardsSetToValidate )
)))         


;Selector
;Dominio: Mazo de Cartas Completo X Mazo de Cartas Incompleto
;Recorrido: Mazo de cartas con las cartas faltantes | Mazo de cartas Vacío
;Descripción Retorna un mazo de cartas con las cartas faltantes
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
           
                                                             
;Otro
;Dominio: orden del mazo
;Recorrido: true | false
;Descripción: retorna true o false para un orden válido
(define isValidOrder(lambda (order)
    (if (= order 1) #true 
    (if (isPrimeWrapper order)
        #true
        #false ))))

;Otro 
;Dominio: orden del mazo (number)
;Recorrido: true | false
;Descripción: Función que ejecuta una Recursión de Cola que retorna un true cuando el valor es primo
(define isPrimeWrapper (lambda (order)
   (define isPrime ( lambda (order count)
        (if (= order count)
           #true
         (if (= (remainder order count) 0)
           #false
           (isPrime order (+ count 1))))))
  (isPrime order 2)))

;Otro
;Dominio: orden del mazo (number) X auxiliar j (number) X auxiliar i (number X auxiliar k (number)
;Recorrido: posición (number)
;Descripción: calcular el valor de la posición que deseamos sacar del conjunto de elementos
(define calculateValueToDrawACard (lambda (n j i k) (- (+(+ n 2) (* n (- k 1)) (modulo(+(*(- i 1)(- k 1))(- j 1))n)) 1)))


;Otro
;Dominio: RandomFunction
;Recorrido: Mazo de Cartas desordenado
;Descripción: Desordena el mazo de cartas
(define shuffleCardsSet (lambda ( cardsSet randomFn)
      (define shuffleCardsSetAuxiliar (lambda (randomFn cardsSet count)
          (if ( = count (randomFn (length cardsSet)))
              cardsSet
              (shuffleCardsSetAuxiliar randomFn (append  (getLastCards cardsSet)  (list(getFirstCard cardsSet))) (+ count 1)))))
     (shuffleCardsSetAuxiliar randomFn cardsSet 0)))











 