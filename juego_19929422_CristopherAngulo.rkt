#lang scheme
(require "mazo_19929422_CristopherAngulo.rkt")
(require "modo_19929422_CristopherAngulo.rkt")
(require "jugador_19929422_CristopherAngulo.rkt")
(provide game)
;Implementación del TDA Juego
;Representación: numPlayers(int) X cardsSet X mode (fn) X rndFn (fn)


;Constructor
;Dominio: Número de Jugadores X Mazo de Cartas X Modo de Juego X Función Random
;Recorrido: Juego 
;Descripción: Función que crea el juego
(define game (lambda (numPlayers cardsSet mode rndFn)
 (if (and (validateNumberOfPlayerToPlay numPlayers) (dobble? cardsSet))     
     (list numPlayers initialPlayers cardsSet mode initialAreaGame)
      null)))



(define initialPlayers null)
(define initialAreaGame "área de juego")

(define validateNumberOfPlayerToPlay (lambda ( numPlayers)
    (if (and (number? numPlayers) ( > numPlayers 1))
        #true
        #false)))


;Selector
;Dominio: Juego
;Recorrido: Jugadores
;Descripción: Retorna los jugadores actuales del juego
(define getPlayers (lambda (game)(cadr game)))


;Selector
;Dominio: Jugadores 
;Recorrido:  Jugador
;Descripción: Retorna el primer jugador
(define getFirstPlayer (lambda (players) (car players)))

;Selector
;Dominio: Jugadores 
;Recorrido:  Jugadores
;Descripción: Retorna los últimos Jugadores
(define getLastPlayers (lambda (players) (cdr players)))


;Selector
;Dominio: Juego 
;Recorrido:  Número de Jugadores
;Descripción: Retorna los el número de jugadores
(define getNumberPlayers (lambda (game) (car game)))

;Selector
;Dominio: Juego 
;Recorrido:  Mazo de Cartas
;Descripción: Retorna los el número de jugadores
(define getCardsSet (lambda (game) (caddr game)))

;Selector
;Dominio: Juego 
;Recorrido:  Modo de Juego
;Descripción: Retorna el modo de juego
(define getPlayMode (lambda (game) (cadddr game)))


;Selector
;Dominio: Juego 
;Recorrido:  Modo de Juego
;Descripción: Retorna el modo de juego
(define getAreaGame (lambda (game) ( car (cddddr  game))))

;Selector
;Dominio: Juego 
;Recorrido:  Jugador que le toca jugar
;Descripción: Retorna el usuario que le toca jugar
(define whoseTurnIsIt? (lambda (game) (getFirstPlayer(getPlayers game))))
; recordar -> mover el jugador al último                    


;Modificador
;Dominio: jugador X Juego
;Recorrido: juego
;Descripción: registra un usuario nuevo al juego
(define register (lambda (player game)
   (if (and (player? player) (not(playerIsRegistered (getPlayers game) player)))
         (list (getNumberPlayers game) ( append (getPlayers game) (list player) ) (getCardsSet game) (getPlayMode game) (getAreaGame game))
          game)))                                                              


;Modificador
(define play (lambda (game action)
 (if (null? action)
    (list (getNumberPlayers game) ( append (getPlayers game) (list player))
          (getCardsSet game) (getPlayMode game) ((getPlayMode game) (getCardsSet game)))  
     #false
     )))
  


                 

;Otros
;Dominio: Jugadores X Jugador
;Recorrido:  True | False
;Descripción: Revisa si el jugador ya se encuentra registrado
(define playerIsRegistered (lambda (players player)
  (if (null? players)
      #false
  (if (not(eqv? player (car players)))
      (playerIsRegistered (getLastPlayers players) player)
      #true))))



(define numPlayers 2)
(define elements (list  1 2 3 4 5 6 7 8 9 10 11 12 13 ))
(define dobbleCards (cardsSet elements 4 13 3))
(define game1  (game numPlayers dobbleCards stackMode 2))
;ejemplo de ejecución de juego
; game1 

(define player1 (player "cristopher"))
(define player2 (player "cristian"))
(define player3 (player "cristobal"))

;(whoseTurnIsIt? (register "pedro" (register "Felipe" (register "Cristopher" game1))))
(play (register "pedro" (register "Felipe" (register "Cristopher" game1))) null) 



