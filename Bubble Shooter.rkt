;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |PROYECTO FINAL FDP|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;___________________________________________NOTAS___________________________________________

;-La dificultad del juego cambia cada dos minutos.

;___________________________________________GRUPO___________________________________________

; Integrante 1:
; Nombre: Christian Camilo Taborda 
; Código: 201632081
; Plan: 3743

; Integrante 2:
; Nombre: Mike Davila Giraldo
; Código: 201632701
; Plan: 3743

; Integrante 3:
; Nombre: Juan Felipe Uribe
; Código: 201622976
; Plan: 3743

; Archivo: Proyecto Final FDP
; Nombre: Bubble Shooter
; Fecha de creación: 29/06/2016
; Fecha de última modificación: 

;___________________________________________LIBRERÍAS___________________________________________

;Universe : Librería para la creación de mundos interactivos.
(require 2htdp/universe)

;Image : Librería para el manejo de imágenes.
(require 2htdp/image)

;___________________________________________DEFINICIÓN DE DATOS___________________________________________

;Un shooter es una estructura:
;(make-shooter image posn)
(define-struct shooter [imagen posicion])

;Una paleta-colores es una lista:
;-empty
;-(cons string paleta-colores)

;Una dificultad es una cadena de:
;-[ "Novato" , "Medio" , "Experto" , "Maestro" ]

;Una burbuja es una estructura:
;(make-burbuja image posn)
(define-struct burbuja [imagen posicion])

;Un T es una estructura:
;(make-T number number posn)
(define-struct T [minutos segundos posn])

;Una lista-burbujas es una lista:
;-empty
;(cons burbuja lista-burbujas)

;Un ejercito es una lista:
;-empty
;-(cons lista-burbujas ejercito)

;Un mundo es una estructura:
;(make-mundo shooter burbuja ejercito T lista-burbujas lista-burbujas string number)
(define-struct mundo [disparador proyectil ejercito tiempo siguientes bala dificultad puntaje])

;Una lista-posn es una lista:
;-empty
;-(cons posn lista-posn)

;Una bomba es una burbuja imagen:
;-(make-burbuja (overlay (circle 15 "outline" "black")
;                        (underlay/offset (circle 15 "solid" "black") -6 -6 
;                        (overlay (circle 5 "outline" "black") (circle 5 "solid" "white")))) (make-posn X Y))

;___________________________________________CONSTANTES___________________________________________

;Estas constantes definen el alto y el ancho del área del juego:
(define alto 562)
(define ancho 975)

;Esta constante define el disparador del juego:
(define disparador (make-shooter (overlay (isosceles-triangle 30 40 "outline" "black")
                                          (isosceles-triangle 30 40 "solid" "dim gray"))
                                 (make-posn 324 547)))

;Esta constante define la imagen de la barra lateral:
(define barra (overlay (rectangle 8 (* 2 alto) "outline" "black")
                       (rectangle 8 (* 2 alto) "solid" "dim gray")))

;Esta constante define la paleta de colores de las burbujas:

;- Novato:
(define c-novato (list "red" "yellow" "blue" "green"))
;- Medio:
(define c-medio (cons "brown" c-novato))
;- Experto:
(define c-experto (cons "purple" c-medio))
;- Maestro:
(define c-maestro (cons "orange" c-experto))

;___________________________________________FUNCIONES NECESARIAS PREVIAMENTE___________________________________________

;cantidad-elementos : list --> number
;Retorna la cantidad de elementos de una lista.
(define (cantidad-elementos lista)
  (cond
    [(empty? lista) 0]
    [(cons? lista) (+ 1 (cantidad-elementos (rest lista)))]))

;make-bubble : paleta-colores paleta-colores --> image
;Crea la imagen de una burbuja con un color al azar, dada una paleta-colores y otra paleta-colores de referencia.
(define (make-bubble colors referencia)
  (overlay (circle 15 "outline" "black")
           (underlay/offset (circle 15 "solid" (if (string=? "black" (list-ref colors (random (cantidad-elementos colors))))
                                                   (if (= (random 8) 4) "black"
                                                   (list-ref referencia (random (cantidad-elementos referencia))))
                                                   (list-ref referencia (random (cantidad-elementos referencia))))) -6 -6
                            (overlay (circle 5 "outline" "black")
                                     (circle 5 "solid" "white")))))

;_________________________________________________________________________________________________________________________________

;Esta constante define la imagen del proyectil:
(define proyectil (make-burbuja (make-bubble c-novato c-novato) (make-posn 324 532)))

;Esta constante define la siguiente burbuja:
(define next (make-burbuja (make-bubble c-novato c-novato) (make-posn 900 492)))

;Esta constante define la lista de las burbujas siguientes:
(define siguientes (list next))

;Esta constante define el tiempo transcurrido:
(define tiempo (make-T 0 0 (make-posn 812.5 92)))

;Esta constante define una lista con la burbuja disparada:
(define bala empty)

;Estas constantes definen listas de burbujas:
(define burbujas1
  (list
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 20 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 52 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 84 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 116 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 148 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 180 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 212 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 244 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 276 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 308 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 340 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 372 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 404 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 436 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 468 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 500 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 532 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 564 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 596 20))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 628 20))))

(define burbujas2
  (list
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 36 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 68 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 100 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 132 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 164 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 196 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 228 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 260 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 292 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 324 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 356 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 388 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 420 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 452 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 484 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 516 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 548 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 580 52))
   (make-burbuja (make-bubble (cons "black" c-novato) c-novato) (make-posn 612 52))))

;___________________________________________FUNCIONES NECESARIAS PREVIAMENTE___________________________________________

;generar-burbujas : lista-burbujas number --> lista-burbujas
;Genera una lista de burbujas a partir de otra sumando cierta cantidad a las posiciones en Y de las burbujas. 
(define (generar-burbujas list-burbujas cantidad)
  (cond
    [(empty? list-burbujas) empty]
    [(cons? list-burbujas) (cons (make-burbuja (burbuja-imagen (first list-burbujas))
                                 (make-posn (posn-x (burbuja-posicion (first list-burbujas))) (+ cantidad (posn-y (burbuja-posicion (first list-burbujas))))))
                           (generar-burbujas (rest list-burbujas) cantidad))]))

;colorear : image paleta-colores paleta-colores --> image
;Cambia el color de la imagen de una burbuja dada una paleta-colores:
(define (colorear bubble color referencia)
  (make-bubble color referencia))

;colorear-burbujas : lista-burbujas paleta-colores --> lista-burbujas
;Cambia el color de las imágenes de una lista-burbujas dada una paleta-colores.
(define (colorear-burbujas list-burbujas color referencia)
  (cond
    [(empty? list-burbujas) empty]
    [(cons? list-burbujas) (cons (make-burbuja (colorear (burbuja-imagen (first list-burbujas)) color referencia) (burbuja-posicion (first list-burbujas)))
                                 (colorear-burbujas (rest list-burbujas) color referencia))]))

;_________________________________________________________________________________________________________________________________

;Esta constante define el ejército de burbujas del juego:
(define burbujas (list burbujas1 burbujas2 (generar-burbujas (colorear-burbujas burbujas1 (cons "black" c-novato) c-novato) 64)
                      (generar-burbujas (colorear-burbujas burbujas2 (cons "black" c-novato) c-novato) 64)
                      (generar-burbujas (colorear-burbujas burbujas1 (cons "black" c-novato) c-novato) 128)))

;Esta constante define el estado principal del juego:
(define estado (make-mundo disparador proyectil burbujas tiempo siguientes bala "Novato" 0))

;Estas constantes definen el movimiento del disparador:
(define +delta 16)
(define -delta -16)

;Estas constantes definen el movimiento del proyectil y del ejercito:
(define -alfa -32)
(define +alfa 32)

;Esta constante define la imagen de la escena principal del juego:
(define escena (place-image (text/font "Tiempo:" 24 "black" "Gill Sans" 'swiss 'normal 'bold #false) 812.5 52
               (place-image (text/font "Dificultad:" 24 "black" "Gill Sans" 'swiss 'normal 'bold #true) 741.25 152
               (place-image (text/font "Puntaje:" 24 "black" "Gill Sans" 'swiss 'normal 'bold #true) 754.25 184
               (place-image (text/font "Récord:" 24 "black" "Gill Sans" 'swiss 'normal 'bold #true) 759.25 216
               (place-image (text/font "Controles:" 24 "black" "Gill Sans" 'swiss 'normal 'bold #false) 812.5 296
               (place-image (text/font "<-- / Izquierda" 24 "dim gray" "Gill Sans" 'swiss 'normal 'bold #false) 812.5 344
               (place-image (text/font "--> / Derecha" 24 "dim gray" "Gill Sans" 'swiss 'normal 'bold #false) 812.5 376
               (place-image (text/font "Espacio / Disparar" 24 "dim gray" "Gill Sans" 'swiss 'normal 'bold #false) 812.5 408
               (place-image (text/font "Siguiente -->" 24 "black" "Gill Sans" 'swiss 'normal 'bold #false) 770.25 492             
               (place-image barra 650 497 (place-image (rectangle 650 560 "solid" "gray") 326 281.5 (empty-scene ancho alto)))))))))))))

;___________________________________________FUNCIONES AUXILIARES___________________________________________

;draw-burbujas : lista-burbujas image image --> image
;Pinta las burbujas de una lista-burbujas en una escena.
(define (draw-burbujas lista-burbujas escena)
  (cond
    [(empty? lista-burbujas) escena]
    [(cons? lista-burbujas) (place-image
                             (burbuja-imagen (first lista-burbujas))
                             (posn-x (burbuja-posicion (first lista-burbujas)))
                             (posn-y (burbuja-posicion (first lista-burbujas)))
                             (draw-burbujas (rest lista-burbujas) escena))]))

;draw-ejercito : ejercito image --> image
;Pinta las burbujas de un ejército en una escena.
(define (draw-ejercito ejercito escena)
  (cond
    [(empty? ejercito) escena]
    [(cons? ejercito) (draw-burbujas (first ejercito) (draw-ejercito (rest ejercito) escena))]))

;draw-time : T image --> image
;Pinta el tiempo transcurrido en una escena.
(define (draw-time T scene)
      (place-image
       (text/font
        (string-append
         (number->string (T-minutos T)) " " ":" " " (number->string (floor (T-segundos T))))
        24 "dim gray" "Gill Sans" 'swiss 'normal 'bold #f)
       (posn-x (T-posn T)) (posn-y (T-posn T)) scene))

;draw-dificultad : dificultad image --> image
;Pinta la dificultad actual del juego:
(define (draw-dificultad dificultad scene)
  (place-image
   (text/font dificultad 24 "dim gray" "Gill Sans" 'swiss 'normal 'bold #f)
   880 152 scene))

;draw-puntos : number image --> image
;Pinta el puntaje actual del juego:
(define (draw-puntos puntos scene)
  (place-image
   (text/font
    (number->string puntos) 24 "dim gray" "Gill Sans" 'swiss 'normal 'bold #f)
   880 184 scene))

;contar : T --> T
;Aumenta el tiempo para que transcurra durante el juego.
(define (contar t)
  (cond
    [(= (floor (T-segundos t)) 60) (make-T (add1 (T-minutos t)) 0 (T-posn t))]
    [else (make-T (T-minutos t) (+ (T-segundos t) 0.015) (T-posn t))]))

;contabilizar : mundo --> mundo
;Permite transcurrir el tiempo de un mundo durante el juego.
(define (contabilizar mundo)
  (make-mundo (mundo-disparador mundo)
              (mundo-proyectil mundo)
              (mundo-ejercito mundo)
              (contar (mundo-tiempo mundo))
              (mundo-siguientes mundo)
              (mundo-bala mundo)
              (mundo-dificultad mundo)
              (mundo-puntaje mundo)))

;mover : mundo number --> mundo
;Cambia las posiciones del disparador y el proyectil en caso de no haberse disparado.
(define (mover mundo delta)
  (contabilizar
   (make-mundo
    (make-shooter (shooter-imagen (mundo-disparador mundo))
                  (make-posn (+ (posn-x (shooter-posicion (mundo-disparador mundo))) delta) (posn-y (shooter-posicion (mundo-disparador mundo)))))
    (make-burbuja (burbuja-imagen (mundo-proyectil mundo)) (if (= (posn-y (burbuja-posicion (mundo-proyectil estado)))
                                                                 (posn-y (burbuja-posicion (mundo-proyectil mundo))))
                                                              (make-posn (+ (posn-x (burbuja-posicion (mundo-proyectil mundo))) delta)
                                                                         (posn-y (burbuja-posicion (mundo-proyectil mundo))))
                                                              (make-posn (posn-x (burbuja-posicion (mundo-proyectil mundo)))
                                                                         (posn-y (burbuja-posicion (mundo-proyectil mundo))))))
    (mundo-ejercito mundo)
    (mundo-tiempo mundo)
    (mundo-siguientes mundo)
    (mundo-bala mundo)
    (mundo-dificultad mundo)
    (mundo-puntaje mundo))))

;disparar: mundo number --> mundo
;Cambia la posición en Y del proyectil.
(define (disparar mundo alfa)
  (make-mundo
   (mundo-disparador mundo)
   (make-burbuja (burbuja-imagen (first (mundo-siguientes mundo))) (burbuja-posicion (mundo-proyectil mundo)))
   (mundo-ejercito mundo)
   (mundo-tiempo mundo)
   (colorear-burbujas (mundo-siguientes mundo) (cond
                                             [(string=? (mundo-dificultad mundo) "Novato") c-novato]
                                             [(string=? (mundo-dificultad mundo) "Medio") c-medio]
                                             [(string=? (mundo-dificultad mundo) "Experto") c-experto]
                                             [(string=? (mundo-dificultad mundo) "Maestro") c-maestro])
                      (cond
                                             [(string=? (mundo-dificultad mundo) "Novato") c-novato]
                                             [(string=? (mundo-dificultad mundo) "Medio") c-medio]
                                             [(string=? (mundo-dificultad mundo) "Experto") c-experto]
                                             [(string=? (mundo-dificultad mundo) "Maestro") c-maestro]))                     
   (cons (make-burbuja (burbuja-imagen (mundo-proyectil mundo)) (make-posn (posn-x (burbuja-posicion (mundo-proyectil mundo)))
                                                                          (+ (posn-y (burbuja-posicion (mundo-proyectil mundo))) alfa)))
   (mundo-bala mundo))
   (mundo-dificultad mundo)
   (mundo-puntaje mundo)))

;nivelar: mundo --> mundo
;Cambia la dificultad actual del juego en ejecución en determinada parte del tiempo.
(define (nivelar mundo)
  (make-mundo
   (mundo-disparador mundo) (mundo-proyectil mundo) (mundo-ejercito mundo) (mundo-tiempo mundo) (mundo-siguientes mundo) (mundo-bala mundo)
   (cond
     [(= (floor (T-minutos (mundo-tiempo mundo))) 2) "Medio"]
     [(= (floor (T-minutos (mundo-tiempo mundo))) 4) "Experto"]
     [(= (floor (T-minutos (mundo-tiempo mundo))) 6) "Maestro"]
     [else (mundo-dificultad mundo)])
   (mundo-puntaje mundo)))     

;balear : lista-burbujas --> list-burbujas
;Cambia el estado en Y del proyectil después de ser disparado para elevarlo.
(define (balear list-burbujas)
  (list (make-burbuja (burbuja-imagen (first list-burbujas))
                      (make-posn (posn-x (burbuja-posicion (first list-burbujas)))
                      (+ (posn-y (burbuja-posicion (first list-burbujas))) -alfa)))))

;disparo? : mundo --> boolean
;Indica si dentro de un mundo se disparó el proyecil o no.
(define (disparo? mundo)
(if (empty? (mundo-bala mundo)) #false #true))

;volar: mundo --> mundo
;Eleva el proyectil de un mundo luego de ser disparado.
(define (volar mundo)
   (if (disparo? mundo)
       (if (frontal? (first (mundo-bala mundo))
                     (mundo-ejercito mundo))
           mundo
           (agregar (make-mundo
                     (mundo-disparador mundo)
                     (mundo-proyectil mundo)
                     (mundo-ejercito mundo)
                     (mundo-tiempo mundo)
                     (mundo-siguientes mundo)
                     (if (= 20 (posn-y (burbuja-posicion (first (mundo-bala mundo)))))
                         (mundo-bala mundo) (balear (mundo-bala mundo)))
                     (mundo-dificultad mundo)
                     (mundo-puntaje mundo)))) mundo))

;sumar32 : lista-burbujas --> lista-burbujas
;Suma 32 a la posición en Y de cada burbuja de la lista.
(define (sumar32 lista-burbujas)
  (cond
    [(empty? lista-burbujas) empty]
    [(cons? lista-burbujas) (cons (make-burbuja (burbuja-imagen (first lista-burbujas))
                                                (make-posn (posn-x (burbuja-posicion (first lista-burbujas)))
                                                           (+ 32 (posn-y (burbuja-posicion (first lista-burbujas))))))
                                  (sumar32 (rest lista-burbujas)))]))

;sumatoria32 : ejercito --> ejercito
;Suma 32 a las posiciones en Y de cada burbuja del ejército.
(define (sumatoria32 ejercito)
  (cond
    [(empty? ejercito) empty]
    [(cons? ejercito) (cons (sumar32 (first ejercito)) (sumatoria32 (rest ejercito)))]))

;bajar: mundo --> mundo
;Baja las burbujas del ejército y agrega una nueva list-burbujas al mismo en determinado tiempo del juego.
(define (bajar mundo)
  (if (= (floor (T-segundos (mundo-tiempo mundo))) 60)
  (make-mundo (mundo-disparador mundo)
              (mundo-proyectil mundo)
              (cons (if (= (posn-x (burbuja-posicion (first (first (mundo-ejercito mundo))))) 36)
                        (colorear-burbujas burbujas1
                                           (cond
                                             [(string=? (mundo-dificultad mundo) "Novato") (cons "black" c-novato)]
                                             [(string=? (mundo-dificultad mundo) "Medio") (cons "black" c-medio)]
                                             [(string=? (mundo-dificultad mundo) "Experto") (cons "black" c-experto)]
                                             [(string=? (mundo-dificultad mundo) "Maestro") (cons "black" c-maestro)])
                                           (cond
                                             [(string=? (mundo-dificultad mundo) "Novato") c-novato]
                                             [(string=? (mundo-dificultad mundo) "Medio") c-medio]
                                             [(string=? (mundo-dificultad mundo) "Experto") c-experto]
                                             [(string=? (mundo-dificultad mundo) "Maestro") c-maestro]))
                        (generar-burbujas (colorear-burbujas burbujas2 (cond
                                             [(string=? (mundo-dificultad mundo) "Novato") (cons "black" c-novato)]
                                             [(string=? (mundo-dificultad mundo) "Medio") (cons "black" c-medio)]
                                             [(string=? (mundo-dificultad mundo) "Experto") (cons "black" c-experto)]
                                             [(string=? (mundo-dificultad mundo) "Maestro") (cons "black" c-maestro)])
                                                             (cond
                                             [(string=? (mundo-dificultad mundo) "Novato") c-novato]
                                             [(string=? (mundo-dificultad mundo) "Medio") c-medio]
                                             [(string=? (mundo-dificultad mundo) "Experto") c-experto]
                                             [(string=? (mundo-dificultad mundo) "Maestro") c-maestro])) -alfa))
                    (sumatoria32 (mundo-ejercito mundo)))
              (mundo-tiempo mundo)
              (mundo-siguientes mundo)
              (mundo-bala mundo)
              (mundo-dificultad mundo)
              (mundo-puntaje mundo))
  mundo))

;buscar-burbuja : posn lista-burbujas --> burbuja
;Extrae una burbuja de una lista-burbujas dada una posición.
(define (buscar-burbuja posn list-burbujas)
  (cond
    [(empty? list-burbujas) empty]
    [(cons? list-burbujas) (if (and (= (posn-x (burbuja-posicion (first list-burbujas))) (posn-x posn))
                                    (= (posn-y posn) (posn-y (burbuja-posicion (first list-burbujas)))))
                               (first list-burbujas) (buscar-burbuja posn (rest list-burbujas)))]))

;busqueda-burbuja : posn ejercito --> burbuja
;Extrae una burbuja de un ejército de burbujas dada una posición.
(define (busqueda-burbuja posn ejercito)
  (cond
    [(empty? ejercito) empty]
    [(cons? ejercito) (if (empty? (buscar-burbuja posn (first ejercito)))
                          (busqueda-burbuja posn (rest ejercito))
                          (buscar-burbuja posn (first ejercito)))]))

;posn-lista?: posn lista-burbujas --> boolean
;Indica si la posición hace parte de alguna de las burbujas de una lista-burbujas o no.
(define (posn-lista? posn list-burbujas)
  (cond
    [(empty? list-burbujas) #false]
    [(cons? list-burbujas) (if (and (= (posn-y (burbuja-posicion (first list-burbujas))) (posn-y posn))
                                    (= (posn-x (burbuja-posicion (first list-burbujas))) (posn-x posn)))
                               #true (posn-lista? posn (rest list-burbujas)))]))

;posn-ejercito?: posn ejercito --> boolean
;Indica si la posición hace parte de alguna de las burbujas de un ejército de burbujas o no.
(define (posn-ejercito? posn ejercito)
  (cond
    [(empty? ejercito) #false]
    [(cons? ejercito) (if (posn-lista? posn (first ejercito))
                          #true (posn-ejercito? posn (rest ejercito)))]))

;frontal?: burbuja ejercito --> boolean
;Indica si hay una burbuja dentro del ejército que esté arriba de la burbuja dada. 
(define (frontal? burbuja ejercito)
  (if (or (posn-ejercito? (make-posn (- (posn-x (burbuja-posicion burbuja)) 16)
                                 (- (posn-y (burbuja-posicion burbuja)) 32)) ejercito)
          (posn-ejercito? (make-posn (+ (posn-x (burbuja-posicion burbuja)) 16)
                                 (- (posn-y (burbuja-posicion burbuja)) 32)) ejercito)
          (posn-ejercito? (make-posn (posn-x (burbuja-posicion burbuja))
                                 (- (posn-y (burbuja-posicion burbuja)) 32)) ejercito))
      #true #false))

;unir : burbuja ejercito --> ejercito
;Agrega una burbuja dentro de un ejército de burbujas.
(define (unir burbuja ejercito)
  (cons (cons burbuja (first ejercito)) (rest ejercito)))

;agregar : mundo --> mundo
;Agrega una burbuja disparada a un ejército de burbujas en caso de llegar hasta el mismo.
(define (agregar mundo)
  (if (frontal? (first (mundo-bala mundo)) (mundo-ejercito mundo))
      (make-mundo (mundo-disparador mundo)
                  (mundo-proyectil mundo)
                  (unir (first (mundo-bala mundo)) (mundo-ejercito mundo))
                  (mundo-tiempo mundo)
                  (mundo-siguientes mundo)
                  empty
                  (mundo-dificultad mundo)
                  (mundo-puntaje mundo))
      mundo))

;crear-radar : burbuja --> lista-posn
;Crea una lista-posn a partir de las posiciones que hay alrededor del radio de explosión de una burbuja.
(define (crear-radar burbuja)
(list (make-posn (posn-x (burbuja-posicion burbuja)) (- (posn-y (burbuja-posicion burbuja)) 32)) 
      (make-posn (- (posn-x (burbuja-posicion burbuja)) 32) (posn-y (burbuja-posicion burbuja)))
      (make-posn (+ (posn-x (burbuja-posicion burbuja)) 32) (posn-y (burbuja-posicion burbuja)))
      (make-posn (- (posn-x (burbuja-posicion burbuja)) 16) (- (posn-y (burbuja-posicion burbuja)) 32))
      (make-posn (+ (posn-x (burbuja-posicion burbuja)) 16) (- (posn-y (burbuja-posicion burbuja)) 32))))

;filtrar-radar : lista-posn ejercito --> lista-posn
;Filtra las posiciones de una lista-posn dependiendo de si hacen parte o no de las burbujas de un ejército.
(define (filtrar-radar list-posn ejercito)
  (cond
    [(and (empty? list-posn) (empty? ejercito)) empty]
    [(empty? list-posn) empty]
    [(empty? ejercito) empty]
    [else (if (posn-ejercito? (first list-posn) ejercito) (cons (first list-posn) (filtrar-radar (rest list-posn) ejercito))
              (filtrar-radar (rest list-posn) ejercito))]))

;limpiar : list --> lista-burbujas
;Filtra las burbujas de una lista de burbujas y listas vacías.
(define (limpiar list-burbujas)
  (filter burbuja? list-burbujas))

;sacar-burbujas : lista-posn lista-burbujas --> lista-burbujas
;Saca las burbujas de una lista-burbujas y forma una nueva dada una lista-posn.
(define (sacar-burbujas list-posn list-burbujas)
  (cond
    [(and (empty? list-posn) (empty? list-burbujas)) empty]
    [(empty? list-posn) empty]
    [(empty? list-burbujas) empty]
    [else (limpiar (cons (buscar-burbuja (first list-posn) list-burbujas) (sacar-burbujas (rest list-posn) list-burbujas)))]))

;extraer-burbujas : list-posn ejercito --> lista-burbujas
;Extrae las burbujas de un ejército y forma una lista-burbujas dada una lista-posn.
(define (extraer-burbujas list-posn ejercito)
  (cond
    [(and (empty? list-posn) (empty? ejercito)) empty]
    [(empty? list-posn) empty]
    [(empty? ejercito) empty]
    [else (append (sacar-burbujas list-posn (first ejercito)) (extraer-burbujas list-posn (rest ejercito)))]))

;filtrar-color : burbuja lista-burbujas --> list-burbujas
;Filtra las burbujas de una lista-burbujas que sean del mismo color que una burbuja dada o que sean bombas.
(define (filtrar-color burbuja list-burbujas)
  (if (image=? (overlay (circle 15 "outline" "black")
                        (underlay/offset (circle 15 "solid" "black") -6 -6 
                        (overlay (circle 5 "outline" "black") (circle 5 "solid" "white" )))) (burbuja-imagen burbuja)) list-burbujas
  (cond
    [(empty? list-burbujas) empty]
    [(cons? list-burbujas) (if (or (image=? (burbuja-imagen burbuja) (burbuja-imagen (first list-burbujas)))
                                   (image=? (overlay (circle 15 "outline" "black")
                                                     (underlay/offset (circle 15 "solid" "black") -6 -6 
                        (overlay (circle 5 "outline" "black") (circle 5 "solid" "white" )))) (burbuja-imagen (first list-burbujas))))
                              (cons (first list-burbujas) (filtrar-color burbuja (rest list-burbujas)))
                              (filtrar-color burbuja (rest list-burbujas)))])))

;extraer-posn : lista-burbujas --> lista-posn
;Extrae las posiciones de las burbujas de una lista-burbujas y crea una lista-posn.
(define (extraer-posn lista-burbujas)
  (cond
    [(empty? lista-burbujas) empty]
    [(cons? lista-burbujas) (cons (burbuja-posicion (first lista-burbujas)) (extraer-posn (rest lista-burbujas)))]))

;borrar : posn lista-burbujas --> list-burbujas
;Filtra las burbujas que tengan la posición dada de una lista-burbujas.
(define (borrar posn list-burbujas)
  (cond
    [(empty? list-burbujas) empty]
    [(cons? list-burbujas) (if (and (and (>= (posn-x (burbuja-posicion (first list-burbujas))) (- (posn-x posn) 16))
                                         (<= (posn-x (burbuja-posicion (first list-burbujas))) (+ 16 (posn-x posn))))
                                    (= (posn-y posn) (posn-y (burbuja-posicion (first list-burbujas))))) (rest list-burbujas)
                                        (cons (first list-burbujas) (borrar posn (rest list-burbujas))))]))

;exterminar : posn ejercito --> ejercito
;Filtra las burbujas que tengan la posición dada de un ejército de burbujas.
(define (exterminar posn ejercito)
  (cond
    [(empty? ejercito) empty]
    [(cons? ejercito) (cons (borrar posn (first ejercito)) (exterminar posn (rest ejercito)))]))

;estallar : lista-posn lista-burbujas --> list-burbujas
;Elimina las burbujas que tengan las posiciones de la lista-posn en la lista-burbujas.
(define (estallar list-posn list-burbujas)
  (cond
    [(and (empty? list-burbujas) (empty? list-posn)) empty]
    [(empty? list-burbujas) empty]
    [(empty? list-posn) list-burbujas]
    [else (estallar (rest list-posn) (exterminar (first list-posn) list-burbujas))]))

;bomba? : lista-posn ejercito --> boolean
;Indica si hay una bomba dentro de una lista de posiciones dado un ejercito.
(define (bomba? list-posn ejercito)
  (cond
    [(and (empty? ejercito) (empty? list-posn)) #false]
    [(empty? ejercito) #false]
    [(empty? list-posn) #false]   
    [else (if (image=? (overlay (circle 15 "outline" "black")
                                                 (underlay/offset (circle 15 "solid" "black") -6 -6 
                        (overlay (circle 5 "outline" "black") (circle 5 "solid" "white" )))) (burbuja-imagen (first (extraer-burbujas list-posn ejercito))))
                              #true (bomba? (rest list-posn) ejercito))]))

;preparar : ejercito --> list-posn
;Crea una lista-posn dado un ejército. 
(define (preparar ejercito) (extraer-posn (filtrar-color (first (first ejercito)) (extraer-burbujas
        (filtrar-radar (crear-radar (first (first ejercito))) ejercito) ejercito))))

;contar-ejercito: ejercito --> number
;Cuenta la cantidad de burbujas dentro de un ejército:
(define (contar-ejercito ejercito)
  (cond
    [(empty? ejercito) 0]
    [(cons? ejercito) (+ (cantidad-elementos (first ejercito)) (contar-ejercito (rest ejercito)))]))

;explotar : mundo --> mundo
;Elimina las burbujas que hayan sido alcanzadas por el proyectil y cumplan con las codiciones de explosión durante el juego.
(define (explotar mundo) 
  (if (frontal? (first (first (mundo-ejercito mundo))) (mundo-ejercito mundo))
      (make-mundo (mundo-disparador mundo)
                  (mundo-proyectil mundo)
                  (if (bomba? (preparar (mundo-ejercito mundo)) (mundo-ejercito mundo))
                      (estallar (cons (burbuja-posicion (first (first (mundo-ejercito mundo))))
                            (preparar (mundo-ejercito mundo))) (mundo-ejercito mundo))
                  (if (>= (add1 (cantidad-elementos (preparar (mundo-ejercito mundo)))) 3)
                      (estallar (cons (burbuja-posicion (first (first (mundo-ejercito mundo))))
                            (preparar (mundo-ejercito mundo))) (mundo-ejercito mundo))
                      (mundo-ejercito mundo)))
                  (mundo-tiempo mundo)
                  (mundo-siguientes mundo)
                  (mundo-bala mundo)
                  (mundo-dificultad mundo)
                  (cantidad-elementos
                   (cons (burbuja-posicion (first (first (mundo-ejercito mundo))))
                             (preparar (mundo-ejercito mundo)))))
      mundo))

;zona-radar : posn --> lista-posn
;Crea una lista-posn a partir de las posiciones que hay alrededor de una posición dada.
(define (zona-radar posn)
(list (make-posn (posn-x posn) (- (posn-y posn) 32))
      (make-posn (posn-x posn) (+ (posn-y posn) 32))
      (make-posn (- (posn-x posn) 32) (posn-y posn))
      (make-posn (+ (posn-x posn) 32) (posn-y posn))
      (make-posn (- (posn-x posn) 16) (- (posn-y posn) 32))
      (make-posn (+ (posn-x posn) 16) (- (posn-y posn) 32))
      (make-posn (- (posn-x posn) 16) (+ (posn-y posn) 32))
      (make-posn (+ (posn-x posn) 16) (+ (posn-y posn) 32))))

;distribuir-radar : lista-posn --> lista-posn
;Crea una lista-posn a partir de los radares de cada posición de una lista-posn dada.
(define (distribuir-radar list-posn)
  (cond
    [(empty? list-posn) empty]
    [(cons? list-posn) (append (zona-radar (first list-posn)) (distribuir-radar (rest list-posn)))]))

;___________________________________________MANEJADORAS___________________________________________

; TO-DRAW:

;draw-game : mundo --> image
;Pinta los cambios que recibe el mundo durante el juego:
(define (draw-game mundo)
  (draw-puntos (mundo-puntaje mundo)
  (draw-dificultad (mundo-dificultad mundo) 
  (draw-burbujas (mundo-bala mundo) (draw-burbujas (mundo-siguientes mundo)                                                   
    (draw-ejercito (mundo-ejercito mundo) (place-image (burbuja-imagen (mundo-proyectil mundo))
                                          (posn-x (burbuja-posicion (mundo-proyectil mundo))) (posn-y (burbuja-posicion (mundo-proyectil mundo)))
     (draw-time (mundo-tiempo mundo) (place-image (shooter-imagen (mundo-disparador mundo))
                            (posn-x (shooter-posicion (mundo-disparador mundo)))
                            (posn-y (shooter-posicion (mundo-disparador mundo))) escena)))))))))

; ON-TICK:

;actualizar : mundo --> mundo
;Cambia algunos atrubutos del mundo a medida que transcurre el tiempo durante el juego.
(define (actualizar mundo)
  (nivelar (explotar (volar (bajar (contabilizar mundo))))))

; ON-KEY:

;controlar : mundo key --> mundo
;Permite controlar el juego mediante el teclado.
(define (controlar mundo key)
  (cond
    [(key=? key "left") (if (>= (posn-x (shooter-posicion (mundo-disparador mundo))) 20.5) (mover mundo -delta) mundo)]
    [(key=? key "right") (if (<= (posn-x (shooter-posicion (mundo-disparador mundo))) 622) (mover mundo +delta) mundo)]
    [(key=? key " ") (if (frontal? (mundo-proyectil mundo) (mundo-ejercito mundo))
                          mundo (disparar mundo -alfa))]
    [else mundo]))

; STOP-WHEN:

;detener? : mundo --> boolean
;Detiene el juego en caso de limpiar el ejército de burbujas o de que ellas lleguen hasta el disparador.
(define (detener? mundo)
(if (or (frontal? (mundo-proyectil mundo) (mundo-ejercito mundo))
        (empty? (mundo-ejercito mundo))) #true #false))  

;___________________________________________EVENTOS___________________________________________

(big-bang estado
          (to-draw draw-game)
          (on-tick actualizar (/ 1 100))
          (on-key controlar)
          (stop-when detener?))
















