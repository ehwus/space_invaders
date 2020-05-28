;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define TANK-Y (- HEIGHT 20))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define MTS (empty-scene WIDTH HEIGHT))
(define BLANK (square 0 "solid" "white"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;; empty
;; (cons Missile ListOfMissile)
(define LOM1 empty)
(define LOM2 (cons M1 empty))
(define LOM3 (cons M1 (cons M2 empty)))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (first lom)
                   (fn-for-lom (rest lom)))]))

;; Template rules used:
;; one of: 2 cases
;; atomic distinct: empty
;; compound: (cons Missile ListOfMissile)

;; ListOfInvader is one of:
;; empty
;; (cons Invader ListOfInvader)
(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I1 (cons I2 empty)))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (first loi)
                   (fn-for-loi (rest loi)))]))

;; Template rules used:
;; one of: 2 cases
;; atomic distinct: empty
;; compound: (cons Invader ListOfInvader)


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =================
;;;;;;;;;;;;;;;;;;;;
;;
;; GAME FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;


;; Game -> Game
;; start the world with (main G0)
;; 
(define (main g)
  (big-bang g                   ; Game
    (on-tick   update-game)     ; Game -> Game
    (to-draw   render-game)     ; Game -> Image
    (on-key    command-tank)    ; Game KeyEvent -> Game
    (stop-when   game-over?)))  ; Game -> Boolean


;; Game -> Game
;; compute next frame, including all unit/missile movement and spawning/despawning

;(define (update-game g) g)   ; stub
(define (update-game g)
  (make-game (despawn-invaders (spawn-invaders (update-invaders (game-invaders g))) (game-missiles g))
             (despawn-missiles (update-missiles (game-missiles g)))
             (update-tank        (game-tank  g))))

;; ListOfInvaders -> ListOfInvaders
;; Spawn invaders based on Invade Rate, random element so testing manually

;(define (spawn-invaders loi) loi) ; stub

(define (spawn-invaders loi)
  (cond [(< (random 1000) INVADE-RATE)
         (cons (make-invader (random WIDTH) 0 -5) loi)]
        [else loi]))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Despawn invaders that are hit by missiles
(check-expect (despawn-invaders LOI1 LOM1) empty)
(check-expect (despawn-invaders LOI3 (list M3)) (list I2))

;(define (despawn-invaders loi lom) loi) ; stub
(define (despawn-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi  ]
        [else (if  (collided? (first loi) lom)
                   (despawn-invaders (rest loi) lom)
                   (cons (first loi)
                         (despawn-invaders (rest loi) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; return true if ship has been hit by missile
(check-expect (collided? I1 (list (make-missile 150 100))) true)

;(define (collided? i lom) false) ; stub
(define (collided? i lom)
  (cond [(empty? lom) false]
        [else (if (and (< (- (invader-x i) HIT-RANGE) (missile-x (first lom)) (+ (invader-x i) HIT-RANGE))
                       (< (- (invader-y i) HIT-RANGE) (missile-y (first lom)) (+ (invader-y i) HIT-RANGE)))
                  true
                  (collided? i (rest lom)))]))

          
;; ListOfMissile -> ListOfMissile
;; Despawn missiles > height
(check-expect (despawn-missiles empty) empty)
(check-expect (despawn-missiles (cons (make-missile 50 10) empty))
              (cons (make-missile 50 10) empty))
(check-expect (despawn-missiles (cons (make-missile 50 -10) (cons (make-missile 50 10) empty)))
              (cons (make-missile 50 10) empty))

;(define (despawn-missiles lom) lom) ; stub
(define (despawn-missiles lom)
  (cond [(empty? lom) empty]
        [(> 0 (missile-y (first lom)))
         (despawn-missiles (rest lom))]
        [else (cons (first lom)
                    (despawn-missiles (rest lom)))]))


;; ListOfInvader -> ListOfInvader
;; Moves, spawns and despawns new invaders
(check-expect (update-invaders empty) empty)
(check-expect (update-invaders (cons (make-invader 100 50 12) empty))                                 ; middle of screen
              (cons (make-invader (+ 100 (* 12 INVADER-X-SPEED)) (+ 50 INVADER-Y-SPEED) 12) empty))
(check-expect (update-invaders (cons (make-invader WIDTH 100 1) empty))                               ; right edge
              (cons (make-invader (- WIDTH 1) (+ INVADER-Y-SPEED 100) -1) empty))
(check-expect (update-invaders (cons (make-invader 0 100 -8) empty))                                  ; left edge
              (cons (make-invader 1 (+ INVADER-Y-SPEED 100) 8) empty))

;(define (update-invaders loi) loi) ; stub

(define (update-invaders loi)
  (cond [(empty? loi) empty]
        [else (cond [(>= 0 (invader-x (first loi)))
                     (cons (make-invader 1
                                         (+ INVADER-Y-SPEED (invader-y (first loi)))
                                         (* -1 (invader-dx (first loi))))
                           (update-invaders (rest loi)))]                    
                    [(<= WIDTH (invader-x (first loi)))
                     (cons (make-invader (- WIDTH 1)
                                         (+ INVADER-Y-SPEED (invader-y (first loi)))
                                         (* -1 (invader-dx (first loi))))
                           (update-invaders (rest loi)))]                    
                    [else  (cons (make-invader (+ (* (invader-dx (first loi)) INVADER-X-SPEED) (invader-x (first loi)))
                                               (+ INVADER-Y-SPEED (invader-y (first loi)))
                                               (invader-dx (first loi)))
                                 (update-invaders (rest loi)))])]))

;(update-invaders (rest loi)))]))

;; ListOfMissiles -> ListOfMissiles
;; Moves missiles upwards at MISSILE-SPEED
(check-expect (update-missiles empty) empty)
(check-expect (update-missiles LOM2) (cons (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)) empty))
(check-expect (update-missiles LOM3) (cons (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))
                                           (cons (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED )) empty)))
 
;(define (update-missiles lom) lom) ; stub
(define (update-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
                    (update-missiles (rest lom)))]))

;; Tank -> Tank
;; Moves tank unit by TANK-SPEED * DIR, bouncing off edges of screen
(check-expect (update-tank T1) (make-tank (+ (tank-x T1) (* TANK-SPEED (tank-dir T1))) (tank-dir T1)))
(check-expect (update-tank T2) (make-tank (+ (tank-x T2) (* TANK-SPEED (tank-dir T2))) (tank-dir T2)))
(check-expect (update-tank (make-tank WIDTH 1)) (make-tank (- WIDTH 1) -1))
(check-expect (update-tank (make-tank 0 -1)) (make-tank 1 1))

;(define (update-tank t) t) ; stub
(define (update-tank t)
  (cond [(>= 0 (tank-x t))
         (make-tank 1 1)]
        [(<= WIDTH (tank-x t))
         (make-tank (- WIDTH 1) -1)]
        [else (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) 
                         (tank-dir t))]))
        
;; Game -> Boolean
;; Return true if any UFO has reached HEIGHT
(check-expect (game-over? G0) false)
(check-expect (game-over? G1) false)
(check-expect (game-over? G2) false)
(check-expect (game-over? G3) true)

;(define (game-over? g) false)  ; stub
(define (game-over? g)
  (below-height? (game-invaders g)))

;; ListOfInvaders -> Boolean
;; Return true if any invader is at Y of height or below
(check-expect (below-height? empty) false)
(check-expect (below-height? (cons (make-invader 100 100 15) empty)) false)
(check-expect (below-height? (cons (make-invader 100 100 15) (cons (make-invader 100 (+ 5 HEIGHT) 15) empty))) true)

;(define (below-height? loi) false) ; stub
(define (below-height? loi)
  (cond [(empty? loi) false]
        [else (if  (<= HEIGHT (invader-y (first loi)))
                   true
                   (below-height? (rest loi)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RENDERING FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Game -> Image
;; render the next frame
(check-expect (render-game (make-game empty empty T0))                      ; Base case, as there will always be a tank
              (place-image BLANK 0 0
                           (place-image BLANK 0 0
                                        (place-image TANK (tank-x T0) TANK-Y MTS))))

(check-expect (render-game (make-game empty empty T1))                      ; Different tank position
              (place-image BLANK 0 0
                           (place-image BLANK 0 0
                                        (place-image TANK (tank-x T1) TANK-Y MTS))))

(check-expect (render-game (make-game (list I1) (list M1) T1))              ; 1 invader, 1 missile
              (place-image INVADER 150 100
                           (place-image BLANK 0 0
                                        (place-image MISSILE 150 300
                                                     (place-image BLANK 0 0
                                                                  (place-image TANK (tank-x T1) TANK-Y MTS))))))

;(define (render-game g) MTS)    ; stub
;<take template from Game>
(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank         (game-tank g)))))

;; ListOfInvaders -> Image
;; Render a list of invaders at correct X Y on screen

(check-expect (render-invaders empty MTS) (place-image BLANK 0 0 MTS))
(check-expect (render-invaders LOI2 MTS) (place-image INVADER (invader-x I1) (invader-y I1)
                                                      (place-image BLANK 0 0 MTS)))
(check-expect (render-invaders LOI3 MTS) (place-image INVADER (invader-x I2) (invader-y I2)
                                                      (place-image INVADER (invader-x I1) (invader-y I1)
                                                                   (place-image BLANK 0 0 MTS))))

;(define (render-invaders loi) (place-image BLANK 0 0 MTS))    ; stub
;<use template from LOI>
(define (render-invaders loi mts)
  (cond [(empty? loi) (place-image BLANK 0 0 mts)]
        [else (place-image INVADER (invader-x (first loi)) (invader-y (first loi))
                           (render-invaders (rest loi) mts))]))

;; ListOfMissiles -> Image
;; Render a list of missiles at correct XY on screen
(check-expect (render-missiles empty MTS) (place-image BLANK 0 0 MTS))
(check-expect (render-missiles LOM2 MTS) (place-image MISSILE 150 300
                                                      (place-image BLANK 0 0 MTS)))
(check-expect (render-missiles LOM3 MTS) (place-image MISSILE (missile-x M1) (missile-y M1)
                                                      (place-image MISSILE (missile-x M2) (missile-y M2)
                                                                   (place-image BLANK 0 0 MTS))))

;(define (render-missiles lom mts) (place-image BLANK 0 0 MTS))    ; stub
;<use template from LOM>
(define (render-missiles lom mts)
  (cond [(empty? lom) (place-image BLANK 0 0 mts)]
        [else (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))
                           (render-missiles (rest lom) mts))]))


;; Game -> Image
;; Render a tank on screen at correct X and TANK-Y
(check-expect (render-tank T0) (place-image TANK (tank-x T0) TANK-Y MTS))
(check-expect (render-tank T1) (place-image TANK (tank-x T1) TANK-Y MTS))

;(define (render-tank g) (place-image BLANK 0 0 MTS))    ; stub
;<use template for Tank>
(define (render-tank t)
  (place-image TANK (tank-x t) TANK-Y MTS))


;;;;;;;;;;;;;;;;;;
;;
;; KEY FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;

;; Game KeyEvent -> Game
;; change tank direction to left or right with arrow keys, shoot with space


(check-expect (command-tank G0 "a") (make-game empty empty (make-tank (tank-x (game-tank G0)) 0)))  ; no valid input, no movement
(check-expect (command-tank G0 "left") (make-game empty empty (make-tank (/ WIDTH 2) -1)))          ; right to left
(check-expect (command-tank G0 "right") (make-game empty empty (make-tank (/ WIDTH 2) 1)))          ; right stays right
(check-expect (command-tank
               (make-game (list I1 I2) (list M1 M2) (make-tank 50 -1)) "right")
              (make-game (list I1 I2) (list M1 M2)  (make-tank 50  1)))                             ; left to right
(check-expect (command-tank
               (make-game (list I1 I2) (list M1 M2) (make-tank 50 -1)) "left")
              (make-game (list I1 I2) (list M1 M2)  (make-tank 50 -1)))                             ; left stays left
(check-expect (command-tank (make-game empty empty T0) " ")                                
              (make-game empty (cons (make-missile (tank-x T0) TANK-Y) empty) T0))                  ; shoot from T0
(check-expect (command-tank (make-game empty empty T1) " ")
              (make-game empty (cons (make-missile (tank-x T1) TANK-Y) empty) T1))                  ; shoot from T1

;(define (command-tank g ke) game)    ; stub
;<use key template, add conditions for left right space>

(define (command-tank g ke)
  (cond [(key=? ke " ")     (make-game  (game-invaders g) (cons (make-missile (tank-x (game-tank g)) TANK-Y) (game-missiles g)) (game-tank g))]
        [(key=? ke "right") (make-game  (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g))  1))]
        [(key=? ke "left")  (make-game  (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [else (make-game  (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 0))]))





