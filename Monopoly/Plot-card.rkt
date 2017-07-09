#lang racket/gui

(provide (all-defined-out))


;-------------------------Purchaseable-plots---------------------------------------
;;PLOT
(define plot-card%
  (class object%
    (init-field 
     name
     price
     number
     p1coords
     p2coords
     [original-rent #f]
     [color #f]
     [occupant (make-hash)]
     [house-price "30"]
     [hotel-price "50"]
     [houses 0]
     [hotels 0]
     [owner #f]
     [rent original-rent])
    
    (define/public (get-name)
      name)
    (define/public (get-hotel-price)
      hotel-price)
    (define/public (get-p1coords)
      p1coords)
    (define/public (get-p2coords)
      p2coords)
    (define/public (get-price)
      price)
    (define/public (get-rent)
      rent)
    (define/public (get-house-price)
      house-price)
    (define/public (add-house this-ui player)
      (cond
        [(or (= houses 4)(= hotels 1))
         (send this-ui present "This plot is already at maximum capacity")]  
        [else 
         (set! houses (+ houses 1))
         (set! rent (+ original-rent (* houses 20) (* hotels 200)))
         (send player subtract-amount (string->number house-price) this-ui)
         (send this-ui present (string-append "You bought a house on " name ", "
                                              name " now has " (number->string houses) " house(s)"))
         (send (send this-ui get-window) refresh)]))
    
    (define/public (add-hotel this-ui player)
      (cond
        [(and (= houses 4) (= hotels 0)) 
         (set! hotels (+ hotels 1))
         (set! houses 0)
         (set! rent (+ original-rent (* houses 20) (* hotels 200)))
         (send player subtract-amount (string->number hotel-price) this-ui)
         (send this-ui present (string-append "You bought a hotel on " name))
         (send (send this-ui get-window) refresh)]
        [else (send this-ui present "You can't add a hotel")]))
    
    (define/public (get-owner)
      owner)
    (define/public (draw-proc canvas dc player1 player2)
      (let* ((our-pic (make-object bitmap% "images/monopoly.jpg"))
             (p1coord (send (send player1 get-place) get-p1coords))
             (p2coord (send (send player2 get-place) get-p2coords)))
        (send dc scale 0.5 0.5)
        (send dc draw-bitmap our-pic 0 0) 
        (send dc scale 2 2)
        (send dc set-brush "white" 'solid)
        (send dc set-pen "black" 0 'solid)
        (send dc draw-text "1" (car p1coord) (cdr p1coord))
        (send dc draw-text "2" (car p2coord) (cdr p2coord))
        (send dc draw-rectangle 300 0 190 300)
        (send dc set-brush color 'solid)
        (send dc draw-rectangle 310 10 170 60)
        (send dc draw-text name 350 30)
        (send dc draw-text (string-append "Price: $"
                                          (number->string 
                                           price)) 350 80)
        (send dc draw-text (string-append "Rent $" (number->string
                                                    rent)) 360 120)
        (send dc draw-text (string-append "With 1 House    $"(number->string
                                                              (+ original-rent 20))) 315 140)
        (send dc draw-text (string-append "With 2 Houses   $" (number->string
                                                               (+ original-rent 40))) 315 160)
        (send dc draw-text (string-append "With 3 Houses   $" (number->string
                                                               (+ original-rent 60))) 315 180)
        (send dc draw-text (string-append "With 4 Houses   $" (number->string
                                                               (+ original-rent 80))) 315 200)
        (send dc draw-text (string-append "With HOTEL    $" (number->string
                                                             (+ original-rent 200))) 315 220)
        (send dc draw-text (string-append "Houses cost " "$" house-price) 320 250)
        (send dc draw-text (string-append "Hotels cost " "$" hotel-price) 320 270)))
    
    (define/public (number-of-houses)
      houses)
    (define/public (hotel?)
      (if (= hotels 0) 
          #f
          #t))
    (define/public (get-number)
      number)
    (define/public (add player)
      (hash-set! occupant (send player get-name) player))
    (define/public (remove player)
      (hash-remove! occupant (send player get-name)))
    (define/public (get-occupant)
      occupant)
    (define/public (get-color)
      color)
    (define/public (set-owner player)
      (set! owner player))
    (define/public (dice-proc player this-ui)
      (cond
        [(or (eq? (send (send player get-place) get-owner) #f)
             (eq? (send (send player get-place) get-owner) player))
         (void)]
        [else              
         (send this-ui present (string-append "You must pay " (send owner get-name) " $"
                                              (number->string rent) " in rent for stepping on "
                                              name)) 
         (send player subtract-amount rent this-ui)
         (send owner add-amount rent)]))
    
    (super-new)))

;;STATION
(define station%
  (class plot-card%
    (inherit-field
     name
     price
     rent
     owner
     original-rent)
    
    (define/override (add-house this-ui player)
      (send this-ui present "You can't build a house here"))
    (define/override (draw-proc canvas dc player1 player2)
      (let* ((my-pic (make-object bitmap% "images/train.jpg"))
             (our-pic (make-object bitmap% "images/monopoly.jpg"))
             (p1coord (send (send player1 get-place) get-p1coords))
             (p2coord (send (send player2 get-place) get-p2coords)))
        (send dc scale 0.5 0.5)
        (send dc draw-bitmap our-pic 0 0)
        (send dc scale 2 2)
        (send dc set-brush "white" 'solid)
        (send dc set-pen "black" 0 'solid)
        (send dc draw-text "1" (car p1coord) (cdr p1coord))
        (send dc draw-text "2" (car p2coord) (cdr p2coord))
        (send dc draw-rectangle 300 0 190 300)
        (send dc scale 0.5 0.5)
        (send dc draw-bitmap my-pic 670 200)
        (send dc scale 2 2)
        (send dc set-font (make-object font% 15 'decorative))
        (send dc draw-text name 330 30)
        (send dc draw-text (string-append "Price:$"
                                          (number->string price)) 325 200)
        (send dc draw-text "Rent:" 325 220)
        (send dc set-font (make-object font% 10 'decorative))
        (send dc draw-text (string-append "With 1 station:$" 
                                          (number->string rent)) 325 240)
        (send dc draw-text (string-append "With 2 stations:$"
                                          (number->string (* rent 2))) 325 255)
        (send dc set-font normal-control-font)))
    (define/override (dice-proc player this-ui)
      (cond 
        [(or (eq? owner #f)
             (eq? owner player))
         (void)]
        [(cond
           [(and (eq? name "West-Station")(not (eq? (send owner owned-plot? "East-Station") #f)))
            (send this-ui present (string-append "You must pay " (send owner get-name) " $"
                                                 (number->string (* rent 2)) " in rent for stepping on "
                                                 name)) 
            (send player subtract-amount (* rent 2) this-ui)
            (send owner add-amount (* rent 2))]
           [(and (eq? name "East-Station")(not (eq? (send owner owned-plot? "West-Station") #f)))
            (send this-ui present (string-append "You must pay " (send owner get-name) " $"
                                                 (number->string (* rent 2)) " in rent for stepping on "
                                                 name)) 
            (send player subtract-amount (* rent 2) this-ui)
            (send owner add-amount (* rent 2))]
           [else (send this-ui present (string-append "You must pay " (send owner get-name) " $"
                                                      (number->string rent) " in rent for stepping on "
                                                      name)) 
                 (send player subtract-amount rent this-ui)
                 (send owner add-amount rent)])]))
    (super-new)))

;;WATER-WORKS
(define ww%
  (class plot-card%
    (inherit-field
     name
     price
     rent
     owner
     original-rent)
    (define/override (add-house this-ui player)
      (send this-ui present "You can't build a house here"))
    (define/override (draw-proc canvas dc player1 player2)
      (let* ((our-pic (make-object bitmap% "images/monopoly.jpg"))
             (my-pic (make-object bitmap% "images/tap.png"))
             (p1coord (send (send player1 get-place) get-p1coords))
             (p2coord (send (send player2 get-place) get-p2coords)))
        (send dc scale 0.5 0.5)
        (send dc draw-bitmap our-pic 0 0) 
        (send dc scale 2 2)
        (send dc set-brush "white" 'solid)
        (send dc set-pen "black" 0 'solid)
        (send dc draw-text "1" (car p1coord) (cdr p1coord))
        (send dc draw-text "2" (car p2coord) (cdr p2coord))
        (send dc draw-rectangle 300 0 190 300)
        (send dc scale 0.8 0.8)
        (send dc draw-bitmap my-pic 380 115)
        (send dc scale (/ 5 4) (/ 5 4))
        (send dc set-font (make-object font% 20 'decorative))
        (send dc draw-text name 310 50)
        (send dc set-font normal-control-font)
        (send dc draw-text (string-append "Price:$"
                                          (number->string price)) 360 230)
        (send dc draw-text "Rent raised by $10 for each turn" 310 245)))
    (define/override (dice-proc player this-ui)
      (set! rent (* 10 (send this-ui get-turn)))
      (cond
        [(or (eq? owner #f)(eq? owner player)) (void)]
        [else
         (send this-ui present (string-append
                                "You stepped on " 
                                name 
                                ", which is owned by " 
                                (send owner get-name)
                                ". You must pay $" 
                                (number->string rent)
                                " in rent."))
         (send player subtract-amount rent this-ui)
         (send owner add-amount rent)]))
    (super-new)))

;Electric
(define electric%
  (class plot-card%
    (inherit-field
     name
     price
     rent
     owner
     original-rent)
    (define/override (add-house this-ui player)
      (send this-ui present "You can't build a house here"))
    (define/override (draw-proc canvas dc player1 player2)
      (let* ((our-pic (make-object bitmap% "images/monopoly.jpg"))
             (my-pic (make-object bitmap% "images/lightbulb.png"))
             (p1coord (send (send player1 get-place) get-p1coords))
             (p2coord (send (send player2 get-place) get-p2coords)))
        (send dc scale 0.5 0.5)
        (send dc draw-bitmap our-pic 0 0) 
        (send dc scale 2 2)
        (send dc set-brush "white" 'solid)
        (send dc set-pen "black" 0 'solid)
        (send dc draw-text "1" (car p1coord) (cdr p1coord))
        (send dc draw-text "2" (car p2coord) (cdr p2coord))
        (send dc draw-rectangle 300 0 190 300)
        (send dc scale 0.5 0.5)
        (send dc draw-bitmap my-pic 650 115)
        (send dc scale 2 2)
        (send dc set-font (make-object font% 17 'decorative))
        (send dc draw-text name 305 30)
        (send dc set-font normal-control-font)
        (send dc draw-text (string-append "Price:$"
                                          (number->string price)) 360 230)
        (send dc draw-text "50% of players balance as rent" 310 245)))
    (define/override (dice-proc player this-ui)
      (set! rent (quotient (send player get-balance) 2))
      (cond
        [(or (eq? owner #f)(eq? owner player)) (void)]
        [else
         (send this-ui present (string-append
                                "You stepped on " 
                                name 
                                ", which is owned by " 
                                (send owner get-name)
                                ". You must pay $" 
                                (number->string rent)
                                " in rent."))
         (send player subtract-amount rent this-ui)
         (send owner add-amount rent)]))
    (super-new)))




;--------------------------------------Custom-plots------------------------------------

;Custom-base-plot
(define custom-plot%
  (class object%
    (init-field
     number
     p1coords
     p2coords
     [occupant (make-hash)])
    
    (define/public (get-number)
      number)
    (define/public (get-p1coords)
      p1coords)
    (define/public (get-p2coords)
      p2coords)
    (define/public (get-price)
      0)
    (define/public (get-owner)
      "None")
    (define/public (number-of-houses)
      #f)
    (define/public (add player)
      (hash-set! occupant (send player get-name) player))
    (define/public (remove player)
      (hash-remove! occupant (send player get-name)))
    
    (super-new)))

;;GO
(define go-plot%
  (class custom-plot%
    (inherit-field
     number
     p1coords
     p2coords
     occupant)
    (define/public (dice-proc player this-ui)
      (send this-ui present "GO!"))
    (define/public (draw-proc canvas dc player1 player2)
      (let* ((our-pic (make-object bitmap% "images/monopoly.jpg"))
             (p1coord (send (send player1 get-place) get-p1coords))
             (p2coord (send (send player2 get-place) get-p2coords)))
        (send dc scale 0.5 0.5)
        (send dc draw-bitmap our-pic 0 0) 
        (send dc scale 2 2)
        (send dc set-brush "white" 'solid)
        (send dc set-pen "black" 0 'solid)
        (send dc draw-text "1" (car p1coord) (cdr p1coord))
        (send dc draw-text "2" (car p2coord) (cdr p2coord))
        (send dc draw-rectangle 300 0 190 300)
        (send dc set-font (make-object font% 50 'decorative))
        (send dc draw-text "GO" 340 100)
        (send dc set-font normal-control-font)))
    (super-new)))

;;FREE PARKING

(define free-parking%
  (class custom-plot%
    (inherit-field
     number
     p1coords
     p2coords
     occupant)
    (init-field
     [balance 0])
    (define/public (get-balance)
      balance)
    (define/public (draw-proc canvas dc player1 player2)
      (let* ((my-pic (make-object bitmap% "images/Free-Parking.jpg"))
             (our-pic (make-object bitmap% "images/monopoly.jpg"))
             (p1coord (send (send player1 get-place) get-p1coords))
             (p2coord (send (send player2 get-place) get-p2coords)))
        (send dc scale 0.5 0.5)
        (send dc draw-bitmap our-pic 0 0)
        (send dc scale 2 2)
        (send dc set-brush "white" 'solid)
        (send dc set-pen "black" 0 'solid)
        (send dc draw-text "1" (car p1coord) (cdr p1coord))
        (send dc draw-text "2" (car p2coord) (cdr p2coord))
        (send dc draw-rectangle 300 0 190 300)
        (send dc scale 0.25 0.25)
        (send dc draw-bitmap my-pic 1300 200)
        (send dc scale 4 4)))
    
    (define/public (dice-proc player this-ui)
      (send player add-amount balance)
      (send this-ui present 
            (string-append "You cashed in on the free parking pot and received $"
                           (number->string balance)))     
      (set! balance 0))
     (define/public (add-amount amount)
      (set! balance (+ balance amount)))
    (super-new)))


;;CHANCE
(define chance-funcs (make-hash))

(define (chance-func1 player this-ui)
  (send player add-amount 199)
  (send this-ui present "You inherit 199$"))
(hash-set! chance-funcs "1" chance-func1)

(define (chance-func2 player this-ui)
  (send this-ui present "Speeding ticket,you must pay 199$")
  (send player subtract-amount 199 this-ui)
  (send (send this-ui get-free-parking) add-amount 199))
(hash-set! chance-funcs "2" chance-func2)

(define (chance-func3 player this-ui)
  (send this-ui present 
        "You stepped on chance, and got yourself sent straight to prison")
  (send player set-jail))
(hash-set! chance-funcs "3" chance-func3)

(define (chance-func4 player this-ui)
  (send this-ui present "Dentist appointment: $150")
  (send player subtract-amount 150 this-ui)
  (send (send this-ui get-free-parking) add-amount 150))
(hash-set! chance-funcs "4" chance-func4)

(define (chance-func5 player this-ui)
  (send this-ui present "You got a bonus: $300")
  (send player add-amount 300))
(hash-set! chance-funcs "5" chance-func5)

(define chance%
  (class custom-plot%
    (inherit-field
     number
     p1coords
     p2coords
     occupant)
    (define/public (dice-proc current-player this-ui)
      (let ((x (+ (random (hash-count chance-funcs)) 1)))
        ((hash-ref chance-funcs (number->string x)) current-player this-ui)))
    (define/public (draw-proc canvas dc player1 player2)
      
      (let* ((our-pic (make-object bitmap% "images/monopoly.jpg"))
             (p1coord (send (send player1 get-place) get-p1coords))
             (p2coord (send (send player2 get-place) get-p2coords)))
        (send dc scale 0.5 0.5)
        (send dc draw-bitmap our-pic 0 0) 
        (send dc scale 2 2)
        (send dc set-brush "white" 'solid)
        (send dc set-pen "black" 0 'solid)
        (send dc draw-text "1" (car p1coord) (cdr p1coord))
        (send dc draw-text "2" (car p2coord) (cdr p2coord))
        (send dc draw-rectangle 300 0 190 300)
        (send dc set-font (make-object font% 100 'decorative))
        (send dc draw-text "?" 350 50)
        (send dc set-font normal-control-font)))
    
    (super-new)))

;;JAIL
(define jail%
  (class custom-plot%
    (inherit-field
     number
     p1coords
     p2coords
     occupant)
    (init-field
     jail-p1
     jail-p2)
    (define/public (get-p1-jail-coords)
      jail-p1)
    (define/public (get-p2-jail-coords)
      jail-p2)
    (define/public (draw-proc canvas dc player1 player2)
      (let* ((our-pic (make-object bitmap% "images/monopoly.jpg"))
             (my-pic (make-object bitmap% "images/jail.jpg"))
             (p1coord (send (send player1 get-place) get-p1coords))
             (p2coord (send (send player2 get-place) get-p2coords)))
        (send dc scale 0.5 0.5)
        (send dc draw-bitmap our-pic 0 0) 
        (send dc scale 2 2)
        (send dc set-brush "white" 'solid)
        (send dc set-pen "black" 0 'solid)
        (send dc draw-text "1" (car p1coord) (cdr p1coord))
        (send dc draw-text "2" (car p2coord) (cdr p2coord))
        (send dc draw-rectangle 300 0 190 300)
        (send dc set-font (make-object font% 50 'decorative))
        (send dc draw-text "JAIL" 325 10)
        (send dc set-font normal-control-font)
        (send dc scale 0.25 0.25)
        (send dc draw-bitmap my-pic 1250 300)
        (send dc scale 4 4)))
    (define/public (dice-proc player this-ui)
          (send this-ui present "Just visiting."))
    (super-new)))

;;GO TO JAIL
(define go-to-jail%
  (class custom-plot%
    (inherit-field
     number
     p1coords
     p2coords
     occupant)
    (define/public (dice-proc player this-ui)
      (send this-ui present "You stepped on go to jail and were sent directly to prison")
      (send player set-jail))
    (define/public (draw-proc canvas dc player1 player2)
      (let* ((our-pic (make-object bitmap% "images/monopoly.jpg"))
             (my-pic (make-object bitmap% "images/go-to-jail.png"))
             (p1coord (send (send player1 get-place) get-p1coords))
             (p2coord (send (send player2 get-place) get-p2coords)))
        (send dc scale 0.5 0.5)
        (send dc draw-bitmap our-pic 0 0) 
        (send dc scale 2 2)
        (send dc set-brush "white" 'solid)
        (send dc set-pen "black" 0 'solid)
        (send dc draw-text "1" (car p1coord) (cdr p1coord))
        (send dc draw-text "2" (car p2coord) (cdr p2coord))
        (send dc draw-rectangle 300 0 190 300)
        (send dc draw-bitmap my-pic 310 60)))
    
    (super-new)))

