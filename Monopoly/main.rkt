#lang racket/gui
(require "Plot-card.rkt")
(require "cards.rkt")
(require "Player.rkt")

(define player1
  (new player%
       [name "jonte"]
       [place go]
       [appearance  "images/player1.png"]))

(define player2
  (new player%
       [name "Jaqqe"]
       [place go]
       [appearance  "images/player2.png"]))

(define game%
  (class object%
    (init-field
     [current-turn 1]
     [current-player player1])
    
    (define (plot-button-proc button event)
      (let ((place (send current-player get-place)))
        (cond
          [(eq? (send dice-button get-label) "Dice")(present "don't can't buy ;(")]
          [(> (send place get-price)(send current-player get-balance))
           (send this present "Not enough funds")]
          [(eq? (send place get-owner) #f)
           (present (string-append "congratulations, you bought "
                                   (send 
                                    (send current-player get-place) get-name)))
           (send current-player buy-plot place this)
           (send place set-owner current-player)
           (send windu refresh)]
          [else (send this present "This plot is already owned")])))

    (define (dice-proc button event)
      (let ((x (+ 1 (random 6)))
            (place (send current-player get-place)))
        (cond
          [(and (eq? (send button get-label) "Dice")
                (eq? (send current-player in-jail?) #t))
           (cond
             [(>= x 4)
               (send this present "You're out of prison! You can continue your business on the next turn")
               (send button set-label (number->string x))
               (send current-player set-jail)]
             [else (send button set-label (number->string x))
              (send this present "Unlucky! To get out of prison you must get a 4 or higher on the dice. Try again next turn.")])]
          [(and (eq? (send button get-label) "Dice")
                (>= (+ (string->number (send place get-number)) x)
                    (hash-count plots)))
           (send current-player add-amount 50)
           (send button set-label (number->string x))
           (send current-player
                 move-to
                 (hash-ref plots 
                           (number->string
                            (- (+ (string->number (send place get-number)) x)
                               (hash-count plots)))))
           (send windu refresh)
           (send (send current-player get-place) dice-proc current-player this)]
          [(eq? (send button get-label) "Dice")
           (send button set-label (number->string x))
           (send current-player
                 move-to
                 (hash-ref plots 
                           (number->string 
                            (+ (string->number (send place get-number)) x))))
           (send windu refresh)
           (send (send current-player get-place) dice-proc current-player this)]
          
          [else (send this present "You have already rolled the dice this turn")])))
    
    (define (next-proc button event)
      (if (eq? (send current-player in-jail?) #t)
          (send current-player move-to jail)
          (void))
      (cond
        [(eq? (send dice-button get-label) "Dice")(void)]
        [(eq? current-player player2)
         (clear-output)
         (send game change-player)
         (send game change-turn)
         (send dice-button set-label "Dice")
         (send windu refresh)]
        [else (send game change-player)
              (clear-output)
              (send dice-button set-label "Dice")
              (send windu refresh)]))
    
    (define (house-proc button event)
      (let ((place (send current-player get-place)))
        (cond
          [(and (eq? (send place get-owner) current-player)
                (not (eq? (send place number-of-houses) #f))
                (> (send current-player get-balance)
                   (string->number (send place get-house-price))))
           (send place add-house this current-player)]
          [else (send this present "You can't buy a house at this time")])))

    (define (hotel-proc button event)
      (let ((place (send current-player get-place)))
        (cond
          [(and (eq? (send place get-owner) current-player)
                (not (eq? (send place number-of-houses) #f))
                (> (send current-player get-balance)
                   (string->number (send place get-hotel-price))))
           (send place add-hotel this current-player)]
          [else (send this present "You can't buy a hotel at this time")])))
          
              
    (define/public (get-current-player)
      current-player)
    (define/public (change-turn)
      (set! current-turn (+ current-turn 1)))
    (define/public (get-free-parking)
      free-parking)
    
    (define/public (change-player)
      (cond
        [(eq? current-player player1)
         (set! current-player player2)]
        [(eq? current-player player2)
         (set! current-player player1)]
        [else #f]))
    
    (define windu
      (new frame%
           [label "Monopol"]
           [height 600]
           [width 800]))
    
    (define horiz-panel1 (new horizontal-panel%
                              [style (list 'border)]
                              [parent windu]
                              [min-height 230]
                              [alignment '(right center)]))
    
    (define horiz-panel2 (new horizontal-panel%
                              [style (list 'border)]
                              [parent windu]
                              [min-height 70]))
    
    (define horiz-panel3 (new horizontal-panel%
                              [style (list 'border)]
                              [parent windu]
                              [alignment '(right center)]))
    
    (define dice-button (new button%
                             [label "Dice"]
                             [parent horiz-panel3]
                             [callback dice-proc]))
    
    (define next-button (new button%
                             [label "NEXT"]
                             [parent horiz-panel3]
                             [callback next-proc]))
    
    (define plot-button (new button%
                             [label "Buy Plot"]
                             [parent horiz-panel1]
                             [callback plot-button-proc]
                             ))
    
    (define house-button (new button%
                              [label "Buy House"]
                              [parent horiz-panel1]
                              [callback house-proc]
                              ))
    
    (define hotel-button (new button%
                              [label "Buy Hotel"]
                              [parent horiz-panel1]
                              [callback hotel-proc]
                              ))

    (define/public (get-turn)
      current-turn)
    
    (define/public (draw-proc canvas dc)
      (let ((my-pic (make-object bitmap% (send current-player get-appearance))))
        (send dc scale 0.2 0.2)
        (send dc draw-bitmap my-pic 0 100)
        (send dc scale 5 5)
        (send dc 
              draw-text 
              (string-append 
               "Current turn: " 
               (number->string current-turn)) 
              550 0)
        (send dc draw-text
              (string-append 
               "Free parking pot: $"
               (number->string 
                (send free-parking get-balance)))
              510 20)
        (send current-player draw-proc canvas dc)))
    
    (define/public (draw-proc1 canvas dc)
      (send (send current-player get-place) draw-proc canvas dc player1 player2))
    
    (define (draw-proc2 canvas dc)
      (send this draw-proc canvas dc))
    
    (define (drawing-proc canvas dc)
      (send this draw-proc1 canvas dc))
    
    (define *our-canvas* (new canvas%
                              [parent horiz-panel1]
                              [paint-callback drawing-proc]))
    
    (define canvas1 (new canvas% 
                         [parent horiz-panel3]
                         [paint-callback draw-proc2]))
    
    (define/public (notify message)
      (define dialog-box 
        (new dialog% 
             [label message]
             [width 200]
             [height 100]
             [enabled #t]
             [style '(close-button)]
             [parent windu]))
      (new text-field%
           [label #f]
           [parent dialog-box]
           [enabled #f]
           [init-value message])
      (new button%
           [parent dialog-box]
           [label "OK"]
           [callback 
            (lambda args
              (send dialog-box show #f))])
      (send dialog-box show #t))
    
    (define/public (close-ui)
      (send windu show #f))
    
    (define/public (clear-output)
      (send output-field set-value ""))
    
    
    (define/public (present entered-line)      
      (let
          ([message-with-line-feed (string-append entered-line "\n")])
        (send this present-no-lf message-with-line-feed)))
    
    (define/public (present-no-lf entered-string)        
      (send output-field set-value
            (string-append
             (send output-field get-value)           
             entered-string))
      (scroll-to-bottom))
    
    (define/private (scroll-to-bottom)
      (let
          ([editor (send output-field get-editor)])
        
        (send 
         editor
         scroll-to-position
         (send editor last-position))
        (send output-field refresh)))
    
    (define output-panel
      (new vertical-panel%
           [parent horiz-panel2]))
    
    (define output-field
      (new text-field%
           [parent output-panel]
           [min-height (send output-panel get-height)]
           [label ""]
           [style '(multiple)]
           [enabled #f]))
    
    (send windu show #t)
    
    (define/public (get-window)
      windu)
    (super-new)))

(define game
  (new game%))
