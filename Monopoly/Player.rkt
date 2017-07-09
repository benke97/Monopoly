#lang racket
(provide player%)

(define player%
  (class object%
    (init-field
     name
     place
     appearance
     [jail #f]
     [turn #f]
     [owned-plots (make-hash)]
     [balance 1000])
    
    (define/public (get-name)
      name)
    (define/public (in-jail?)
      jail)
    (define/public (set-jail)
      (if (eq? jail #f)
          (set! jail #t)
          (set! jail #f)))
    (define/public (get-balance)
      balance)
    (define/public (my-list->string lst)
      (cond
        [(null? lst) " "]
        [else (string-append (car lst) ", " (my-list->string (cdr lst)))]))
    (define/public (draw-proc canvas dc)
      (send dc 
              draw-text 
              name 
              10 0)
        (send dc 
              draw-text
              (string-append 
               "Properties:" 
               (my-list->string 
                (hash-keys owned-plots))) 
              100 80)
        (send dc 
              draw-text 
              (string-append "Balance:$" 
                             (number->string
                              balance)) 
              100 100))
    (define/public (subtract-amount amount this-ui)
      (cond
        [(< (- balance amount) 0)
         (set! balance (- balance amount))
         (send (send this-ui get-window) refresh)
         (send this-ui notify 
               (string-append name " lost due to insufficient funds"))
         (send this-ui close-ui)]
        [else (set! balance (- balance amount))]))
    (define/public (add-amount amount)
      (set! balance (+ balance amount)))
    (define/public (buy-plot plot this-ui)
      (cond
        [(> balance (send plot get-price))
         (send this subtract-amount (send plot get-price) this-ui)
         (hash-set! owned-plots (send plot get-name) plot)]
        [else "du får inte"]))
    (define/public (get-place)
      place)
    (define/public (owned-plot? plot-name)
      (hash-has-key? owned-plots plot-name))
    (define/public (get-appearance)
      appearance)
    (define/public (move-to new-place)
      (send place remove this)
      (set! place new-place)
      (send new-place add this))
    (define/public (get-owned-plots)
      (hash-keys owned-plots))
    
    
    (super-new)))

