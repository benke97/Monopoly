#lang racket/gui
(provide (all-defined-out))
(require "Plot-card.rkt")

(define plots (make-hash))

;;PURPLE
(define go
  (new go-plot%
       [number "0"]
       [p1coords '(250 . 230)]
       [p2coords '(270 . 230)]))
(hash-set! plots "0" go)
 
(define linköping
  (new plot-card%
       [name "Linköping"]
       [price 50]
       [original-rent 50]
       [color "purple"]
       [number "7"]
       [p1coords '(10 . 160)]
       [p2coords '(30 . 160)]))
(hash-set! plots "7" linköping)

(define svanhildsvagen
   (new plot-card%
        [name "Svanhildsvagen"]
        [price 55]
        [original-rent 60]
        [color "purple"]
        [number "6"]
        [p1coords '(10 . 200)]
        [p2coords '(30 . 200)]))
(hash-set! plots "6" svanhildsvagen)

;;YELLOW
(define vasagatan
  (new plot-card%
       [name "Vasagatan"]
       [price 60]
       [original-rent 70]
       [color "yellow"]
       [number "9"]
       [p1coords '(10 . 80)]
       [p2coords '(30 . 80)]))

(hash-set! plots "9" vasagatan)

(define slottet
  (new plot-card%
       [name "Slottet"]
       [price 65]
       [original-rent 80]
       [color "yellow"]
       [number "11"]
       [p1coords '(90 . 10)]
       [p2coords '(90 . 30)]))
(hash-set! plots "11" slottet)

;;BROWN

(define mordor
  (new plot-card%
       [name "Mordor"]
       [price 70]
       [original-rent 90]
       [color "brown"]
       [number "1"]
       [p1coords '(195 . 250)]
       [p2coords '(210 . 250)]))
(hash-set! plots "1" mordor)

(define osgiliath
  (new plot-card%
       [name "Osgiliath"]
       [price 75]
       [original-rent 100]
       [color "brown"]
       [number "4"]
       [p1coords '(80 . 260)]
       [p2coords '(95 . 260)]))
(hash-set! plots "4" osgiliath)

;;BLUE

(define asgard
  (new plot-card%
       [name "Asgard"]
       [price 80]
       [original-rent 110]
       [color "blue"]
       [number "17"]
       [p1coords '(260 . 120)]
       [p2coords '(280 . 120)]))
(hash-set! plots "17" asgard)

(define lith
  (new plot-card%
       [name "LiTH"]
       [price 85]
       [original-rent 120]
       [color "blue"]
       [number "19"]
       [p1coords '(260 . 200)]
       [p2coords '(280 . 200)]))
(hash-set! plots "19" lith)

;;RED

(define strandvagen
  (new plot-card%
       [name "Strandvagen"]
       [price 90]
       [original-rent 130]
       [color "red"]
       [number "14"]
       [p1coords '(200 . 10)]
       [p2coords '(200 . 30)]))
(hash-set! plots "14" strandvagen)

(define norrmalmstorg
  (new plot-card%
       [name "Norrmalmstorg"]
       [price 95]
       [original-rent 140]
       [color "red"]
       [number "16"]
       [p1coords '(260 . 80)]
       [p2coords '(280 . 80)]))
(hash-set! plots "16" norrmalmstorg)

(define free-parking
  (new free-parking%
       [number "10"]
       [p1coords '(10 . 48)]
       [p2coords '(55 . 48)]))
(hash-set! plots "10" free-parking)

(define west-station
  (new station%
       [name "West-Station"]
       [rent 100]
       [price 100]
       [number "8"]
       [p1coords '(5 . 120)]
       [p2coords '(50 . 120)]))
(hash-set! plots "8" west-station)

(define east-station
  (new station%
       [name "East-Station"]
       [rent 100]
       [price 100]
       [number "18"]
       [p1coords '(235 . 160)]
       [p2coords '(285 . 160)]))
(hash-set! plots "18" east-station)


(define chance1
  (new chance%
       [number "3"]
       [p1coords '(115 . 280)]
       [p2coords '(135 . 280)]))
(hash-set! plots "3" chance1)

(define jail
  (new jail%
       [number "5"]
       [p1coords '(30 . 280)]
       [p2coords '(50 . 280)]
       [jail-p1 '(30 . 225)]
       [jail-p2 '(62 . 225)]))
(hash-set! plots "5" jail)

(define water-works
  (new ww%
       [number "12"]
       [name "Water-works"]
       [rent 10]
       [price 100]
       [p1coords '(130 . 5)]
       [p2coords '(130 . 50)]))
(hash-set! plots "12" water-works)

(define electric-company
  (new electric%
       [number "2"]
       [name "Electric-Company"]
       [rent 10]
       [price 100]
       [p1coords '(155 . 280)]
       [p2coords '(175 . 280)]))
(hash-set! plots "2" electric-company)

(define chance2
  (new chance%
       [number "13"]
       [p1coords '(165 . 5)]
       [p2coords '(165 . 52)]))
(hash-set! plots "13" chance2)

(define go-to-jail
  (new go-to-jail%
       [number "15"]
       [p1coords '(235 . 10)]
       [p2coords '(280 . 50)]))
(hash-set! plots "15" go-to-jail)

       