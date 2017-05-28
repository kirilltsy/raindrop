#lang racket
(require 2htdp/universe 2htdp/image)

(define MAXTIME 200)
(define SIZE 500)
(define MAXWATERLEVEL 10)
(define DIF 0.98)
(define TICKRATE 0.04)
(define MS (empty-scene 600 600 "blue"))

(struct state (maxtimenow time waterlevel score drop))
(struct drop (text ans))

(define (main p)
  (big-bang 0
            (close-on-stop true)
            (on-key checkstart)
            (to-draw renderstart)
            (stop-when deadstart)
            (name "Tsybulkin's Raindrop")
            )
  (big-bang p
            (on-tick next TICKRATE)
            (on-key check)
            (stop-when dead? render-end)
            (to-draw render)
            (name "Tsybulkin's Raindrop")
            ))

(define (checkstart a ke) (cond [(key=? ke " ") -1]
        [else  0]))

(define (deadstart a) (if (< a 0) true false))

(define (renderstart a)
  (place-image (above (text "Welcome to Raindrop" 40 "yellow")
                      (text "to answer the question press" 30 "black")
                      (text "right arrow if it's correct" 30 "black")
                      (text "or left arrow otherwise." 30 "black")
                      (text " " 30 "black")
                      (text "To start press space bar" 30 "black")
                      (text "left and right" 30 "black"))
               300
               300
               MS))

(define (next p)
  (cond [(> (+ (state-time p) 1)
            (/ (* (state-maxtimenow p)
                  (- MAXWATERLEVEL (state-waterlevel p)))
               MAXWATERLEVEL)) 
         (state (* (state-maxtimenow p) DIF)
                0
                (+ (state-waterlevel p) 1)
                (state-score p)
                (newdrop)
                )]
        [else
         (state (state-maxtimenow p)
                (+ 1 (state-time p))
                (state-waterlevel p)
                (state-score p)
                (state-drop p))]))

(define (render p)
  (drawdrop p (place-image (rectangle 600
                        (* (/ (- MAXWATERLEVEL (state-waterlevel p)) MAXWATERLEVEL) SIZE)
                        "solid"
                        "white")
             300
             (/ (* (/ (- MAXWATERLEVEL (state-waterlevel p)) MAXWATERLEVEL) SIZE) 2)
             (place-image (text (string-append "Score: " (number->string (state-score p))) 30 "black")
                          300
                          550
                          MS))))

(define (newdrop)
  (define a (random 1 5))
  (cond
          [(= a 1) (+drop)]
          [(= a 2) (-drop)]
          [(= a 3) (/drop)]
          [(= a 4) (*drop)]))

(define (+drop)
  (define a (random 1 51))
  (define b (random 1 51))
  (define c (+ a b))
  (define d (random 2 101))
  (define e (random 1 3))
  (if (= c d) (set e 1) (set e (random 1 3)))
  (cond
    [(= e 1) (drop (string-append (number->string a)
                                  " + "
                                  (number->string b)
                                  " = "
                                  (number->string c))
                   true)]
    [(= e 2) (drop (string-append (number->string a)
                                  " + "
                                  (number->string b)
                                  " = "
                                  (number->string d))
                   false)])
  )

(define (-drop)
  (define a (random 51 101))
  (define b (random 1 51))
  (define c (- a b))
  (define d (random 2 101))
  (define e (random 1 3))
  (if (= c d) (set e 1) (set e (random 1 3)))
  (cond
    [(= e 1) (drop (string-append (number->string a)
                                  " - "
                                  (number->string b)
                                  " = "
                                  (number->string c))
                   true)]
    [(= e 2) (drop (string-append (number->string a)
                                  " - "
                                  (number->string b)
                                  " = "
                                  (number->string d))
                   false)])
  )

(define (*drop)
  (define a (random 1 11))
  (define b (random 1 11))
  (define c (* a b))
  (define d (* (random 1 11) (random 1 11)))
  (define e (random 1 3))
  (if (= c d) (set e 1) (set e (random 1 3)))
  (cond
    [(= e 1) (drop (string-append (number->string a)
                                  " * "
                                  (number->string b)
                                  " = "
                                  (number->string c))
                   true)]
    [(= e 2) (drop (string-append (number->string a)
                                  " * "
                                  (number->string b)
                                  " = "
                                  (number->string d))
                   false)])
  )

(define (/drop)
  (define a (random 1 101))
  (define b (random 1 11))
  (define c (/ a b))
  (define a1 (random 1 101))
  (define b1 (random 1 11))
  (define c1 (/ a b))
  (define e (random 1 3))
  (if (= c c1) (set e 1) (set e (random 1 3)))
  (cond
    [(= e 1) (drop (string-append (number->string a)
                                  " / "
                                  (number->string b)
                                  " = "
                                  (number->string (quotient a b))
                                  " + "
                                  (number->string (- (/ a b) (quotient a b))))
                   true)]
    [(= e 2) (drop (string-append (number->string a)
                                  " / "
                                  (number->string b)
                                  " = "
                                  (number->string (quotient a1 b1))
                                  " + "
                                  (number->string (- (/ a1 b1) (quotient a1 b1))))
                   false)])
  )


(define (drawdrop p im)
  (place-image (text (drop-text (state-drop p))
                     30
                     "blue")
               300
               (* SIZE (/ (state-time p)
                              (state-maxtimenow p)))
               im
               ))

(define (render-end p)
  (place-image (above (text "You LOST" 100 "red")
                      (text (string-append "Score: " (number->string (state-score p))) 50 "black"))
               300
               300
               MS))

(define (dead? p) (cond [(< (state-waterlevel p) MAXWATERLEVEL) false]
                        
             [else true]))

(define (check p ke)
  (cond [(key=? ke "left") (checkdrop p false)]
        [(key=? ke "right") (checkdrop p true)]
        [else  p]
       ))

(define (checkdrop p b)
  (cond [(boolean=? (drop-ans (state-drop p)) b)
         (state
               (* (state-maxtimenow p) DIF)
               0
               (state-waterlevel p)
               (+ (state-score p) 1)
               (newdrop)
               )]
        [else
         (state
               (* (state-maxtimenow p) DIF)
               0
               (+ (state-waterlevel p) 1)
               (state-score p)
               (newdrop)
               )]
        ))

(main (state MAXTIME 0 0 0 (newdrop)))
