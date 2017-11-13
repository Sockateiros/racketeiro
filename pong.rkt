#lang racket/gui

;the collisions need to take into account the path the objects have taken to get where they are... or the physics engine needs to simulate/sample the world with enough periodicity in order not to let things pass each other
;also with the later approach we need to set a maximum speed limit and calculate the period based on that... on collisions it might also not be enough to reverse velocities because if things are inside each other they will stay there

(define-values [screen-w screen-h] (get-display-size #t #:monitor 0))
(define frame (new frame% [label "Drawing Example"]))
(send frame set-cursor (make-object cursor% 'blank))

(define p1-w (* screen-w 0.01))
(define p1-h (* screen-h 0.2))
(define p1-x 0)
(define p1-y (- (* 0.5 screen-h) (* 0.5 p1-h)))
(define p1-vx 0)
(define p1-vy 0)
(define p1-score 0)

(define p2-w (* screen-w 0.01))
(define p2-h (* screen-h 0.2))
(define p2-x (- screen-w p2-w))
(define p2-y (- (* 0.5 screen-h) (* 0.5 p2-h)))
(define p2-vx 0)
(define p2-vy 0)
(define p2-score 0)

(define ball-x (* screen-w 0.5))
(define ball-y (* screen-h 0.5))
(define ball-vx 0.5)
(define ball-vy 0)
(define ball-r (* screen-h 0.02))

(define kup 0)
(define kdown 0)
(define kleft 0)
(define kright 0)

(define kw 0)
(define ks 0)
(define ka 0)
(define kd 0)

(define prev-ts 0)

(define game-key-handler
  (lambda (event)
    (let ([kc (send event get-key-code)]
          [krc (send event get-key-release-code)])
      (cond
        [(eq? kc 'up) (set! kup -1)]
        [(and (eq? kc 'release) (eq? krc 'up)) (set! kup 0)]
        [(eq? kc 'down) (set! kdown 1)]
        [(and (eq? kc 'release) (eq? krc 'down)) (set! kdown 0)]
        [(eq? kc 'left) (set! kleft -1)]
        [(and (eq? kc 'release) (eq? krc 'left)) (set! kleft 0)]
        [(eq? kc 'right) (set! kright 1)]
        [(and (eq? kc 'release) (eq? krc 'right)) (set! kright 0)]
        
        [(eq? kc #\w) (set! kw -1)]
        [(and (eq? kc 'release) (eq? krc #\w)) (set! kw 0)]
        [(eq? kc #\s) (set! ks 1)]
        [(and (eq? kc 'release) (eq? krc #\s)) (set! ks 0)]))))

(define key-handler
  (lambda (event)
    (send timer start 10)
    (set! prev-ts (current-inexact-milliseconds))
    (set! key-handler game-key-handler)))

(define my-canvas%
  (class canvas%
    (define/override (on-char event)
      (key-handler event))
    (super-new)))
 
(define canvas (new my-canvas% [parent frame]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc set-text-foreground "blue")
                       (send dc draw-text "PRESS ANY KEY TO START GAME" 0 0))]))

(define (draw-player dc x y w h)
  (send dc set-brush "white" 'solid)
  (send dc draw-rectangle x y w h))

(define (draw-ball dc x y r)
  (send dc draw-rectangle x y r r))

(define (make-draw-world w h)
  (lambda (dc)
    (send dc set-brush "black" 'solid)
    (send dc draw-rectangle 0 0 w h)
    (draw-player dc p1-x p1-y p1-w p1-h)
    (draw-player dc p2-x p2-y p2-w p2-h)
    (draw-ball dc ball-x ball-y ball-r)
    (draw-scores dc p1-score p2-score)))

(define (update-players-vel)
  (set! p1-vy (+ kw ks))
  (set! p2-vx (+ kleft kright))
  (set! p2-vy (+ kup kdown)))

(define (update-pos x vx dt)
  (+ x (* vx dt)))

(define (update-players-pos dt)
  (let ([p1-npx (update-pos p1-x p1-vx dt)]
        [p1-npy (update-pos p1-y p1-vy dt)]
        [p2-npx (update-pos p2-x p2-vx dt)]
        [p2-npy (update-pos p2-y p2-vy dt)])
    (set! p1-x p1-npx)
    (set! p1-y p1-npy)
    (set! p2-x p2-npx)
    (set! p2-y p2-npy)))

(define (hit-wall? y h)
  (or (> (+ y h) screen-h) (< y 0)))

(define (hit-player? bx by px py pw ph)
 (and (<= bx (+ px pw))
      (>= by py)
      (<= by (+ py ph))
      (>= bx px)))

(define (update-ball dt)
  (let ([bnx (update-pos ball-x ball-vx dt)]
        [bny (update-pos ball-y ball-vy dt)])
    (cond
      [(hit-wall? bny ball-r) (set! ball-vy (* -1 ball-vy))]
      [(hit-player? bnx bny p1-x p1-y p1-w p1-h)
       (begin (set! ball-vx (* -1 ball-vx))
              (set! ball-vy (+ (* 0.1 p1-vy) ball-vy)))]
      [(hit-player? (+ bnx ball-r) bny p2-x p2-y p2-w p2-h)
       (begin (set! ball-vx (* -1 ball-vx))
              (set! ball-vy (+ (* 0.1 p2-vy) ball-vy)))]
      [(< bnx 0)
       (begin (set! ball-x (* 0.5 screen-w))
              (set! ball-y (* 0.5 screen-h))
              (set! ball-vx 0.5)
              (set! ball-vy 0)
              (set! p2-score (+ 1 p2-score)))]
      [(> bnx screen-w)
       (begin (set! ball-x (* 0.5 screen-w))
              (set! ball-y (* 0.5 screen-h))
              (set! ball-vx -0.5)
              (set! ball-vy 0)
              (set! p1-score (+ 1 p1-score)))]
      [else (begin (set! ball-x bnx) (set! ball-y bny))])))
    
(define (draw-scores dc p1-s p2-s)
  (send dc set-font (send the-font-list find-or-create-font 24 'system 'normal 'bold))
  (send dc set-text-foreground "white")
  (send dc draw-text (number->string p1-s) (* 0.25 screen-w) (* 0.02 screen-h))
  (send dc draw-text (number->string p2-s) (* 0.75 screen-w) (* 0.02 screen-h)))

(define (update-world)
  (let* ([ts (current-inexact-milliseconds)]
         [dt (- ts prev-ts)])
    (update-players-vel)
    (update-players-pos dt)
    (update-ball dt)
    (set! prev-ts ts)))

(define draw-world (make-draw-world screen-w screen-h))
(send frame fullscreen #t)
(send frame show #t)
(send canvas focus)
(define timer (new timer%
                   [notify-callback (lambda ()
                                      (update-world)
                                      (send canvas refresh-now draw-world))]
                   [interval #f]))