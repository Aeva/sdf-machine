#lang racket/base

(require racket/format)
(require racket/flonum)
(require racket/unsafe/ops)
(require racket/performance-hint)
(require ffi/unsafe/atomic)

(require racket/gui)
(require racket/draw)
(require racket/class)

(require "register_machine.rkt")


(begin-encourage-inline
  (define sd
    (sdfn (reg 3 input)
          (reg 3 offset-2 -50. 0. 50.)
          (reg 3 offset-3 90. 0. 150.)
          (reg 3 point)
          (reg 1 radius-1 200.)
          (reg 1 radius-2 150.)
          (reg 1 radius-3 80.)
          (reg 1 radius-4 40.)
          (reg 1 radius-5 120.)
          (reg 1 sphere-1)
          (reg 1 sphere-2)
          (reg 1 sphere-3)
          (reg 1 sphere-4)
          (reg 1 sphere-5)
          (reg 1 out)
          (sd-sphere sphere-1 input radius-1)
          (sub point input offset-2)
          (sd-sphere sphere-2 point radius-2)
          (sd-sphere sphere-5 point radius-5)
          (sub point input offset-3)
          (sd-sphere sphere-3 point radius-3)
          (sd-sphere sphere-4 point radius-4)
          (sd-cut out sphere-1 sphere-2)
          (sd-cut out out sphere-3)
          (min out out sphere-4)
          (min out out sphere-5))))


(define (safe-sd x y z)
  (unless (flonum? x)
    (error "Expected flonum, got:" x))
  (unless (flonum? y)
    (error "Expected flonum, got:" y))
  (unless (flonum? z)
    (error "Expected flonum, got:" z))
  (sd x y z))


; show image in new window
(define (image-window bmp)
  (let* ([width (send bmp get-width)]
         [height (send bmp get-height)]
         [frame (new frame%
                     [label "sdf"]
                     [width (+ width 16)]
                     [height (+ height 39)])])
    (new canvas%
         [parent frame]
         [paint-callback
          (lambda (canvas ctx)
            (send ctx draw-bitmap bmp 0 0))])
    (send frame show #t)))


; monty carlo SDF volume renderer
(define (volume-eris iterations sd x1 y1 z1 x2 y2 z2)
  (start-atomic)
  (let ([hits null]
        [width (+ (- x2 x1) 1)]
        [height (+ (- y2 y1) 1)]
        [depth (+ (- z2 z1) 1)])
    (define sd-start (current-inexact-milliseconds))
    (for ([i iterations])
      (let* ([x (->fl (random x1 (+ x1 width)))]
             [y (->fl (random y1 (+ y1 height)))]
             [z (->fl (random z1 (+ z1 depth)))]
             [d (sd x y z)])
        (when (and (d . <= . -1.) (d . >= . -9.))
          (set! hits (cons (flvector x y z (flabs d)) hits)))))
    (define sd-end (current-inexact-milliseconds))
    (define sd-delta (- sd-end sd-start))
    (display (~a "SDF queries finished in " sd-delta " ms.\n"))
    (define draw-start (current-inexact-milliseconds))
    (let* ([z-sorted (sort hits < #:key (lambda (h) (unsafe-flvector-ref h 2)))]
           [bmp (make-bitmap width height)]
           [ctx (new bitmap-dc% [bitmap bmp])]
           [last-c 0]
           [color (make-color 0 0 0)])
      (for ([h z-sorted])
        (let* ([r (flvector-ref h 3)]
               [d (* r 2)]
               [x (- (flvector-ref h 0) x1 r)]
               [y (- (flvector-ref h 1) y1 r)]
               [z (- (flvector-ref h 2) z1)]
               [c (exact-round (* (/ z depth) 255))])
          (when (c . > . last-c)
            (set! last-c c)
            (set! color (make-color c c c))
            (send ctx set-pen color 0 'solid)
            (send ctx set-brush color 'solid))
          (send ctx draw-ellipse x y d d))
          )
      (define draw-end (current-inexact-milliseconds))
      (define draw-delta (- draw-end draw-start))
      (end-atomic)
      (display (~a "Sorting and drawing finished in " draw-delta " ms.\n"))
      bmp)))


; run the demo
(image-window
 (volume-eris 1000000 sd -200 -200 -200 200 200 200))
