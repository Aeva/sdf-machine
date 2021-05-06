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


(define sd
  (sdfn (reg 3 input)
        (reg 3 offset-1 10. 0. 0.)
        (reg 3 offset-2 -50. 0. 0.)
        (reg 3 point)
        (reg 1 radius-1 200.)
        (reg 1 radius-2 150.)
        (reg 1 sphere-1)
        (reg 1 sphere-2)
        (reg 1 out)
        (sub point input offset-1)
        (sd-sphere sphere-1 point radius-1)
        (sub point input offset-2)
        (sd-sphere sphere-2 point radius-2)
        (sd-cut out sphere-1 sphere-2)))


(define (safe-sd x y z)
  (unless (flonum? x)
    (error "Expected flonum, got:" x))
  (unless (flonum? y)
    (error "Expected flonum, got:" y))
  (unless (flonum? z)
    (error "Expected flonum, got:" z))
  (sd x y z))


; Render the distance field, line by line
(define (scanline field x-extent y-extent)
  (define width (+ 1 (* x-extent 2)))
  (define height (+ 1 (* y-extent 2)))
  (define bmp (make-bitmap width height))
  (define ctx (new bitmap-dc% [bitmap bmp]))

  (define scan-start #f)
  (define scan-stop #f)

  (begin-encourage-inline
    (define (draw y)
      (send ctx draw-line
            scan-start y
            scan-stop y)
      (set! scan-start #f)))

  (start-atomic)
  (define start (current-inexact-milliseconds))
  (for ([y (in-range height)])
    (for ([x (in-range width)])
      (define sample-x (- x x-extent))
      (define sample-y (- y y-extent))
      (define sample (field (->fl sample-x) (->fl sample-y) 0.))
      (define solid (sample . <= . 0))
      (cond
        [(and solid scan-start)
         (set! scan-stop x)]
        [(and solid (not scan-start))
         (set! scan-start x)
         (set! scan-stop x)]
        [(and (not solid) scan-start)
         (draw y)]
        [else (void)]))
    (when scan-start (draw y)))
  (define stop (current-inexact-milliseconds))
  (end-atomic)
  (define delta (- stop start))
  (display (~a delta " ms\n"))
  bmp)


; Render the distance field and post some stats.
(define (run-demo)
  (display "drawing...\n")
  (let* ([bmp (scanline sd 210 200)]
         [frame (new frame%
                     [label "sdf"]
                     [width 480]
                     [height 480])])
    (new canvas%
         [parent frame]
         [paint-callback
          (lambda (canvas ctx)
            (send ctx draw-bitmap bmp 0 0))])
    (send frame show #t)))

(run-demo)
