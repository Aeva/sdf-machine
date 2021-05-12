#lang racket/base

; Copyright 2021 Aeva Palecek
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

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
          (reg 3 point)
          (reg 1 radius-1 200.)
          (reg 1 radius-2 150.)
          (reg 1 sphere-1)
          (reg 1 sphere-2)
          (reg 1 out)
          (sd-sphere sphere-1 input radius-1)
          (sub point input offset-2)
          (sd-sphere sphere-2 point radius-2)
          (sd-cut out sphere-1 sphere-2))))


(define (safe-sd x y z)
  (unless (flonum? x)
    (error "Expected flonum, got:" x))
  (unless (flonum? y)
    (error "Expected flonum, got:" y))
  (unless (flonum? z)
    (error "Expected flonum, got:" z))
  (sd x y z))


; Render the distance field, line by line
(define (scanline bmp field z color)
  (define width (send bmp get-width))
  (define height (send bmp get-height))
  (define ctx (new bitmap-dc% [bitmap bmp]))
  (define x-extent (/ (- width 1) 2))
  (define y-extent (/ (- height 1) 2))
  (send ctx set-pen color 0 'solid)
  (send ctx set-brush color 'solid)

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
      (define sample (field (->fl sample-x) (->fl sample-y) z))
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
  (- stop start))


; Render the distance field and post some stats.
(define (run-demo slices)
  (let* ([width 421]
         [height 401]
         [bmp (make-bitmap width height)]
         [bmp-ctx (new bitmap-dc% [bitmap bmp])]
         [frame (new frame%
                     [label "sdf"]
                     [width (+ width 16)]
                     [height (+ height 39)])])
    (send bmp-ctx set-background (make-color 0 0 0))
    (send bmp-ctx clear)
    (display "rendering...\n")
    (define start (current-inexact-milliseconds))
    (for ([slice slices])
      (let* ([alpha (/ slice (- slices 1))]
             [inv-a (- 1.0 alpha)]
             [c (round (* alpha 255))]
             [color (make-color c c c)]
             [z (+ (* inv-a -200.0) (* alpha 200.0))])
        (scanline bmp sd (->fl z) color)))
    (define end (current-inexact-milliseconds))
    (define delta (- end start))
    (display (~a slices " slices rendered in " (/ delta 1000) " seconds."))
    (new canvas%
         [parent frame]
         [paint-callback
          (lambda (canvas ctx)
            (send ctx draw-bitmap bmp 0 0))])
    (send frame show #t)))

(run-demo 1000)
