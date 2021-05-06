#lang racket
(require racket/format)
(require racket/flonum)


(provide sdfn)


(define-syntax (sdfn stx)
  (let* (; List of syntax objects corresponding to commands.
         [src (cdr (syntax-e stx))]

         ; Register information.  name : (start size (fill ...))
         [register-info (make-hash)]

         ; Register names, in order of definition.
         [register-names null]

         ; Next register start, and final register file size.
         [cursor 0]

         ; Add a new register definition.
         [new-reg (lambda (cmd size name fill)
                    (when (size . < . 1)
                      (error "Invalid register size." cmd))
                    (when (hash-ref register-info name #f)
                      (error "Duplicate register name." cmd))
                    (unless (eq? (length fill) size)
                      (display size)
                      (display "\n")
                      (display fill)
                      (display "\n")
                      (error "Mismatch between default values and register size." cmd))
                    (hash-set! register-info name (list cursor size fill))
                    (set! register-names (append register-names (list name)))
                    (set! cursor (+ cursor size)))]

         ; Extracted commands.
         [cmd-seq null]

         ; Processed commands.
         [proc (list #'begin)])

    ; Search through the command list for register definitions.
    (for ([cmd src])
      (syntax-case cmd (reg)
        
        ; Define a new register without any default values.
        [(reg size name)
         (let* ([size (syntax->datum #'size)]
                [name (syntax->datum #'name)]
                [fill (for/list ([i size]) 0.0)])
           (new-reg cmd size name fill))]
        
        ; Define a new register with default values.
        [(reg size name fill ...)
         (let* ([size (syntax->datum #'size)]
                [name (syntax->datum #'name)]
                [fill (syntax->datum #'(fill ...))])
           (new-reg cmd size name fill))]
        
        ; Invalid register definition.
        [(reg etc ...)
         (error "Expected (reg name size) or (reg name size fill ...)" cmd)]

        ; Capture everything eles as program commands to be validated later.
        [(etc ...) (set! cmd-seq (append cmd-seq (list cmd)))]))


    (let* (; Raise an error if the register wasn't defined.
           [check-reg (lambda (cmd reg)
                        (unless (hash-ref register-info reg #f)
                          (error "Unknown register" cmd)))]

           ; Returns the size of a defined register.
           [reg-size (lambda (reg)
                       (cadr (hash-ref register-info reg)))]

           ; Returns the location of a defined register.
           [reg-loc (lambda (reg)
                      (car (hash-ref register-info reg)))]

           ; Generates syntax for setting a register.
           [reg-set (lambda (reg value)
                      (let ([loc (reg-loc reg)])
                        #`(flvector-set! registers #,loc #,value)))]

           ; Generates syntax for reading from a register.
           [reg-ref (lambda (reg lane)
                      (let ([loc (+ (reg-loc reg) lane)])
                        #`(flvector-ref registers #,loc)))]

           ; Commit some syntax to the proc list.
           [commit (lambda (part)
                     (set! proc (append proc (list part))))]

           ; Length command handler.
           [handle-len
            (lambda (cmd out vec)
              (check-reg cmd out)
              (check-reg cmd vec)
              (unless (eq? (reg-size out) 1)
                (error "len op expects scalar output" cmd))
              (commit
               (reg-set out #`(flsqrt
                               #,(append
                                  (list #'fl+)
                                  (for/list ([i (reg-size vec)])
                                    #`(fl* #,(reg-ref vec i) #,(reg-ref vec i))))))))])

      ; Process accumulated commands to build out 'proc.
      (for ([cmd cmd-seq])
        (syntax-case cmd (len)
          [(len out vec)
           (let* ([out (syntax->datum #'out)]
                  [vec (syntax->datum #'vec)])
             (handle-len cmd out vec))])))
        
    ; This produces the initial values for the register file.
    (define initial-values (apply append (for/list ([name register-names]) (caddr (hash-ref register-info name)))))

    ; Emit code:
    #`(let
          ([registers
            (apply flvector '#,initial-values)])
        (lambda (x y z)
          (flvector-set! registers 0 x)
          (flvector-set! registers 1 y)
          (flvector-set! registers 2 z)
          #,(datum->syntax stx proc)
          (flvector-ref registers #,(- cursor 1))))))


; Define a signed distance function!
(define mysdf
  (sdfn (reg 3 input)
        (reg 4 moop 1. 0. 0. 1.)
        (reg 1 out -.4)
        (len out moop)))


; See if it worked!
;(display (mysdf 1. 1. 0.))
