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
           [reg-set (lambda (reg lane value)
                      (let ([loc (+ (reg-loc reg) lane)])
                        #`(flvector-set! registers #,loc #,value)))]

           ; Generates syntax for reading from a register.
           [reg-ref (lambda (reg lane)
                      (let ([loc (+ (reg-loc reg) lane)])
                        #`(flvector-ref registers #,loc)))]

           ; Commit some syntax to the proc list.
           [commit (lambda (part)
                     (set! proc (append proc (list part))))]

           ; Generic operator handler.
           [handle-op
            (lambda (op cmd out lhs rhs)
              (check-reg cmd out)
              (check-reg cmd lhs)
              (check-reg cmd rhs)
              (let ([lanes (min (reg-size out)
                                (reg-size lhs)
                                (reg-size rhs))])
                (for ([lane lanes])
                  (commit
                   (reg-set out lane #`(#,op
                                        #,(reg-ref lhs lane)
                                        #,(reg-ref rhs lane)))))))]

           ; Square root command handler.
           [handle-sqrt
            (lambda (cmd out vec)
              (check-reg cmd out)
              (check-reg cmd vec)
              (let ([lanes (min (reg-size out)
                                (reg-size vec))])
                (for ([lane lanes])
                  (commit
                   (reg-set out lane #`(flsqrt #,(reg-ref vec lane)))))))]

           ; Dot product command handler.
           [handle-dot
            (lambda (cmd out lhs rhs)
              (check-reg cmd out)
              (check-reg cmd lhs)
              (check-reg cmd rhs)
              (unless (eq? (reg-size out) 1)
                (error "len op expects scalar output" cmd))
              (let ([lanes (min (reg-size lhs)
                                (reg-size rhs))])
                (commit
                 (reg-set out 0 #`#,(append
                                     (list #'fl+)
                                     (for/list ([lane lanes])
                                       #`(fl* #,(reg-ref lhs lane) #,(reg-ref rhs lane))))))))]

           ; Length command handler.
           [handle-len
            (lambda (cmd out vec)
              (check-reg cmd out)
              (check-reg cmd vec)
              (unless (eq? (reg-size out) 1)
                (error "len op expects scalar output" cmd))
              (commit
               (reg-set out 0 #`(flsqrt
                                 #,(append
                                    (list #'fl+)
                                    (for/list ([i (reg-size vec)])
                                      #`(fl* #,(reg-ref vec i) #,(reg-ref vec i))))))))]

           ; SD Sphere command handler.
           [handle-sd-sphere
            (lambda (cmd out vec rad)
              (check-reg cmd out)
              (check-reg cmd vec)
              (check-reg cmd rad)
              (unless (eq? (reg-size out) 1)
                (error "sd sphere expects a scalar output" cmd))
              (unless (eq? (reg-size vec) 3)
                (error "sd sphere expects a vec3 input" cmd))
              (unless (eq? (reg-size rad) 1)
                (error "sd sphere expects a scalar radius" cmd))
              (commit
               (reg-set out 0 #`(fl-
                                 (flsqrt
                                  #,(append
                                     (list #'fl+)
                                     (for/list ([i (reg-size vec)])
                                       #`(fl* #,(reg-ref vec i) #,(reg-ref vec i)))))
                                 #,(reg-ref rad 0)))))])

      ; Process accumulated commands to build out 'proc.
      (for ([cmd cmd-seq])
        (syntax-case cmd (add sub mul div min max sqrt dot len sd-sphere)
          ; Addition
          [(add out lhs rhs)
           (let* ([out (syntax->datum #'out)]
                  [lhs (syntax->datum #'lhs)]
                  [rhs (syntax->datum #'rhs)])
             (handle-op #'fl+ cmd out lhs rhs))]
          ; Subtraction
          [(sub out lhs rhs)
           (let* ([out (syntax->datum #'out)]
                  [lhs (syntax->datum #'lhs)]
                  [rhs (syntax->datum #'rhs)])
             (handle-op #'fl- cmd out lhs rhs))]
          ; Multiplication
          [(mul out lhs rhs)
           (let* ([out (syntax->datum #'out)]
                  [lhs (syntax->datum #'lhs)]
                  [rhs (syntax->datum #'rhs)])
             (handle-op #'fl* cmd out lhs rhs))]
          ; Division
          [(div out lhs rhs)
           (let* ([out (syntax->datum #'out)]
                  [lhs (syntax->datum #'lhs)]
                  [rhs (syntax->datum #'rhs)])
             (handle-op #'fl/ cmd out lhs rhs))]
          ; Min
          [(min out lhs rhs)
           (let* ([out (syntax->datum #'out)]
                  [lhs (syntax->datum #'lhs)]
                  [rhs (syntax->datum #'rhs)])
             (handle-op #'flmin cmd out lhs rhs))]
          ; Max
          [(max out lhs rhs)
           (let* ([out (syntax->datum #'out)]
                  [lhs (syntax->datum #'lhs)]
                  [rhs (syntax->datum #'rhs)])
             (handle-op #'flmax cmd out lhs rhs))]
          ; Square root
          [(sqrt out vec)
           (let* ([out (syntax->datum #'out)]
                  [vec (syntax->datum #'vec)])
             (handle-sqrt cmd out vec))]
          ; Dot product
          [(dot out lhs rhs)
           (let* ([out (syntax->datum #'out)]
                  [lhs (syntax->datum #'lhs)]
                  [rhs (syntax->datum #'rhs)])
             (handle-dot cmd out lhs rhs))]
          ; Length
          [(len out vec)
           (let* ([out (syntax->datum #'out)]
                  [vec (syntax->datum #'vec)])
             (handle-len cmd out vec))]
          ; Sphere
          [(sd-sphere out vec rad)
           (let* ([out (syntax->datum #'out)]
                  [vec (syntax->datum #'vec)]
                  [rad (syntax->datum #'rad)])
             (handle-sd-sphere cmd out vec rad))])))
        
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
