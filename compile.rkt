#lang racket
(require "./asm/interp.rkt" rackunit)
(provide (all-defined-out))


(define (run e)
  (asm-interp (compile e)))

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e '())
    ret
    err
    (push rbp) ; push before calling
    (call error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(? integer? i)
     `((mov rax ,(* i 4)))]
    [(? boolean? b)
     `((mov rax ,(if b #b101 #b001)))]
    [(? char? c)
     `((mov rax ,(+ (* (char->integer c) 4) 2)))]
    [`(char? ,c)
     `((mov rax ,(if (char? c) #b101 #b001)))]
    [`(integer? ,i)
     `((mov rax ,(if (integer? i) #b101 #b001)))]
    [`(boolean? ,b)
     `((mov rax ,(if (boolean? b) #b101 #b001)))]
    [`(add1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (add rax 4)))]
    [`(sub1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (sub rax 4)))]
    [`(zero? ,e0)
     (let ((c0 (compile-e e0 c))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         ,@assert-integer
         (cmp rax 0)
         (mov rax #b001) ; #f         
         (jne ,l0)
         (mov rax #b101) ; #t
         ,l0))]    
    [`(if ,e0 ,e1 ,e2)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 c))
           (c2 (compile-e e2 c))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         (cmp rax #b001) ; compare to #f
         (je ,l0)        ; jump to c2 if #f
         ,@c1
         (jmp ,l1)       ; jump past c2
         ,l0
         ,@c2
         ,l1))]
    [`(abs ,e0)
     (let ([c0 (compile-e e0 c)])
       `(,@c0
         ,@assert-integer
         (mov rbx rax)
         (neg rax)
         (cmovl rax rbx)))]
    [`(- ,e0)
     (let ([c0 (compile-e e0 c)])
       `(,@c0
         ,@assert-integer
         (neg rax)))]
    [`(char->integer ,e0)
     (if (char? e0)
         (let ([c0 (char->integer e0)])
       `(,@(compile-e c0 c)))
         '((jmp err)))]
    [`(integer->char ,e0)
     (with-handlers ([exn:fail:contract? (lambda (exn) '((jmp err)))])
       (let ([c0 (integer->char e0)]) 
       `((mov rax ,(+ (* e0 4) 2)))))]

    ;; TODO: make sure this works with your generalized let
    [(? symbol? x)
     (let ((i (lookup x c)))
       `((mov rax (offset rsp ,(- (add1 i))))))]

    ;; TODO: generalize
    [`(let ((,x ,e0)) ,e1)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 (cons x c))))
       `(,@c0
         (mov (offset rsp ,@(- (add1 (length c)))) rax)
         ,@c1))]
    [(list 'cond cs ... `(else ,en))
     (compile-cond-env cs en c)]
    ))

(define (compile-cond-env cs en r)
  (match cs
    ['() (let ([c0 (compile-e en r)]) `(,@c0))]
    [(cons `(,eq ,ea) cs)
     (match (compile-e eq r)
       ['err 'err]
       [v
        (let ([c0 (compile-e eq r)]
              [c1 (compile-cond-env cs en r)]
              [c2 (compile-e ea r)]
              [l0 (gensym "cond")]
              [l1 (gensym "cond")])
          `(,@c0
            (cmp rax #b001)
            (jne ,l0)
            ,@c1
            (jmp ,l1)
            ,l0
            ,@c2
            ,l1))])]))


  ;; Variable CEnv -> Natural
  (define (lookup x cenv)
    (match cenv
      ['() (error "undefined variable:" x)]
      [(cons y cenv)
       (match (symbol=? x y)
         [#t (length cenv)]
         [#f (lookup x cenv)])]))

  ;; Asm
  (define assert-integer
    `((mov rbx rax)
      (and rbx #b11)
      (cmp rbx 0)
      (jne err)))
