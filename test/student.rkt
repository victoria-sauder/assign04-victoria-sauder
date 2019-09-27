#lang racket
(require "../compile.rkt" "../asm/interp.rkt" rackunit)

(define (run e)
  (asm-interp (compile e)))


(check-equal? (run '(let ((x 7)) x)) 7)