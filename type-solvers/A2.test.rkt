#lang racket ; CSC324 — 2022W — Assignment 2 — Type Inference

; Puts together the constraint generation and solving.
; You can put more tests here if you like.

(require "A2.infer.rkt" "A2.unify.rkt" rackunit)

(define (infer e env)
  (match (type-and-constraints e (α-type-generator))
    [(list type constraints)
     (define σ (mgu α? (append env constraints)))
     (σ type)]))

#;(check-equal? (infer '(λ (x) ((+ x) 5))
                       '())
                '(-> αx α3))

(check-equal? (infer '(λ (x) ((+ x) 5))
                     '((α+ (-> Number (-> Number Number)))))
              '(-> Number Number))
