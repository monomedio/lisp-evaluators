#lang racket ; CSC324 — 2022W — Assignment 1 — Eva Implementation

; Task: implement eva according to A1.eva-test.rkt.

(provide (contract-out (eva (any/c . -> . any/c)))
         ; Add any helper functions you tested in A1.eva-test.rkt.
         ; Whether you add contracts to them is optional.
         (contract-out (substitute (any/c any/c . -> . any/c)))
         #;a-helper)


(define (substitute func_term arg_term)
  (define (sub body pattern replacement)
    (match body
      [(app (λ (x) (equal? x pattern)) #t) replacement]
      [(list 'λ (list a) b) #:when (not (equal? a pattern)) (map (λ (z) (sub z pattern replacement)) body)]
      [(list a b) (map (λ (z) (sub z pattern replacement)) body)]
      [_ body]))
  (cond [(and (not (list? (last func_term))) (not (symbol? (last func_term)))) func_term]
        [else (sub (last func_term) (first (second func_term)) arg_term)]))

; Produce the value of a closed term from LCA.
(define (eva term)
  (match term
    [(list a b) (eva (substitute (eva a) (eva b)))]
    [_ term]))

