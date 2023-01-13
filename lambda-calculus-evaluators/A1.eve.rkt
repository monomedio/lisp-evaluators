#lang racket ; CSC324 — 2022W — Assignment 1 — Eve Implementation

; Task: implement eve according to A1.eve-test.rkt.

(provide
 (contract-out (eve (any/c . -> . (list/c any/c (hash/c symbol? list?)))))
 ; Add any helper functions you tested in A1.eva-test.rkt.
 ; Whether you add contracts to them is optional.
 #;a-helper)

; · Support: indexer

; A constructor for a zero-arity function that when called successively
; produces symbols of the form prefix0 prefix1 prefix2 etc.

(provide (contract-out (indexer (any/c . -> . (-> symbol?)))))

(define (indexer prefix)
  (define last-index -1)
  (λ ()
    (local-require (only-in racket/syntax format-symbol))
    (set! last-index (add1 last-index))
    (format-symbol "~a~a" prefix last-index)))

; · eve

; Produce a two-element list with the value and the environment-closure table
; from evaluating an LCE term.
(define (eve term)

  ; A mutable table of environments and closures.
  ; Task: Look up hash-ref and hash-set! in the racket documentation.
  (define environments-closures (make-hash))
  
  ; Iterators to produce indices for environments and closures.
  (define En (indexer 'E))
  (define λn (indexer 'λ))

    ;Variable lookup function
  (define (var_lookup var E)
    (define curr_binding (first (hash-ref environments-closures E)))
    (define next_env (second (hash-ref environments-closures E)))
    (cond [(equal? var (first curr_binding)) (second curr_binding)]
          [else (var_lookup var next_env)]))

  ; Task: complete rec.
  (define (rec t E) (match t
                      ;if t is a lambda term
                      [(list 'λ a b)
                         (define new_clos (λn))
                         (hash-set! environments-closures new_clos (list t E))
                         new_clos]
                      ;if t is a function term
                      [(list a b)
                         (define func_eval (rec a E))
                         (define arg_eval (rec b E))
                         (define clos (hash-ref environments-closures func_eval))
                         (define func_id (first (second (first clos))))
                         (define func_body (last (first clos)))
                         (define new_env (En))
                         (hash-set! environments-closures new_env (list (list func_id arg_eval) (second clos)))
                         (rec func_body new_env)]
                      ;if t is a variable
                      [a #:when (symbol? t)
                         (var_lookup t E)]
                      ;if t is a literal
                      [_ t]
                        ))
  
  (list (rec term (En))
        (make-immutable-hash (hash->list environments-closures))))
