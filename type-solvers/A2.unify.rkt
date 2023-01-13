#lang racket

; CSC324 — 2022W — Assignment 2 — Unifier Implementation

(provide (contract-out (mgu (-> (-> any/c boolean?)
                                (listof (list/c any/c any/c))
                                (or/c #f (-> any/c any/c))))))

; The mgu of equations, or #f if there is no mgu, where the unary predicate
; variable? identifies variables.
(define (mgu variable? equations)
  (match equations
    [(list) (λ (x) x)]
    [_ (define hash-list '())
       (cond [(in-list? #f (map (λ (x) (mgu-exists? x variable?)) equations)) #f]
             [else (define final-list (reverse (rec variable? equations hash-list))) (cond [(empty? final-list) (λ (x) x)]
                                                                                           [(λ (x) (final-rec-replace final-list x))])])]))


(define (rec variable? equations hash-list)
  (cond [(empty? equations) hash-list]
        [(variable? (first (first equations))) (rec
                                                   variable?
                                                 (rec-replace (first equations) (rest equations))
                                                 (list* (first equations) hash-list))]
        [(equal? (first (first equations)) (second (first equations))) (rec variable? (rest equations) hash-list)]
        [(and (list? (first (first equations))) (list? (second (first equations)))) (rec variable? (rec-compound (first equations) (rest equations)) hash-list)]
        [(variable? (second (first equations))) (rec
                                                    variable?
                                                  (list* (list (second (first equations)) (first (first equations))) (rest equations)) hash-list)]))


(define (final-rec-replace hash1 hash2)
  (cond [(empty? hash1) hash2]
        [else (final-rec-replace (rest hash1) (final-rec-replace-helper (first hash1) hash2))]))


(define (final-rec-replace-helper expr body)
  (cond [(equal? expr body) expr]
        [(list? body) (map (λ (x) (final-rec-replace-helper expr x)) body)]
        [(equal? (first expr) body) (second expr)]
        [else body]))

;gives new equations list with instances of expr replaced
(define (rec-replace expr body)
  (cond [(and (list? body) (empty? body)) body]
        [(list? body) (map (λ (x) (rec-replace expr x)) body)]
        [(equal? (first expr) body) (second expr)]
        [else body]))


(define (rec-compound expr rest-equations)
  (define first-no-functor (rest (first expr)))
  (define second-no-functor (rest (second expr)))
  (rec-compound-helper first-no-functor second-no-functor rest-equations))


(define (rec-compound-helper frst scnd rest-equations)
  (cond [(empty? frst) rest-equations]
        [else (rec-compound-helper (drop-right frst 1) (drop-right scnd 1) (list* (list (last frst) (last scnd)) rest-equations))]))


(define (in-list? expr lst)
  (define res (member expr lst))
  (cond [(equal? res #f) #f]
        [else #t]))


(define (mgu-exists? equation variable?)
  (cond
    [(or (variable? (first equation)) (variable? (second equation))) #t]
    [(and (list? (first equation))
          (list? (second equation))
          (equal? (first (first equation)) (first (second equation)))
          (equal? (length (rest (first equation))) (length (rest (second equation))))) #t]
    [(and (and (not (list? (first equation))) (not (list? (second equation))))
          (equal? (first equation) (second equation))) #t]
    [else #f]))

