#lang racket

; CSC324 — 2022W — Assignment 2 — Type Constraints Implementation

(provide (contract-out (type-and-constraints
                        (-> any/c any/c
                            (list/c any/c (listof (list/c any/c any/c))))))
         α?
         α-type-generator)

; · Support

; Whether t represents a type variable.
(define (α? t)
  (and (symbol? t) (equal? #\α (string-ref (symbol->string t) 0))))

; Generator generator for type variables.
#;((α-type-generator)) ; type variable of the form αn
#;((α-type-generator) id) ; type variable of the form αid
(define (α-type-generator)
  (let ([index -1])
    (λ ((id #f))
      (set! index (add1 index))
      (local-require (only-in racket/syntax format-symbol))
      (format-symbol "α~a" (if id id index)))))

; · type-and-constraints

; A type and list of constraints for expression e, using αs to generate
; type variables.
(define (type-and-constraints e αs)
  (match e
    [x #:when (boolean? x) '(Boolean ())]
    [x #:when (number? x) '(Number ())]
    [x #:when (symbol? x) (list (αs x) '())]
    [(list 'λ (list identifier) body)
     ((λ (x) (list (list '-> (first (type-and-constraints identifier αs)) (first x)) (second x))) (type-and-constraints body αs))]
    [(list func arg)
     (define func-tc (type-and-constraints func αs))
     (define arg-tc (type-and-constraints arg αs))
     (define func-tc-type (first func-tc))
     (define arg-tc-type (first arg-tc))
     (define func-tc-constraints (second func-tc))
     (define arg-tc-constraints (second arg-tc))
     (define alph (αs))
     (define eq-constraints (append (list (list func-tc-type (list '-> arg-tc-type alph))) func-tc-constraints arg-tc-constraints))
     (list alph eq-constraints)]))

