#lang racket ; CSC324 — 2022W — Assignment 1 - Evo Design and Testing

; • Evo: Eager By-Value Stateful Evaluator for an Extended Lambda Calculus

; Task: understand the syntax and semantics of the language LCO described below,
; then create a good test suite for an evaluator of LCO named evo which you will
; then implement in A1.evo.rkt

(require "A1.evo.rkt"
         rackunit)

; We'll refer to the language being interpreted in this part as “LCO”.

; · Terms in LCO

; The grammar of “terms” [along with how we'll refer to them] in LCO is:
;   term = (λ (<parameter-identifier>) <body-term> ... <result-term>) [“λ term”]
;        | (<function-term> <argument-term>)  [“function call term”]
;        | (set! <variable-identifier> <update-term>)  [“assignment term”]
;        | <identifier>  [“variable term”]
;        | <literal>  [“literal term”]

; · Values, Closures, and Environments in LCO

; LCO has the same values, closures, and environments, as LCE except that
; binding values in environments are now stored inside mutable boxes
; (a datatype for replaceable values).

(check-true (box? #&108)) ; a literal (but immutable) box
(check-equal? (box 108) #&108) ; the mutable box constructor from a value
(check-equal? (unbox #&108) 108) ; extracting the value from a box
(check-equal? (let ((b (box 108)))
                (set-box! b 324)
                (unbox b))
              324)

; Notice that racket procedures are valid literal terms in LCA and LCE/LCO:
#;(check-equal? (eva add1) add1)
#;(check-equal? (eve add1) `(,add1 #hash()))
#;(check-equal? (evo add1) `(,add1 #hash()))

; We'll give a semantics to function call where the function term evaluates to
; a racket procedure. A convenient way to embed a procedure in a term is to use
; “quasiquotating” which is like quoting but can be escaped out of (“unquoted”).
; The quasiquoting character is the backwards single-quote, aka back-tick, and
; the unquoting character is the upside-down quasiquote, aka comma:
(check-equal? '(add1 2) (list 'add1 2))
(check-not-equal? '(add1 2) (list add1 2))
(check-equal? '(add1 2) `(add1 2)) ; no change since no unquoting
(check-true (symbol? (first `(add1 2))))
(check-equal? `(,add1 2) (list add1 2)) ; use variable's value
(check-true (procedure? (first `(,add1 2))))

; · Semantics of LCO

; Evaluation still takes a term and an index of an appropriate environment.

; Evaluation of a value or λ is as for LCE.

; Evaluation of a variable is as for LCE (with the extra step of extracting
; its current value from the box it is stored in).

; Evaluation of function call allows the value of the function term to be a
; racket procedure, in which case the evaluation is the result of calling that
; procedure on the value of the argument term.

; Evaluation of function call when the value of the function term is (an index
; of) a closure is evaluation (in the new environment as described in LCE) of
; each body term (in order) followed by producing the value of the result term. TODO

; Evaluation of assignment is evaluation of the update term in the current
; environment, replacement of the variable's value in the current environment,
; producing the void value (which the implementation names and exports as Void).

; · Design and Testing

; Create a good test suite for evo, with a comment above each test case giving
; a clear rationale for its inclusion.

; Also include test cases for any significant helper functions you create
; (e.g. a recursive function is likely to be significant) to aid debugging, and
; export those helper functions from A1.evo.rkt so they can be referenced here.

; Illustrate embedding racket function call, boxed values in environments, and
; result value of assignment.
(check-equal? (evo `((λ (x) (set! x (,add1 x))) 1))
                `(,Void #hash((E1 . ((x #&2) E0))
                              (λ0 . ((λ (x) (set! x (,add1 x))) E0)))))

;TEST SUITE
(check-equal? (evo `((λ (x) ((λ (y) (set! x (,add1 x))) 2)) 1))
              `(,Void
                #hash((E1 . ((x #&2) E0))
                      (E2 . ((y #&2) E1))
                      (λ0 . ((λ (x) ((λ (y) (set! x (,add1 x))) 2)) E0))
                      (λ1 . ((λ (y) (set! x (,add1 x))) E1)))))


; Definitions
(define environments-closures #hash((E2 . ((y #&324) E1))
                                    (E1 . ((x #&2) E0))
                                    (λ0 . ((λ (x) (set! x (,add1 x))) E0))))

;Mini test from above
(check-equal? (evo add1) `(,add1 #hash()))

; Evaluation of a value is itself
(check-equal? (evo 3) `(3 #hash()))
(check-equal? (evo "jon") `("jon" #hash()))

; Evaluation of a lambda function is the value of a new closure containing
; the term and environment index
(check-equal? (evo '(λ (x) 3)) '(λ0 #hash((λ0 . ((λ (x) 3) E0)))))
(check-equal? (evo '(λ (x) ((λ (y) "z"))))
              '(λ0 #hash((λ0 . ((λ (x) ((λ (y) "z"))) E0)))))
(check-equal? (evo '(λ (x) ((λ (y) ((λ (z) "value"))))))
              '(λ0 #hash((λ0 . ((λ (x) ((λ (y) ((λ (z) "value"))))) E0)))))

; Evaluation of function call allows the value of the function term to be a
; racket procedure
(check-equal? (evo (add1 2)) `(3 #hash()))
(check-equal? (evo (length '(1 2 3 4))) `(4 #hash()))


; Helper functions
; Variable lookup function.
  (define (var_lookup var E)
    (define curr_binding (first (hash-ref environments-closures E)))
    (define next_env (second (hash-ref environments-closures E)))
    (cond [(equal? var (first curr_binding)) (second curr_binding)]
          [else (var_lookup var next_env)]))

; (should return a box containing the number 2)
(check-equal? #&2 (var_lookup 'x 'E1))

; (should return a box containing the number 324)
(check-equal? #&324 (var_lookup 'y 'E2))

; (should look up the environment chain and return a box containing the number 2)
(check-equal? #&2 (var_lookup 'x 'E2))



