#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Int s)]
    [(? boolean?) (Bool s)]
    [(list 'add1 e)  (Prim1 'add1 (parse e))]
    [(list 'sub1 e)  (Prim1 'sub1 (parse e))]
    ;; TODO: Handle abs, - and not
    [(list 'zero? e) (Prim1 'zero? (parse e))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    ;; TODO: Handle cond
    ;; TODO: Handle case
    ;; TODO: Remove this clause once you've added clauses for
    ;; parsing cond and case; it's here just so running the test suite
    ;; doesn't trigger parse errors.
    [_ (Int 0)]
    [_ (error "parse error")]))



