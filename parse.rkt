#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Int s)]
    [(? boolean?) (Bool s)]
    [(list 'add1 e)  (Prim1 'add1  (parse e))]
    [(list 'sub1 e)  (Prim1 'sub1  (parse e))]
    ;; TODO: Handle abs, - and not
    [(list 'zero? e) (Prim1 'zero? (parse e))]
    [(list 'abs e)   (Prim1 'abs   (parse e))]
    [(list '- e)     (Prim1 '-     (parse e))]
    [(list 'not e)   (Prim1 'not   (parse e))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [(list 'cond cs ... (list a b))
     (Cond (clause-helper cs '()) (parse b))]
    [(list 'case val cs ... (list a b))
     (Case (parse val) (case-helper cs '()) (parse b))]
    ;; TODO: Handle cond
    ;; TODO: Handle case
    ;; TODO: Remove this clause once you've added clauses for
    ;; parsing cond and case; it's here just so running the test suite
    ;; doesn't trigger parse errors.
    [_ (Int 0)]
    [_ (error "parse error")]))
    
(define (clause-helper cs acc)
    (match cs
       ['() acc]
       [(cons a rest)
        (match a
         [(list b c) (clause-helper rest (append acc (list (Clause (parse b) (parse c)))))] )]))

(define (case-helper cs acc)
    (match cs
     ['() acc]
     [(cons a rest)
      (match a
       [(list b c) (case-helper rest (append acc (list (Clause (case-helper2 b '())(parse c)))))] )]))
(define (case-helper2 cs acc)
  (match cs
   ['() acc] 
   [(cons a rest)
    (case-helper2 rest (append acc (list (parse a))))]))

