#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)           (compile-integer i)]
    [(Bool b)          (compile-boolean b)]
    [(Prim1 p e)       (compile-prim p e)]
    [(If e1 e2 e3)     (compile-if e1 e2 e3)]
    [(Cond cs e)       (compile-cond cs e)]
    [(Case e cs el)    (compile-case e cs el)]
    ;; TODO: Handle case
    ))


;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax (value->bits i))))

;; Boolean -> Asm
(define (compile-boolean b)
  (seq (Mov 'rax (value->bits b))))

;; Op Expr -> Asm
(define (compile-prim p e)
  (seq (compile-e e)
       (match p
         ['add1 (Add 'rax (value->bits 1))]
         ['sub1 (Sub 'rax (value->bits 1))]
         ;; TODO: Handle abs, -, and not
	 ['abs  ;; Handle abs
	  (let ((l1 (gensym 'abs)))
	    (seq (Cmp 'rax 0)
	         (Jge l1)  ; Jump if greater than or equal (non-negativ
	         (Mov 'rcx 'rax) ; Negate if negative
	         (Sub 'rax 'rcx)
	         (Sub 'rax 'rcx)
	         (Label l1)))]
	 ['-
	   (let ((l1 (gensym 'Neg)))
	        (seq
	 	(Mov 'rcx 0)
		(Sub 'rcx 'rax)
		(Mov 'rax 'rcx)
		(Label l1)))]
	 ['not
	   (let ((l1 (gensym 'not))
		 (l2 (gensym 'not)))
	    (seq (Cmp 'rax val-false)  ; Compare 'rax' with 0
	         (Je l1)  ; Jump if 'rax' was (false), so set to true
		 (Mov 'rax val-false)
		 (Jmp l2)
	         (Label l1)
		 (Mov 'rax val-true)
		 (Label l2)))]
         ['zero?
          (let ((l1 (gensym 'nzero))
                (l2 (gensym 'nzero)))
		(seq (Cmp 'rax 0)
		 (Je l1)
                 (Mov 'rax val-false)
		 (Jmp l2)
                 (Label l1)
		 (Mov 'rax val-true)
		 (Label l2)))]))) 

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp 'rax val-false)
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

(define (compile-cond cs e)
    (match cs
	   ['() 
	    (let ((l1 (gensym 'cond)))
	      (seq (compile-e e)))]
	   [(cons a res) 
	     (match a
	      [(Clause b c)
              (let ((l1 (gensym 'cond))
		    (l2 (gensym 'cond)))
      	      (seq (compile-e b)
           	     (Cmp 'rax val-false)
           	     (Je l1)
           	     (compile-e c)
		     (Jmp l2)
           	     (Label l1)
           	     (compile-cond res e)
		     (Label l2)
           	     ))])]))


(define (compile-case e cs el)
      (match cs
       ['()
         (let ((l1 (gensym 'case)))
           (seq (compile-e el)))]
       [(cons a res)
	(match a
	 [(Clause b c)      
         (let ((l1 (gensym 'case))
               (l2 (gensym 'case)))
         (seq (compile-case-helper e b)
              (Cmp 'rax val-false)
              (Je l1)
	      (compile-e c)
              (Jmp l2)
              (Label l1)
              (compile-case e res el)
              (Label l2)
              ))])]))

(define (compile-case-helper e lst)
  (match lst
	['() 
	  (let ((l1 (gensym 'caseh)))
	    (seq (Mov 'rax val-false)))]
	[(cons a res)
	 (let ((l1 (gensym 'caseh))
	       (l2 (gensym 'caseh)))
	 (seq (compile-e e)
	       (Cmp 'rax val-true)
	       (Je l1)
	       (compile-case-helper e res)
	       (Jmp l2)
	       (Label l1)
	       (compile-e e)
	       (Label l2)
	       ))]))


