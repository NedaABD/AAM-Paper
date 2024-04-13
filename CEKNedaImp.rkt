#lang racket
;;CEK machine: implementation of Figure 1 Horn and Might (AAM paper)

;; e ∈ Exp ::= x | (ee) | (λx.e)
(define (expr? e)
  (match e
    [(? symbol? x) #t]
    [`(λ (,(? symbol? x)) , (? expr? e)) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [_ #f]))

;; υ ∈ Val ::= (λx.e)
(define (value? υ)
  (match υ
    [`((λ (,(? symbol? x)) ,(? expr? e))) #t]
    [_ #f]))

;; ρ ∈ Env = Var →fin Val x Env
(define (environment? ρ)
  (and (hash? ρ)
       (andmap symbol? (hash-keys ρ))
       (andmap value? (hash-values ρ))))

;; k ∈ Kont ::= mt | ar(e,ρ,k) | fn(υ,ρ,k)

(define (continuation? k)
  (match k
    ['done #t]
    [`(ar (,(? expr? e) ,(? environment? ρ) ,(? continuation? k))) #t]
    [`(fn (,(? value? υ) ,(? environment? ρ) ,(? continuation? k))) #t]
    [_ #f]))

;; ς ∈ Σ
(define (ς? state)
  (match state
    [`(,(? expr? e) ,(? environment? ρ) ,(? continuation? k)) #t]
    [`((,(? expr? e0) ,(? expr? e1)) ,(? environment? ρ) ,(? continuation? k)) #t]
    [`(,(? value? υ) ,(? environment? ρ) (ar (,(? expr? e) ,(? environment? ρ) ,(? continuation? k)))) #t]
    [`(,(? value? υ) ,(? environment? ρ) (fn (,(? value? υ) ,(? environment? ρ) ,(? continuation? k)))) #t]
    [_ #f])) 

;; ς ↦ CEK ς'
(define (step-to ς)
  (if ς? 
      (match ς
        [`(,(? symbol? x) ,ρ ,k)
         (match (hash-ref ρ x) ;; This is an implementaion trick for ρ(x) = (v ,ρ') which is in Figure 1.
           [`(closure ,υ ,ρ+)
            `(,υ ,ρ+ ,k)])]
        [`((,e0 ,e1) ,ρ ,k)
         `(,e0 ,ρ (ar ,e1 ,ρ ,k))]
        [`(,υ ,ρ (ar ,e ,ρ+ ,k))
         `(,e ,ρ+ (fn ,υ ,ρ ,k))]
        [`(,υ ,ρ (fn (λ (,x) ,e-b) ,ρ+ ,k))
         `(,e-b ,(hash-set ρ+ x `(closure ,υ , ρ)) ,k)])
      "state is wrong"))
                 
(define (inj-cek e)
  `(,e ,(hash) done))
         
;;> (inj-cek '((λ (z) z) ((λ (x) x) (λ (y) y))))
;;'(((λ (z) z) ((λ (x) x) (λ (y) y))) #hash() done)
;;> (step-to (inj-cek '((λ (z) z) ((λ (x) x) (λ (y) y)))))
;;'((λ (z) z) #hash() (ar ((λ (x) x) (λ (y) y)) #hash() done))
;;> (step-to (step-to (inj-cek '((λ (z) z) ((λ (x) x) (λ (y) y))))))
;;'(((λ (x) x) (λ (y) y)) #hash() (fn (λ (z) z) #hash() done))
;;> (step-to (step-to (step-to (inj-cek '((λ (z) z) ((λ (x) x) (λ (y) y)))))))
;;'((λ (x) x) #hash() (ar (λ (y) y) #hash() (fn (λ (z) z) #hash() done)))
;;> (step-to (step-to (step-to (step-to (inj-cek '((λ (z) z) ((λ (x) x) (λ (y) y))))))))
;;'((λ (y) y) #hash() (fn (λ (x) x) #hash() (fn (λ (z) z) #hash() done)))
;;> (step-to (step-to (step-to (step-to (step-to (inj-cek '((λ (z) z) ((λ (x) x) (λ (y) y)))))))))
;;'(x #hash((x . (closure (λ (y) y) #hash()))) (fn (λ (z) z) #hash() done))
;;> (step-to (step-to (step-to (step-to (step-to (step-to (inj-cek '((λ (z) z) ((λ (x) x) (λ (y) y))))))))))
;;'((λ (y) y) #hash() (fn (λ (z) z) #hash() done))
;;> (step-to (step-to (step-to (step-to (step-to (step-to (step-to (inj-cek '((λ (z) z) ((λ (x) x) (λ (y) y)))))))))))
;;'(z #hash((z . (closure (λ (y) y) #hash()))) done)
;;> (step-to (step-to (step-to (step-to (step-to (step-to (step-to (step-to (inj-cek '((λ (z) z) ((λ (x) x) (λ (y) y))))))))))))
;;'((λ (y) y) #hash() done)    
