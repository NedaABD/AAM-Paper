#lang racket

;; Figure 1
;; To evaluate a program, which
'((λ (z) z) ((λ (x) x) (λ (y) y)))
;; A state in the CEK machine
;; is ς ∈ Σ = Exp × Env × Kont
;; To initialize the machine



(define (inj-cek e)
  `(,e ,(hash) done))

;;Description:

;;(inj-cek e): This function initializes the CEK machine. It takes an expression e as input
;;and constructs the initial state of the machine, represented as a tuple (e, ρ, done), where e is the expression to be evaluated,
;;ρ is the environment (initially empty), and done indicates whether the evaluation is complete. The function returns this tuple.


(define (empty-map)
  (lambda (x) (error "no value")))

;;Error Handling: Returning an error when a symbol is queried in the empty environment serves as a form of error handling. 
;;It ensures that if a symbol is not found in the environment (which should not happen in normal circumstances),
;;an error will be raised, indicating that the value associated with the symbol is not found. This helps in debugging and identifying potential issues in the evaluation process.


(define (ext-map map x v)
  (lambda (y) (if (equal? x y)
                  v
                  (map x))))

(define (syntactic-value? v)
  (match v
    [`(λ (,x) ,e) #t]
    [(? number? n) #t]
    [_ #f]))

(define (steps-to ς)
  (match ς
    [`(,(? symbol? x) ,ρ ,k)
     (match (hash-ref ρ x)
       [`(closure ,lam ,ρ+)
        `(,lam ,ρ+ ,k)]
       [(? number? n) `(,n ,ρ ,k)])]
    [`((,e0 ,e1) ,ρ ,k)
     `(,e0 ,ρ (ar ,e1 ,ρ ,k))]
    [`(,(? syntactic-value? v) ,ρ (ar ,e ,ρ+ ,k))
     `(,e ,ρ+ (fn ,v ,ρ ,k))]
    [`((λ (,y) ,e-b) ,ρ (fn (λ (,x) ,e) ,ρ+ ,k))
     `(,e ,(hash-set ρ+ x `(closure (λ (,y) ,e-b) ,ρ)) ,k)]
    [`(,(? number? n) ,ρ (fn (λ (,x) ,e) ,ρ+ ,k))
     `(,e ,(hash-set ρ+ x n) ,k)]))

(define (compute e)
  (define (done? ς)
  (match ς
    [`((λ (,x) ,e) ,ρ done) #t]
    [`((? number? n) ,ρ done) #t]
    [else #f]))
  (define (loop e)
    (if (done? e)
        (begin (pretty-print e) (displayln "done"))
        (begin
          (displayln "Current machine state ς is:")
          (pretty-print e)
          (displayln "⟶")
          (loop (steps-to e)))))
  (loop (inj-cek e)))
