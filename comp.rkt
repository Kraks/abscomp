#lang racket

#| Implementation of 0CFA with Closure Generation |#
#| Code adapted from Abstract Compilation, Chapter 4 |#

(require rackunit)
(require "share.rkt")
(require "0cfa.rkt")
(require "testcases.rkt")

(define (comp-program prog)
  (comp-call prog))

(define (comp-call e)
  (when (debug) (printf "0cfa-call: ~a\n" e))
  (match e
    [`(letrec ([,var ,l] ...) ,body)
     (define C1 (comp-call body))
     (define C2 (comp-args l))
     (λ (σ) (C1 (C2 (update/multi σ var l))))]
    [`(,f ,args ...)
     (define C1 (comp-app f args))
     (define C2 (comp-args args))
     (cond [(identity? C1) C2]
           [(identity? C2) C1]
           [else (λ (σ) (C1 (C2 σ)))])]))

(define (comp-app f args)
  (when (debug) (printf "0cfa-app: ~a\n" f))
  (cond [(var? f) (λ (σ) (0cfa-abstract-app (lookup σ f) args σ))]
        [(prim? f) (0cfa-prim f args)]
        [(lambda? f)
         (define C (comp-call (lambda-body f)))
         (λ (σ) (C (update/multi σ
                                 (lambda-args f)
                                 (map (λ (a) (lookup σ a)) args))))]))

(define (comp-args args)
  (when (debug) (printf "0cfa-args: ~a\n" args))
  (match args
    [(list) (λ (σ) σ)]
    [`(,arg ,args ...)
     (if (lambda? arg)
         (let ([C1 (comp-call (lambda-body arg))]
               [C2 (comp-args args)])
           (λ (σ) (C2 (C1 σ))))
         (comp-args args))]))

; Set[Lam] Arg* Env -> Env
(define (0cfa-abstract-app fs args σ)
  (when (debug) (printf "0cfa-abstract-app: ~a\n" fs))
  (cond [(set-empty? fs) σ]
        [else
         (define f (set-first fs))
         (define rest (set-remove fs f))
         (let* ([args* (map (λ (a) (lookup σ a)) args)]
                [σ* (update/multi σ (lambda-args f) args*)])
           (0cfa-abstract-app rest args σ*))]))

(define (0cfa-prim op args σ)
  (when (debug) (printf "0cfa-prim: ~a\n" args))
  (comp-args args σ))

(define (comp/analysis prog)
  (define compiled (comp-program prog))
  (define (iter σ)
    (define σ* (compiled σ))
    (if (equal? σ* σ) σ (iter σ*)))
  (iter (compiled mt-store)))

(module+ test
  (check-equal? (comp/analysis example1)
                (0cfa-analysis example1))

  (check-equal? (comp/analysis example2)
                (0cfa-analysis example2))

  (check-equal? (comp/analysis example3)
                (0cfa-analysis example3))

  (check-equal? (comp/analysis example4)
                (0cfa-analysis example4))
  )
