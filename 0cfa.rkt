#lang racket

#| Implementation of 0CFA |#
#| Code adapted from Abstract Compilation, Chapter 2 |#

(require rackunit)
(require "share.rkt")
(require "testcases.rkt")

(provide (rename-out [analysis 0cfa-analysis]))

; Prog Env -> Env
(define (0cfa-program prog σ)
  (0cfa-call prog σ))

; Call Env -> Env
(define (0cfa-call e σ)
  (when (debug) (printf "0cfa-call: ~a\n" e))
  (match e
    ;; TODO: test letrec
    [`(letrec ([,var ,l] ...) ,body)
     (define σ* (update/multi σ var l))
     (define σ** (0cfa-args l σ*))
     (0cfa-call body σ**)]
    [`(,f ,args ...)
     (0cfa-app f args (0cfa-args args σ))]))

; Fun Arg* Env -> Env
(define (0cfa-app f args σ)
  (when (debug) (printf "0cfa-app: ~a\n" f))
  (cond [(var? f) (0cfa-abstract-app (lookup σ f) args σ)]
        [(prim? f) (0cfa-prim f args σ)]
        [(lambda? f)
         (define args* (map (λ (a) (lookup σ a)) args))
         (define σ* (update/multi σ (lambda-args f) args*))
         (0cfa-call (lambda-body f) σ*)]))

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

; Arg* Env -> Env
(define (0cfa-args args σ)
  (when (debug) (printf "0cfa-args: ~a\n" args))
  (match args
    [(list) σ]
    [`(,arg ,args ...)
     (define σ* (if (lambda? arg)
                    (0cfa-call (lambda-body arg) σ)
                    σ))
     (0cfa-args args σ*)]))

; Prim Arg* Env -> Env
(define (0cfa-prim op args σ)
  (when (debug) (printf "0cfa-prim: ~a\n" args))
  (0cfa-args args σ))

(define (analysis prog)
  (define (iter σ)
    (define σ* (0cfa-program prog σ))
    (if (equal? σ* σ) σ (iter σ*)))
  (iter mt-store))

(module+ test
  (check-equal?
   (update (update/multi mt-store '(a b c) '(1 2 3)) 'c 4)
   (hash 'a (set 1) 'c (set 3 4) 'b (set 2)))

  (check-equal?
   (lookup (update (update/multi mt-store '(a b c) '(1 2 3)) 'c 4) 'c)
   (set 3 4))
)

(module+ test
  (parameterize ([debug #f])

    (check-equal? (analysis example2)
                  (hash 'x (set '(lambda (y) (halt y)))))


    (check-equal? (analysis example3)
                  (hash 'x (set) 'y (set) 'c3 (set) 'c2 (set) 'c1 (set)
                        'f (set '(lambda (y c3) (+ y c3)))))


    (check-equal? (analysis example4)
                  (hash 'x (set) 'z (set '(lambda (a) (halt a))) 'k (set '(lambda (z) (halt z)))))

    (check-equal? (analysis example1)
                  (hash
                   't3
                   (set '(lambda (x2 k4) (f x2 k4)))
                   'k4
                   (set '(lambda (x) (halt x)))
                   'x2
                   (set)
                   'x1
                   (set)
                   'apply
                   (set '(lambda (f k3) (k3 (lambda (x2 k4) (f x2 k4)))))
                   't2
                   (set '(lambda (x2 k4) (f x2 k4)))
                   'f
                   (set '(lambda (x2 k4) (f x2 k4)) '(lambda (x1 k2) (+ x1 1 k2)))
                   'k3
                   (set '(lambda (t3) (t3 2 k1)) '(lambda (t2) (apply t2 (lambda (t3) (t3 2 k1)))))
                   'k2
                   (set '(lambda (x) (halt x)))
                   'k1
                   (set '(lambda (x) (halt x)))))
    ))