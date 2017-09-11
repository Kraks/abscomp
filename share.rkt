#lang racket

(provide (all-defined-out))

(define debug (make-parameter #f))

(define ⊥ (set))

(define mt-store (make-immutable-hash))

(define (lookup σ α)
  (match α
    [(? lambda?) (set α)]
    [(? number?) (set)]
    [else (hash-ref σ α ⊥)]))

(define (update* σ α setd)
  (when (debug) (printf "~a -> ~a\n" α setd))
  (hash-update σ α (λ (d) (set-union d setd)) ⊥))

(define (update σ α d)
  (cond [(set? d) (update* σ α d)]
        [else (update* σ α (set d))]))

(define (update/multi σ as ds)
  (foldl (λ (a d σ) (update σ a d)) σ as ds))

(define (var? x)
  (and (symbol? x) (not (number? x))))

(define (prim? x)
  (or (eq? x '+) (eq? x 'if)))

(define (lambda? x)
  (match x
    [`(lambda (,args ...) ,body) #t]
    [else #f]))

(define (lambda-args l) (cadr l))

(define (lambda-body l) (caddr l))
