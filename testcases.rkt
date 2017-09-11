#lang racket

(provide (all-defined-out))

(define example1
  '((lambda (apply k1)
      (apply (lambda (x1 k2) (+ x1 1 k2))
             (lambda (t2) (apply t2 (lambda (t3) (t3 2 k1))))))
    (lambda (f k3) (k3 (lambda (x2 k4) (f x2 k4))))
    (lambda (x) (halt x))))

(define example2
  '((lambda (x) (halt x))
    (lambda (y) (halt y))))

(define example3
  '((lambda (f c1)
      ((lambda (x c2)
         (f x c2))
       2
       c1))
    (lambda (y c3) (+ y c3))
    1))

(define example4
  '((lambda (x k) (k (lambda (a) (halt a))))
    3
    (lambda (z) (halt z))))
