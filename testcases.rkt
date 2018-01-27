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

(define example5
  '((lambda (f1 k1)
      ((lambda (f2 k2)
         ((lambda (f3 k3)
            ((lambda (f4 k4)
               ((lambda (f5 k5)
                  ((lambda (f6 k6)
                     ((lambda (f7 k7)
                        ((lambda (f8 k8)
                           ((lambda (f9 k9)
                              ((lambda (f10 k10)
                                 ((lambda (f11 k11)
                                    ((lambda (f12 k12) (k12 f12))
                                     f11
                                     k11))
                                  f10
                                  k10))
                               f9
                               k9))
                            f8
                            k8))
                         f7
                         k7))
                      f6
                      k6))
                   f5
                   k5))
                f4
                k4))
             f3
             k3))
          f2
          k2))
       f1
       k1))
    (lambda (y) (halt y))
    (lambda (x) (halt x))))