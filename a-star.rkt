#lang racket

(require "./point.rkt")

; helper procedure that returns an arbitrarily rearranged list
; with the minimum value (determined by given procedure) at
; the front; used for finding a square with the smallest
; helpfulness value (f score)
(define (move-min-to-front fn ls)
  (define (iter min min-val new-list old-list)
    (if (null? old-list) (cons min new-list)
        (let ((value (fn (car old-list))))
          (if (< value min-val)
              (iter (car old-list) value
                    (cons min new-list)
                    (cdr old-list))
              (iter min min-val
                    (cons (car old-list) new-list)
                    (cdr old-list))))))
  (if (null? ls) '()
      (iter (car ls) (fn (car ls)) '() (cdr ls))))