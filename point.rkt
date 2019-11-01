#lang racket

; abstract some pair procedures for maximum readability
(define point cons)
(define px car)
(define py cdr)

(provide point)
(provide px)
(provide py)
