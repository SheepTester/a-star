#lang racket

; makes a square (either #, ., o, or *)
(define (make-square char)
  ; the type of square
  (define type
    (cond ((equal? char #\#) 'wall)
          ((equal? char #\.) 'space)
          ((equal? char #\o) 'start)
          ((equal? char #\*) 'finish)
          (else (error "Invalid character"))))

  ; returns the type of square
  (define (get-type) type)

  ; sets the type
  (define (set-to! new-type)
    (set! type new-type))

  ; type of square back to char
  (define (square->char)
    (cond ((equal? type 'wall) #\#)
          ((equal? type 'space) #\.)
          ((equal? type 'start) #\o)
          ((equal? type 'finish) #\*)
          (else (error "Type cannot be converted to char"))))

  ; allow methods to be accessed using (square-instance 'method-name)
  (define (dispatch method)
    (cond ((equal? method 'type) get-type)
          ((equal? method 'set!) set-to!)
          ((equal? method 'square->char) square->char)
          (else (error "Method doesn't exist"))))
  dispatch)

(provide make-square)
