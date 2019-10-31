#lang racket

; needed to access make-color
(require racket/draw)

; makes a square (either #, ., o, or *)
(define (make-square char)
  ; the type of square
  (define type
    (cond ((equal? char #\#) 'wall)
          ((equal? char #\.) 'space)
          ((equal? char #\o) 'start)
          ((equal? char #\*) 'finish)
          ((equal? char #\x) 'explored)
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
          ((equal? type 'explored) #\x)
          (else (error "Type cannot be converted to char"))))

  ; return tile colour as according to type
  (define (colour)
    (cond ((equal? type 'wall) (make-color 40 40 40))
          ((equal? type 'space) (make-color 238 238 238))
          ((equal? type 'start) (make-color 2 253 182))
          ((equal? type 'finish) (make-color 253 72 2))
          ((equal? type 'explored) (make-color 127 127 127))
          (else (error "Type does not have colour"))))

  ; allow methods to be accessed using (square-instance 'method-name)
  (define (dispatch method)
    (cond ((equal? method 'type) get-type)
          ((equal? method 'set!) set-to!)
          ((equal? method 'square->char) square->char)
          ((equal? method 'colour) colour)
          (else (error (string-append "Method "
                                      (symbol->string method)
                                      " doesn't exist")))))
  dispatch)

(provide make-square)
