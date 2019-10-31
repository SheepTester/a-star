#lang racket

(define (make-stack)
  ; store stack in list
  (define stack '())

  ; whether or not the stack is empty
  (define (empty?)
    (null? stack))

  ; the number of items in the stack
  (define (size)
    (length stack))

  ; adds item to top of stack
  (define (add! item)
    (set! stack (cons item stack)))

  ; the item on top of stack
  (define (peek)
    (car stack))

  ; remove and return the item on top
  (define (remove!)
    (let ((removed (peek)))
      (set! stack (cdr stack))
      removed))

  ; allow methods to be accessed using (stack-instance 'method-name)
  (define (dispatch method)
    (cond ((equal? method 'empty?) empty?)
          ((equal? method 'size) size)
          ((equal? method 'add!) add!)
          ((equal? method 'peek) peek)
          ((equal? method 'remove!) remove!)
          (else (error (string-append "Method "
                                      (symbol->string method)
                                      " doesn't exist")))))
  dispatch)

(provide make-stack)