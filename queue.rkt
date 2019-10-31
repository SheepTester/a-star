#lang racket

; (require rnrs/mutable-pairs-6)

(define (make-queue)
  ; pointers to first and last pair in queue (null when default)
  (define first '())
  (define last '())

  ; whether or not the queue is empty
  (define (empty?)
    (null? first))

  ; the number of items in the queue
  (define (size)
    ; there is no mlength :/
    (define (iter mlist count)
      (if (null? mlist) count
          (iter (mcdr mlist) (+ count 1))))
    (iter first 0))

  ; adds item to end of queue
  (define (add! item)
    (if (empty?)
        ; if empty, make a new list and make both pointer point to it
        (let ((new-queue (mcons item '())))
          (set! first new-queue)
          (set! last new-queue))
        ; else, make the cdr at the end of the queue hold a new pair
        ; then point the last pair pointer to the new pair
        (begin
          (set-mcdr! last (mcons item '()))
          (set! last (mcdr last)))))
  
  ; the item at front of queue
  (define (peek)
    (mcar first))

  ; remove and return the item at front of queue
  (define (remove!)
    (let ((removed (peek)))
      ; point first pair pointer to next pair
      (set! first (mcdr first))
      ; if first pair pointer is null, the queue must be empty,
      ; so set last pair pointer to null too
      (when (null? first)
        (set! last '()))
      removed))

  ; allow methods to be accessed using (queue-instance 'method-name)
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

(provide make-queue)
