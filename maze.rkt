#lang racket

(require "./square.rkt")

(define (make-maze str)
  ; 2D list of all the squares
  (define array
    (map (lambda (row)
           (map make-square (string->list row)))
         (string-split str)))

  ; returns the array
  (define (get-array) array)

  ; generates text-based maze representation
  (define (maze->string)
    (string-join
     (map (lambda (row)
            (list->string
             (map (lambda (square)
                    ((square 'square->char)))
                  row)))
          array)
     "\n"))

  ; prints the text-based maze representation
  (define (print)
    (display (maze->string)))

  ; gets square at given position
  (define (get-square x y)
    (list-ref (list-ref array y) x))

  ; dimensions of maze
  (define (width)
    (length (car array)))
  (define (height)
    (length array))

  ; allow methods to be accessed using (maze-instance 'method-name)
  (define (dispatch method)
    (cond ((equal? method 'array) get-array)
          ((equal? method 'print) print)
          ((equal? method 'get-square) get-square)
          ((equal? method 'width) width)
          ((equal? method 'height) height)
          (else (error (string-append "Method "
                                      (symbol->string method)
                                      " doesn't exist")))))
  dispatch)

(provide make-maze)
