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

  ; gets square at given position (or a null square, which is just
  ; a solid tile
  (define null-square (make-square #\#))
  (define (get-square x y)
    (if (or (< x 0) (< y 0) (>= x (width)) (>= y (height)))
        null-square
        (list-ref (list-ref array y) x)))

  ; dimensions of maze
  (define (width)
    (length (car array)))
  (define (height)
    (length array))

  ; finds location of square with given type
  (define (find-square type)
    (let* ((x #f)
           (y (index-where
               array
               (lambda (row)
                 (let ((row-pos
                        (index-where
                         row
                         (lambda (square)
                           (equal? ((square 'type)) type)))))
                   (when row-pos
                     (set! x row-pos))
                   row-pos)))))
      (if y (cons x y) #f)))

  ; allow methods to be accessed using (maze-instance 'method-name)
  (define (dispatch method)
    (cond ((equal? method 'array) get-array)
          ((equal? method 'print) print)
          ((equal? method 'get-square) get-square)
          ((equal? method 'width) width)
          ((equal? method 'height) height)
          ((equal? method 'find-square) find-square)
          (else (error (string-append "Method "
                                      (symbol->string method)
                                      " doesn't exist")))))
  dispatch)

(provide make-maze)
