#lang racket

(require "./square.rkt")
(require "./point.rkt")

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

  ; finds all locations of squares with the given type
  (define (find-squares type)
    (define (iter x y row rows locs)
      (if (null? row)
          (if (null? rows) locs
              (iter 0 (+ y 1) (car rows) (cdr rows) locs))
          (iter (+ x 1)
                y
                (cdr row)
                rows
                (if (equal? (((car row) 'type)) type)
                    (cons (point x y) locs)
                    locs))))
    (iter 0 0 (car array) (cdr array) '()))

  ; allow methods to be accessed using (maze-instance 'method-name)
  (define (dispatch method)
    (cond ((equal? method 'array) get-array)
          ((equal? method 'print) print)
          ((equal? method 'get-square) get-square)
          ((equal? method 'width) width)
          ((equal? method 'height) height)
          ((equal? method 'find-square) find-square)
          ((equal? method 'find-squares) find-squares)
          (else (error (string-append "Method "
                                      (symbol->string method)
                                      " doesn't exist")))))
  dispatch)

(provide make-maze)
