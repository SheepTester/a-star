#lang racket

(require "./square.rkt")

(define (make-maze str)
  ; 2D list of all the squares
  (define array
    (map (lambda (row)
           (map make-square (string->list row)))
         (string-split str)))

  (define (print)
    (display
     (string-join
      (map (lambda (row)
             (list->string
              (map (lambda (square)
                     ((square 'square->char)))
                   row)))
           array)
      "\n")))

  (define (get-square x y)
    (list-ref (list-ref array y) x))

  ; allow methods to be accessed using (maze-instance 'method-name)
  (define (dispatch method)
    (cond ((equal? method 'print) print)
          ((equal? method 'get-square) get-square)
          (else (error "Method doesn't exist"))))
  dispatch)

(provide make-maze)

(define test-maze (make-maze "############
#.#........#
#.#.######.#
#.#....#...#
#.###.*#.#.#
#...####.#.#
#.#.#..#.#.#
#.#.#.##.#.#
#o#......#.#
############"))
((((test-maze 'get-square) 1 1) 'set!) 'finish)
((((test-maze 'get-square) 1 1) 'type))
((test-maze 'print))