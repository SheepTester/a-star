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

; goodness-fn is the heuristic function that estimates how
; good a square will be for quickly getting to finish
; implementation based on
; https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode
(define (a-star maze goodness-fn)
  (define start-loc ((maze 'find-square) 'start))
  (define agenda (list start-loc))
  (let ((start ((maze 'get-square) (px start-loc) (py start-loc))))
    ((start 'set!) 'dist 0)
    ((start 'set!) 'goodness
                   (goodness-fn (px start-loc) (py start-loc))))

  (lambda ()
    ; if agenda is empty, it wasn't able to find finish
    (if (null? agenda) (lambda () #f)
        ; get closest square to finish
        (let* ((moved-list (move-min-to-front
                            (lambda (loc)
                              ((((maze 'get-square) (px loc) (py loc))
                                'closeness)))
                            agenda))
               ; location of that square
               (loc (car moved-list))
               ; the square itself
               (square ((maze 'get-square) (px loc) (py loc)))
               ; the number of squares to one of its neighbours
               ; through this square
               (dist-to-next (+ ((square 'get) 'dist) 1)))
          
          ; considers an adjacent square for being one of the
          ; next squares
          (define (consider-next x y)
            (let ((next-square ((maze 'get-square) x y)))
              ; is it a valid path square?
              (when (and (not (equal? ((next-square 'type))
                                      'wall))
                         ; is this the fastest way to
                         ; get to this square (so far?)
                         (< dist-to-next
                            ((next-square 'get) 'dist)))
                ; refer to current square's location for path
                ; retracing
                ((next-square 'set!) 'previous loc)
                ; remember how long it takes to get here
                ; this way
                ((next-square 'set!) 'dist dist-to-next)
                ; if this square isn't already going to be
                ; processed
                (when (not (member next-square agenda))
                  ; make sure its goodness has been calculated
                  ; (for determining closeless: f score)
                  ((next-square 'set!) 'goodness
                                       (goodness-fn x y))
                  ; add location to agenda
                  (set! agenda
                        (cons (point x y)
                              agenda))))))
          
          ; mark the square as visually explored
          ((square 'set!) 'explored #t)
          ; remove this square from agenda
          (set! agenda (cdr moved-list))
          ; if we've reached finish, it's supposed to be the
          ; closest path! :D
          (if (equal? ((square 'type)) 'finish)
              (lambda () loc)
              (begin
                ; for each adjacent square
                (for-each
                 (lambda (offset)
                   (consider-next (+ (px loc) (px offset))
                                  (+ (py loc) (py offset))))
                 '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
                ; also consider the complementary teleporter
                (when (equal? ((square 'type)) 'teleport)
                  (let ((next-loc ((square 'get) 'complement)))
                    (consider-next (px next-loc) (py next-loc))))
                ; return false to let the caller know it is not done
                #f))))))

(provide a-star)
