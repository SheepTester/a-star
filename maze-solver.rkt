#lang racket

(require "./point.rkt")

(define (maze-solver maze agenda)
  ((agenda 'add!) (cons #f ((maze 'find-square) 'start)))

  ; return step procedure; it will return #f if it is not done
  ; or a lambda resolving to #t (reachable) or #f
  ; (too used to JS promises lol)
  (lambda ()
    (if ((agenda 'empty?)) (lambda () #f)
        (let* ((entry ((agenda 'remove!)))
               (loc (cdr entry))
               (square ((maze 'get-square) (px loc) (py loc))))
          (cond (((square 'get) 'explored) #f)
                ((equal? ((square 'type)) 'finish)
                 (lambda () (car entry)))
                (else
                 ((square 'set!) 'previous (car entry))
                 (define (consider-next x y)
                   (when
                        (not
                         (equal?
                          ((((maze 'get-square) x y)
                            'type))
                          'wall))
                      ((agenda 'add!)
                       (cons loc (point x y)))))
                 (for-each
                  (lambda (offset)
                    (consider-next (+ (px loc) (px offset))
                                   (+ (py loc) (py offset))))
                  '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
                 (when (equal? ((square 'type)) 'teleport)
                   (let ((next-loc ((square 'get) 'complement)))
                    (consider-next (px next-loc) (py next-loc))))
                 ((square 'set!) 'explored #t)
                 #f))))))

(provide maze-solver)
