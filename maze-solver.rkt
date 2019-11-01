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
          (cond ((equal? ((square 'type)) 'explored) #f)
                ((equal? ((square 'type)) 'finish)
                 (lambda () (car entry)))
                (else
                 ((square 'set!) 'previous (car entry))
                 (for-each
                  (lambda (offset)
                    (when
                        (not
                         (equal?
                          ((((maze 'get-square)
                             (+ (px loc) (px offset))
                             (+ (py loc) (py offset)))
                            'type))
                          'wall))
                      ((agenda 'add!)
                       (cons loc
                             (point (+ (px loc) (px offset))
                                    (+ (py loc) (py offset)))))))
                  '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
                 ((square 'set-type!) 'explored)
                 #f))))))

(provide maze-solver)
