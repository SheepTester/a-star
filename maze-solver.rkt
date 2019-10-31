#lang racket

; abstract some pair procedures for maximum readability
(define point cons)
(define px car)
(define py cdr)

(define (maze-solver maze agenda)
  ((agenda 'add!) ((maze 'find-square) 'start))

  ; return step procedure; it will return #f if it is not done
  ; or a lambda resolving to #t (reachable) or #f
  ; (too used to JS promises lol)
  (lambda ()
    (if ((agenda 'empty?)) (lambda () #f)
        (let* ((loc ((agenda 'remove!)))
               (square ((maze 'get-square) (px loc) (py loc))))
          (cond ((equal? ((square 'type)) 'explored) #f)
                ((equal? ((square 'type)) 'finish)
                 (lambda () #t))
                (else
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
                       (point (+ (px loc) (px offset))
                              (+ (py loc) (py offset))))))
                  '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
                 ((square 'set!) 'explored)
                 #f))))))

(provide maze-solver)
