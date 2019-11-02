#lang racket

; needed to access make-color
(require racket/draw)

; makes a square (either #, ., o, or *)
(define (make-square char)
  ; the type of square
  (define type
    (cond ((equal? char #\#) 'wall)
          ((equal? char #\.) 'space)
          ((equal? char #\o) 'start)
          ((equal? char #\*) 'finish)
          ((equal? char #\x) 'explored)
          ((equal? char #\@) 'teleport)
          (else (error "Invalid character"))))

  ; returns the type of square
  (define (get-type) type)

  ; sets the type
  (define (set-to! new-type)
    (set! type new-type))

  ; type of square back to char
  (define (square->char)
    (cond ((equal? type 'wall) #\#)
          ((equal? type 'space) #\.)
          ((equal? type 'start) #\o)
          ((equal? type 'finish) #\*)
          ((equal? type 'explored) #\x)
          ((equal? type 'teleport) #\@)
          (else (error (string-append (string char)
                                      " doesn't represent a square.")))))

  ; return tile colour as according to type
  (define (colour)
    (cond ((equal? type 'wall) (make-color 40 40 40))
          ((equal? type 'space) (make-color 238 238 238))
          ((equal? type 'start) (make-color 2 253 182))
          ((equal? type 'finish) (make-color 253 72 2))
          ((equal? type 'explored) (make-color 127 127 127))
          ((equal? type 'teleport) (make-color 111 91 198))
          (else (error (string-append (symbol->string type)
                                      " doesn't have a colour")))))
  
  ; reference to previous square LOCATION in closest path
  (define previous-square #f)

  ; number of squares in closest path to this square
  (define dist-to-me +inf.0)

  ; estimated "goodness" to finish
  (define goodness +inf.0)

  ; get/set miscellaneous attributes (implementation as such for I am lazy)
  (define (get-var name)
    (cond ((equal? name 'previous) previous-square)
          ((equal? name 'dist) dist-to-me)
          ((equal? name 'goodness) goodness)
          (else (error (string-append "I don't have a "
                                      (symbol->string name))))))
  (define (set-var! name value)
    (cond ((equal? name 'previous) (set! previous-square value))
          ((equal? name 'dist) (set! dist-to-me value))
          ((equal? name 'goodness) (set! goodness value))
          (else (error (string-append "I can't set a nonexistent "
                                      (symbol->string name))))))

  ; estimates closeness of this square based on how close
  ; the path through this square would be
  (define (closeness) (+ dist-to-me goodness))

  ; allow methods to be accessed using (square-instance 'method-name)
  (define (dispatch method)
    (cond ((equal? method 'type) get-type)
          ((equal? method 'set-type!) set-to!)
          ((equal? method 'square->char) square->char)
          ((equal? method 'colour) colour)
          ((equal? method 'get) get-var)
          ((equal? method 'set!) set-var!)
          ((equal? method 'closeness) closeness)
          (else (error (string-append "Method "
                                      (symbol->string method)
                                      " doesn't exist")))))
  dispatch)

(provide make-square)
