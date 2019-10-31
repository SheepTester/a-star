#lang racket/gui

(require "./stack.rkt")
(require "./queue.rkt")
(require "./maze.rkt")

; holds default maze
(define maze (make-maze "#####
#o.*#
#####"))

; render maze
(define square-size 20)
(define (render-maze canvas dc)
  ; set canvas size based on maze size
  (send canvas min-width (* ((maze 'width)) square-size))
  (send canvas min-height (* ((maze 'height)) square-size))
  ; set styles
  (send dc set-pen "white" 1 'transparent)
  (for ([y (in-naturals)]
        [row ((maze 'array))])
    (for ([x (in-naturals)]
          [square row])
      (send dc set-brush ((square 'colour)) 'solid)
      (send dc draw-rectangle
            (* x square-size)
            (* y square-size)
            square-size
            square-size))))

; sets maze to the parsed maze data from the given file path
(define (load-from file-name)
  (set! maze
        (make-maze
         (port->string
          (open-input-file file-name #:mode 'text)
          #:close? #t)))
  (send canvas on-paint))

; make new frame
(define frame
  (new frame%
       [label "Maze"]))
 
; make new text field for maze file name
(define text-field
  (new text-field%
       [parent frame]
       [label "Maze data file name"]))

; make load button
(define load-btn
  (new button%
       [parent frame]
       [label "Load"]
       [callback
        (lambda (button event)
          (load-from (send text-field get-value)))]))

; make start (stack) button
(define start-stack-btn
  (new button%
       [parent frame]
       [label "Start (stack)"]
       [callback
        (lambda (button event)
          (display "start (stack)\n"))]))

; make start (queue) button
(define start-queue-btn
  (new button%
       [parent frame]
       [label "Start (queue)"]
       [callback
        (lambda (button event)
          (display "start (queue)\n"))]))

; make step button
(define step-btn
  (new button%
       [parent frame]
       [label "Step"]
       [callback
        (lambda (button event)
          (display "step"))]))

; make toggle animation button
(define toggle-anim-btn
  (new button%
       [parent frame]
       [label "Toggle animation"]
       [callback
        (lambda (button event)
          (display "toggle anim"))]))

; make canvas where the maze will be rendered
(define canvas
  (new canvas%
       [parent frame]
       [paint-callback render-maze]))
 
; show frame
(send frame show #t)