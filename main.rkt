#lang racket/gui

(require "./stack.rkt")
(require "./queue.rkt")
(require "./maze.rkt")
(require "./maze-solver.rkt")
(require "./point.rkt")

; holds maze data (so it can restart without reloading)
(define maze-data "#####
#o.*#
#####")

; holds default maze
(define maze (make-maze maze-data))

; stepping procedure (if available)
(define step #f)

; name of solution type
(define solution-mode "")

; traceable path and its maze if found
(define path #f)

; render maze
(define square-size 20)
(define dot-diameter 6)
(define (render-maze canvas dc)
  ; set canvas size based on maze size
  (send canvas min-width (* ((maze 'width)) square-size))
  (send canvas min-height (* ((maze 'height)) square-size))
  ; set styles
  (send dc clear)
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
            square-size)))
  (send dc set-brush (make-color 240 227 190) 'solid)
  (define (iter prev-square)
    (when prev-square
      (let ((square
             ((maze 'get-square) (px prev-square) (py prev-square))))
        (send dc draw-ellipse
              (+ (* (px prev-square) square-size)
                 (/ (- square-size dot-diameter) 2))
              (+ (* (py prev-square) square-size)
                 (/ (- square-size dot-diameter) 2))
              dot-diameter
              dot-diameter)
        (iter ((square 'get) 'previous)))))
  (when (and path (equal? (cdr path) maze))
    (iter (car path))))

; sets maze to the parsed maze data from the given file path
(define (load-from file-name)
  (set! maze-data
        (port->string
         (open-input-file file-name #:mode 'text)
         #:close? #t))
  (set! maze (make-maze maze-data))
  (send status set-label "Loaded maze.")
  (send canvas on-paint))

; when step button is called
(define (run-step)
  (when step
    (let ((result (step)))
      (send status set-label
            (if result
                (let ((traceable-path (result)))
                  (set! step #f)
                  (set! path (cons traceable-path maze))
                  ; NOTE: make sure this is the last statement
                  ; here so it returns a string
                  (if traceable-path
                      "Solution complete: finish reachable"
                      "Solution complete: finish not reachable"))
                (string-append solution-mode " in progress")))
      (send canvas on-paint))))

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
          (set! maze (make-maze maze-data))
          (set! step (maze-solver maze (make-stack)))
          (set! solution-mode "Stack-based solution")
          (run-step))]))

; make start (queue) button
(define start-queue-btn
  (new button%
       [parent frame]
       [label "Start (queue)"]
       [callback
        (lambda (button event)
          (set! maze (make-maze maze-data))
          (set! step (maze-solver maze (make-queue)))
          (set! solution-mode "Queue-based solution")
          (run-step))]))

; make step button
(define step-btn
  (new button%
       [parent frame]
       [label "Step"]
       [callback (lambda (button event) (run-step))]))

; initialize current timer
(define timer #f)

; make toggle animation button
(define toggle-anim-btn
  (new button%
       [parent frame]
       [label "Enable animation"]
       [callback
        (lambda (button event)
          (send toggle-anim-btn set-label
                (if timer "Enable animation" "Disable animation"))
          (if timer
              (begin
                (send timer stop)
                (set! timer #f))
              (set! timer (new timer%
                               [notify-callback run-step]
                               [interval 50]
                               [just-once? #f]))))]))

; make status text
(define status
  (new message%
       [parent frame]
       [label "Default maze"]
       [auto-resize #t]))

; make canvas where the maze will be rendered
(define canvas
  (new canvas%
       [parent frame]
       [paint-callback render-maze]))
 
; show frame
(send frame show #t)