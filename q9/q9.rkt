#lang racket
(require math/matrix)
(require racket/draw)

;(: split_into_nums (-> (Listof String) (Listof (Listof Integer))))
(define (split_into_nums input)
  ;(: split_into_nums_inner (-> (Listof Char) (Listof Integer)))
  (define (split_into_nums_inner input)
    (if (null? input)
        '()
        (cons (- (char->integer (car input)) 48) (split_into_nums_inner (cdr input)))))
  (if (null? input)
      '()
      (cons (split_into_nums_inner (string->list (car input))) (split_into_nums (cdr input)))))

;(: check_neighbours (-> Matrix Integer Integer Integer))
(define (check_neighbours input x y)
  (define ret (matrix-ref input x y))
  (when (not (= y 0))
    (when (>= (matrix-ref input x y) (matrix-ref input x (- y 1))) (set! ret -1))
    (when (not (= x 0))
      (when (>= (matrix-ref input x y) (matrix-ref input (- x 1) (- y 1))) (set! ret -1)))
    (when (not (= x 99))
      (when (>= (matrix-ref input x y) (matrix-ref input (+ x 1) (- y 1))) (set! ret -1))))
  (when (not (= y 99))
    (when (>= (matrix-ref input x y) (matrix-ref input x (+ y 1))) (set! ret -1))
    (when (not (= x 0))
      (when (>= (matrix-ref input x y) (matrix-ref input (- x 1) (+ y 1))) (set! ret -1)))
    (when (not (= x 99))
      (when (>= (matrix-ref input x y) (matrix-ref input (+ x 1) (+ y 1))) (set! ret -1))))
  (when (not (= x 0))
    (when (>= (matrix-ref input x y) (matrix-ref input (- x 1) y)) (set! ret -1)))
  (when (not (= x 99))
    (when (>= (matrix-ref input x y) (matrix-ref input (+ x 1) y)) (set! ret -1)))
  ret)

;(: question1 (-> (Arrayof Any) Integer))
(define (question1 input)
  (define count 0)
  (for ([x 100])
    (for ([y 100])
    (set! count (+ count (check_neighbours input x y) 1))))
      count)

(define (question2_visual input)
  (define bmp1 (make-bitmap 1000 1000))
  (define dc (new bitmap-dc% [bitmap bmp1]))
  (for ([x 100])
    (for ([y 100])
      (case (matrix-ref input x y)
        [(0) (send dc set-brush "navy" 'solid)]
        [(1) (send dc set-brush "blue" 'solid)]
        [(2) (send dc set-brush "teal" 'solid)]
        [(3) (send dc set-brush "sky blue" 'solid)]
        [(4) (send dc set-brush "lightgreen" 'solid)]
        [(5) (send dc set-brush "green" 'solid)]
        [(6) (send dc set-brush "greenyellow" 'solid)]
        [(7) (send dc set-brush "yellow" 'solid)]
        [(8) (send dc set-brush "orange" 'solid)]
        [(9) (send dc set-brush "red" 'solid)])
      #|(case (matrix-ref input x y) ;;I opened paint.net and checked sectors lole
        [(9) (begin
               (send dc set-brush "white" 'solid)
               (send dc set-pen "white" 1 'solid))]
        [else (begin
                (send dc set-brush "black" 'solid)
                (send dc set-pen "black" 1 'solid))])|#
        
      (send dc draw-rectangle (* 10 x) (* 10 y) 10 10)))
    (send bmp1 save-file "output.png" 'png))

(define a (split_into_nums (file->lines "input.txt")))
(define mat (list->matrix 100 100 (flatten a)))
(question1 mat)
(question2_visual mat)