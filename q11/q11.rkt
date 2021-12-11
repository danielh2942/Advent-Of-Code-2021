#lang typed/racket
(require math/vector)

;Fuck Arrays, typed racket is so fucking arsey with them
;I want to use vectors, primarily because math/matrix doesn't seem
;to have anything to set a specific position to something arbitrary
;they're disguised as a matrix (kind of) so I don't have to worry about
;the arithmetic for getting x/y positions
(: matrix-set! (-> (Mutable-Vectorof Any) Integer Integer Integer Void)) ;mat is a pointer so this does not need a return type :))))
(define (matrix-set! mat row col val)
  (vector-set! mat (+ (* row 10) col) val))

(: matrix-ref ((Mutable-Vectorof Any) Integer Integer -> Integer))
(define (matrix-ref mat row col)
  ;This basically verifies if the result is a real number
  ;Then converts it to an int (It's very verbose, I know)
  (exact-round (assert (vector-ref mat (+ col (* row 10))) real?)))

(: input_to_num_list (-> (Listof String) (Listof (Listof Integer))))
(define (input_to_num_list input)
  (: input_nums_inner (-> (Listof Char) (Listof Integer)))
  (define (input_nums_inner input)
    (if (null? input)
        '()
        (cons (- (char->integer (car input)) 48) (input_nums_inner (cdr input)))))
  (if (null? input)
      '()
      (cons (input_nums_inner (string->list (car input))) (input_to_num_list (cdr input)))))

(: evaluate_position (-> (Mutable-Vectorof Any) Integer Integer Integer))
(define (evaluate_position mat x y)
  (define ht (make-hash))
  (: eval_inner (-> (Mutable-Vectorof Any) Integer Integer Integer))
  (define (eval_inner mat x y)
    (if (or (hash-ref ht (cons x y) #f) (< x 0) (< y 0) (> x 9) (> y 9))
        0
        (if (= (matrix-ref mat x y) 9)
            (begin
              (matrix-set! mat x y (+ (matrix-ref mat x y) 1))
              (hash-set! ht (cons x y) #t)
              (+ 1
                 (eval_inner mat (+ x 1) y)
                 (eval_inner mat (- x 1) y)
                 (eval_inner mat x (+ y 1))
                 (eval_inner mat x (- y 1))
                 (eval_inner mat (+ x 1) (+ y 1))
                 (eval_inner mat (- x 1) (+ y 1))
                 (eval_inner mat (+ x 1) (- y 1))
                 (eval_inner mat (- x 1) (- y 1))))
            (begin
              (matrix-set! mat x y (+ (matrix-ref mat x y) 1))
              0))))
  (eval_inner mat x y))

(: question1 (-> (Mutable-Vectorof Any) Integer))
(define (question1 mat)
  (define count 0)
  (for ([days 100])
    (for ([x 10])
      (for ([y 10])
        (set! count (+ (evaluate_position mat x y) count))))
    (for ([x 10])
      (for ([y 10])
        (when (> (matrix-ref mat x y) 9)
          (matrix-set! mat x y 0)))))
  count)

(: question2 (-> (Mutable-Vectorof Any) Integer))
(define (question2 mat)
  ;Already done 100 iterations at this stage :))))))
  (define iters 100)
  (: question2_inner (-> (Mutable-Vectorof Any) Integer))
  (define (question2_inner mat)
    (define count 0)
    (set! iters (+ iters 1))
    (for ([x 10])
      (for ([y 10])
        (set! count (+ (evaluate_position mat x y) count))))
    (for ([x 10])
      (for ([y 10])
        (when (> (matrix-ref mat x y) 9)
          (matrix-set! mat x y 0))))
    (if (= count 100)
        iters
        (question2_inner mat)))
  (question2_inner mat))

(define input (list->vector (flatten (input_to_num_list (file->lines "input.txt")))))
(question1 input)
(question2 input)