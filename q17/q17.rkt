#lang racket

(define (question_1 top_left)
  (define (q1_inner i h)
    (if (< i (- (* (cdr top_left) -1) 1))
      (q1_inner (+ i 1) (+ h (+ i 1)))
      h))
  (q1_inner 0 0))

(define (question_2 top_left bottom_right)
  (define (verify_velocity velo_x velo_y)
    (define (verify_velo_inner x y velo_x velo_y)
      (if (and
             (and (>= x (car top_left)) (<= x (car bottom_right)))
             (and (>= y (cdr top_left)) (<= y (cdr bottom_right))))
        #t
        (if (or (> x (car bottom_right)) (< y (cdr top_left)))
            #f
            (verify_velo_inner
             (+ x velo_x) (+ y velo_y)
             (if (> velo_x 0)
                 (- velo_x 1)
                 (if (= velo_x 0)
                     0
                     (+ velo_x 1)))
             (- velo_y 1)))))
    (verify_velo_inner 0 0 velo_x velo_y))
  (define valid_velocities 0)
  (for ([i 1000])
    (for ([j (in-range -1000 1000)])
      (when (verify_velocity i j)
        (set! valid_velocities (+ valid_velocities 1)))))
  valid_velocities)

(define top_left (cons 257 -101))
(define bottom_right (cons 286 -57))
;(define top_left (cons 20 -10))
;(define bottom_right (cons 30 -5))
(question_1 top_left)
(question_2 top_left bottom_right)