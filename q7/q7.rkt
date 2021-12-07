#lang racket
(define (string_list->num_list input)
  (if (null? input) '() (cons (string->number(string-trim (car input))) (string_list->num_list (cdr input)))))

(define (calculate_total_cost nums location hof)
  (define (calculate_total_cost_loop nums location tally)
    (if (null? nums)
        tally
        (calculate_total_cost_loop (cdr nums) location (+ (hof (abs (- (car nums) location))) tally))))
  (calculate_total_cost_loop nums location 0))

(define (get_total_costs nums locations hof cheapest)
  (if (null? locations)
      cheapest
      (begin
        (set! cheapest (min (calculate_total_cost nums (car locations) hof) cheapest))
        (get_total_costs nums (cdr locations) hof cheapest))))

(define (sum_until x)
  (/ (* x (+ 1 x)) 2))

(define (foo x)
  x)

(define data (string_list->num_list (string-split (car (file->lines "input.txt")) ",")))
(define range (sequence->list (in-inclusive-range 0 3000)))
(get_total_costs data range foo 999999999999999999)
(get_total_costs data range sum_until 9999999999999999)