#lang racket

(define (list-subset? a b)
  (subset? (list->set a) (list->set b)))

(define (string-length<? a b)
  (< (string-length a) (string-length b)))

(define (nth index search_list)
  (if (= index 0)
      (car search_list)
      (nth (- index 1) (cdr search_list))))

(define (nthcdr index search_list)
  (if (= index 0)
      list
      (nthcdr (- index 1) (cdr search_list))))

(define (xuntily from until search_list)
  (if (null? search_list)
      '()
      (begin
        (if (= from 0)
            (begin
              (if (> until 0)
                  (append (list (car search_list)) (xuntily from (- until 1) (cdr search_list)))
                  (list (car search_list))))
            (xuntily (- from 1) (- until 1) (cdr search_list))))))


(define (get_seven_seg str)
  (case (string-length str)
    [(2) 1]
    [(3) 7]
    [(4) 4]
    [(7) 8]
    [else 0]))

(define (question_1 input)
  (define (question1inner input one four seven eight)
    (if (null? input)
        (list one four seven eight)
        (begin
          (case (get_seven_seg (string-trim (car input)))
            [(1) (set! one (+ one 1))]
            [(4) (set! four (+ four 1))]
            [(7) (set! seven (+ seven 1))]
            [(8) (set! eight (+ eight 1))])
          (question1inner (cdr input) one four seven eight))))
  (question1inner input 0 0 0 0))

(define (get_output inpt)
  (if (null? inpt)
      '()
      (cons (cadr (string-split (car inpt) "|")) (get_output (cdr inpt)))))

(define (get_input_output inpt)
  (if (null? inpt)
      '()
      (cons (string-split (car inpt) "|") (get_input_output (cdr inpt)))))

(define (make_list_out input)
  (if (null? input)
      '()
      (append (string-split (car input)) (make_list_out (cdr input)))))


#|
1 = pos 0
7 = pos 1
4 = pos 2
8 = pos 9
6 = pos'(6,7,8) filtered against 7 and evals to false
0 = pos'(6,7,8) filtered against 7, evals to true, and filtered against 4 and evals to false
9 = pos'(6,7,8) filtered against 7, evals to true, filtered against 4, evals to true
3 = pos'(3,4,5) filtered against 7, evals to true
5 = 9 filtered against pos'(3,4,5), evals to true
2 = 9 filtered against pos'(3,4,5), and false
|#
(define (filter_against str discrim)
  (list-subset? (string->list discrim) (string->list str)))

(define (generate_filter_list inpt)
  (define discrim '(-1 0 -1 -1 2 -1 -1 1 9 -1))
  (define outpt '())
  (if (not (filter_against (nth 6 inpt) (nth 1 inpt)))     ;; pos 6 is 6
    (if (filter_against (nth 7 inpt) (nth 2 inpt))         ;;pos 7 is 9
        (set! discrim (list 8 0 0 0 2 0 6 1 9 7))
        (set! discrim (list 7 0 0 0 2 0 6 1 9 8)))
    (begin
      (if (not (filter_against (nth 7 inpt) (nth 1 inpt))) ;;pos 7 is 6
        (if (filter_against (nth 6 inpt) (nth 2 inpt))
            (set! discrim (list 8 0 0 0 2 0 7 1 9 6))
            (set! discrim (list 6 0 0 0 2 0 7 1 9 8)))
        (if (filter_against (nth 7 inpt) (nth 2 inpt))     ;;pos 8 is 6
            (set! discrim (list 6 0 0 0 2 0 8 1 9 7))
            (set! discrim (list 7 0 0 0 2 0 8 1 9 6))))))
  (if (filter_against (nth 3 inpt) (nth 1 inpt))           ;; pos 3 is 3
    (if (filter_against (nth (nth 9 discrim) inpt) (nth 4 inpt) ) ;;pos 4 is 5
        (set! discrim (append (xuntily 0 1 discrim) (list 5 3 2 4) (xuntily 6 9 discrim)))
        (set! discrim (append (xuntily 0 1 discrim) (list 4 3 2 5) (xuntily 6 9 discrim))))
    (begin
      (if (filter_against (nth 4 inpt) (nth 1 inpt))       ;;pos 4 is 3
          (if (filter_against (nth (nth 9 discrim) inpt) (nth 5 inpt) ) ;;pos 5 is 5
              (set! discrim (append (xuntily 0 1 discrim) (list 3 4 2 5) (xuntily 6 9 discrim)))
              (set! discrim (append (xuntily 0 1 discrim) (list 5 4 2 3) (xuntily 6 9 discrim))))
          (if (filter_against (nth (nth 9 discrim) inpt) (nth 3 inpt)) ;;pos 3 is 5, pos 5 is 3
              (set! discrim (append (xuntily 0 1 discrim) (list 4 5 2 3) (xuntily 6 9 discrim)))
              (set! discrim (append (xuntily 0 1 discrim) (list 3 5 2 4) (xuntily 6 9 discrim)))))))
  (for ([i (in-range 0 10)])
    (set! outpt (append outpt (list (nth (nth i discrim) inpt)))))
  outpt)

(define (q2_get_digit inpt filters)
  (case (string-length inpt)
    [(2) 1]
    [(3) 7]
    [(4) 4]
    [(5) (begin
           (if (filter_against inpt (nth 2 filters))
               2
               (if (filter_against inpt (nth 3 filters)) 3 5)))]
    [(6) (begin
           (if (filter_against inpt (nth 0 filters))
               0
               (if (filter_against inpt (nth 6 filters)) 6 9)))]
    [(7) 8]
    [else 0]))

(define (q2_get_whole_num inpt filters)
  (define (q2_get_whole_inner inpt filters tally)
    (if (null? inpt)
        tally
        (q2_get_whole_inner (cdr inpt) filters (+ (* tally 10) (q2_get_digit (car inpt) filters)))))
  (q2_get_whole_inner inpt filters 0))

(define (question_2 input)
  (define (question2inner input tally)
    (define eval 0)
    (if (null? input)
        tally
        (begin
          (set! eval (q2_get_whole_num (string-split (cadar input)) (generate_filter_list (sort (string-split (caar input)) string-length<?))))
          (question2inner (cdr input) (+ tally eval)))))
  (question2inner input 0))

(define inpt (file->lines "input.txt"))
(question_1 (make_list_out  (get_output inpt)))
(question_2 (get_input_output inpt))
