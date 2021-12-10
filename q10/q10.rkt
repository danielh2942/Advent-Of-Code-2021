#lang racket

(define (nth index input)
  (if (= index 0)
      (car input)
      (nth (- index 1) (cdr input))))

(define (check_line_for_errors line)
  (define discrim '())
  (define (check_inner line curr)
    (if (null? line)
        0
        (case (char->integer (car curr))
          [(40) ;(
           (case (char->integer (car line))
             [(41) ;)
              (check_inner (cdr line) (cdr curr))]
             [(62) 25137] ;>
             [(93) 57]    ;]
             [(125) 1197] ;}
             [else
              (check_inner (cdr line) (append (list (car line)) curr))])]
          [(60) ;<
           (case (char->integer (car line))
             [(41) 3] ;)
             [(62)
              (check_inner (cdr line) (cdr curr))]
             [(93) 57]    ;]
             [(125) 1197] ;}
             [else
              (check_inner (cdr line) (append (list (car line)) curr))])]
          [(91) ;[
           (case (char->integer (car line))
             [(41) 3] ;)
             [(62) 25137] ;>
             [(93)
              (check_inner (cdr line) (cdr curr))]    ;]
             [(125) 1197] ;}
             [else
              (check_inner (cdr line) (append (list (car line)) curr))])]
          [(123) ;{
           (case (char->integer (car line))
             [(41) 3] ;)
             [(62) 25137] ;>
             [(93)
              57]    ;]
             [(125)
              (check_inner (cdr line) (cdr curr))] ;}
             [else
              (check_inner (cdr line) (append (list (car line)) curr))])])))
  (set! discrim (string->list line))
  (check_inner (cdr discrim) (list (car discrim))))

(define (question_1 input)
  (define (q1_inner input count)
    (if (null? input)
        count
        (q1_inner (cdr input) (+ count (check_line_for_errors (car input))))))
  (q1_inner input 0))

(define (get_incomplete_string input)
  (define discrim '())
  (define (get_incomplete_inner input output)
    (if (null? input)
        output
        (case (char->integer (car output))
          [(40) ;(
           (if (= (char->integer (car input)) 41)
               (get_incomplete_inner (cdr input) (cdr output))
               (get_incomplete_inner (cdr input) (append (list (car input)) output)))]
          [(60)
           (if (= (char->integer (car input)) 62)
               (get_incomplete_inner (cdr input) (cdr output))
               (get_incomplete_inner (cdr input) (append (list (car input)) output)))]
          [(91)
           (if (= (char->integer (car input)) 93)
               (get_incomplete_inner (cdr input) (cdr output))
               (get_incomplete_inner (cdr input) (append (list (car input)) output)))]
          [(123)
           (if (= (char->integer (car input)) 125)
               (get_incomplete_inner (cdr input) (cdr output))
               (get_incomplete_inner (cdr input) (append (list (car input)) output)))])))
  (set! discrim (string->list input))
  (get_incomplete_inner (cdr discrim) (list (car discrim))))
        
(define (make_incomplete_list input)
  (if (null? input)
      '()
      (cons (get_incomplete_string (car input)) (make_incomplete_list (cdr input)))))

(define (filter_out_corruption input)
  (if (null? input)
      '()
      (if (= (check_line_for_errors (car input)) 0)
          (cons (car input) (filter_out_corruption (cdr input)))
          (filter_out_corruption (cdr input)))))

(define (get_score input)
  (define (get_score_inner input count)
    (if (null? input)
        count
        (case (char->integer (car input))
          [(40)
           (get_score_inner (cdr input) (+ (* count 5) 1))]
          [(60)
           (get_score_inner (cdr input) (+ (* count 5) 4))]
          [(91)
           (get_score_inner (cdr input) (+ (* count 5) 2))]
          [(123)
           (get_score_inner (cdr input) (+ (* count 5) 3))]
          [else
           (get_score_inner (cdr input) count)])))
  (get_score_inner input 0))

(define (generate_score_list input)
  (if (null? input)
      '()
      (cons (get_score (car input)) (generate_score_list (cdr input)))))

(define input (file->lines "input.txt"))
(question_1 input)
(set! input (filter_out_corruption input))
(nth 25 (sort (generate_score_list (make_incomplete_list input)) <))