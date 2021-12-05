#lang racket

(define (check_slope p1 p2)
      (cond
        [(= (- (car p2) (car p1)) 0) 1]
        [(= (- (cadr p2) (cadr p1)) 0) 0]
        [else -1]
        )
  )

(define (get_smaller a b)
  (if (< a b)
      a
      b
      )
  )

(define (get_bigger a b)
  (if (< a b)
      b
      a
      )
  )

(define (string_list->num_list input)
  (if (null? input)
      '()
       (append (list (string->number(string-trim (car input)))) (string_list->num_list (cdr input)))
      )
  )

(define (parse_line input)
  (define temp (string-split input "->"))
  (append (list (string_list->num_list (string-split (car temp) ","))) (list (string_list->num_list (string-split (cadr temp) ","))))
  )

(define (parse_list input)
  (if (null? input)
      '()
      (append (list (parse_line (car input))) (parse_list (cdr input)))
      )
  )

(define (from-until coords ht)
  (define temp 0)
  (define tempy 0)
  (define eval 0)
  (cond [(= (check_slope (car coords) (cadr coords)) 0) ;X
         (begin
           (set! temp (get_smaller (caadr coords) (caar coords)))
           (for ([i (in-range 0 (+ (abs (- (caadr coords) (caar coords))) 1))])
             (when (not (hash-ref ht (+ temp i) #f))
               (hash-set! ht (+ temp i) (make-hash))
               )
             (if (not (hash-ref (hash-ref ht (+ temp i)) (cadar coords) #f))
                 (hash-set! (hash-ref ht (+ temp i)) (cadar coords) 1)
                 (hash-set! (hash-ref ht (+ temp i)) (cadar coords) (+ (hash-ref (hash-ref ht (+ temp i)) (cadar coords)) 1))
                 )
             )
           ht)]
        [(= (check_slope (car coords) (cadr coords)) 1) ;Y
         (begin
           (set! temp (get_smaller (cadadr coords) (cadar coords)))
           (for ([i (in-range 0 (+ (abs (- (cadadr coords) (cadar coords))) 1))])
             (if (not (hash-ref ht (caar coords) #f))
               (hash-set! ht (caar coords) (make-hash))
               (display "")
               )
             (if (not (hash-ref (hash-ref ht (caar coords)) (+ temp i) #f))
                 (hash-set! (hash-ref ht (caar coords)) (+ temp i) 1)
                 (hash-set! (hash-ref ht (caar coords)) (+ temp i) (+ (hash-ref (hash-ref ht (caar coords)) (+ temp i)) 1))
                 )
             )
             ht)]
        [else
         (begin ;;45 Degrees -Just comment this section out for Part 1 lol
           (set! eval (/ (- (cadadr coords) (cadar coords)) (- (caadr coords) (caar coords))))
           (cond [(= eval 1) ;;Smallest X, Smallest Y -> Biggest X, Biggest Y
                  (begin
                    (set! temp (get_smaller (caar coords) (caadr coords)))
                    (set! tempy (get_smaller (cadadr coords) (cadar coords)))
                    )]
                 [(= eval -1) ;;Smallest X, Biggest Y -> Biggest X, Smallest Y
                  (begin
                    (set! temp (get_smaller (caar coords) (caadr coords)))
                    (set! tempy (get_bigger (cadadr coords) (cadar coords)))
                    )])
           (for ([i (in-range temp (+ (get_bigger (caar coords) (caadr coords))1))])
             (when (not (hash-ref ht i #f))
                 (hash-set! ht i (make-hash)))
             (if (not (hash-ref (hash-ref ht i) tempy #f))
                 (hash-set! (hash-ref ht i) tempy 1)
                 (hash-set! (hash-ref ht i) tempy (+ (hash-ref (hash-ref ht i) tempy) 1))
                 )
             (set! tempy (+ tempy eval))
             )
           ht)]
        )
  )

(define (map_out coords ht)
  (if (null? coords)
      ht
      (map_out (cdr coords) (from-until (car coords) ht))
      )
  )

(define (counter ht)
  (define counterval 0)
  (for ([ i (in-range 0 1001)])
    (when (hash-ref ht i #f)
      (for ([ j (in-range 0 1001)])
        (when (hash-ref (hash-ref ht i #f) j #f)
          (when (>= (hash-ref (hash-ref ht i) j) 2)
            (set! counterval (+ counterval 1))
            )
          )
        )
      )
    )
  counterval
  )

(define ht (make-hash))
(define instruct (parse_list (file->lines "input.txt")))
(counter (map_out instruct ht))