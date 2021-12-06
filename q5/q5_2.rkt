#lang racket

(define (check_slope p1 p2)
      (cond
        [(= (- (car p2) (car p1)) 0) 1]
        [(= (- (cadr p2) (cadr p1)) 0) 0]
        [else -1]
        )
  )

(define (string_list->num_list input)
  (if (null? input) '() (append (list (string->number(string-trim (car input)))) (string_list->num_list (cdr input))))
  )

(define (parse_line input)
  (define temp (string-split input "->"))
  (append (list (string_list->num_list (string-split (car temp) ","))) (list (string_list->num_list (string-split (cadr temp) ","))))
  )

(define (parse_list input)
  (if (null? input) '() (append (list (parse_line (car input))) (parse_list (cdr input))))
  )

(define (insert_into x y ht)
  (when (not (hash-ref ht x #f)) (hash-set! ht x (make-hash)))
  (if (not (hash-ref (hash-ref ht x) y #f))
      (hash-set! (hash-ref ht x) y 1)
      (hash-set! (hash-ref ht x) y (+ (hash-ref (hash-ref ht x) y) 1))
      )
  ht)

(define (from-until coords ht)
  (define temp 0)
  (define tempy 0)
  (define eval 0)
  (cond [(= (check_slope (car coords) (cadr coords)) 0) ;X
         (begin
           (set! temp (min (caadr coords) (caar coords)))
           (for ([i (in-range 0 (+ (abs (- (caadr coords) (caar coords))) 1))])
             (set! ht (insert_into (+ temp i) (cadar coords) ht))
             )
           ht)]
        [(= (check_slope (car coords) (cadr coords)) 1) ;Y
         (begin
           (set! temp (min (cadadr coords) (cadar coords)))
           (for ([i (in-range 0 (+ (abs (- (cadadr coords) (cadar coords))) 1))])
             (set! ht (insert_into (caar coords) (+ temp i) ht))
             )
             ht)]
        [else
         (begin ;;45 Degrees -Just comment this section out for Part 1 lol
           (set! eval (/ (- (cadadr coords) (cadar coords)) (- (caadr coords) (caar coords))))
           (cond [(= eval 1) ;;Smallest X, Smallest Y -> Biggest X, Biggest Y
                  (begin
                    (set! temp (min (caar coords) (caadr coords)))
                    (set! tempy (min (cadadr coords) (cadar coords)))
                    )]
                 [(= eval -1) ;;Smallest X, Biggest Y -> Biggest X, Smallest Y
                  (begin
                    (set! temp (min (caar coords) (caadr coords)))
                    (set! tempy (max (cadadr coords) (cadar coords)))
                    )])
           (for ([i (in-range temp (+ (max (caar coords) (caadr coords))1))])
             (set! ht (insert_into i tempy ht))
             (set! tempy (+ tempy eval))
             )
           ht)]
        )
  )

(define (map_out coords ht)
  (if (null? coords) ht (map_out (cdr coords) (from-until (car coords) ht)))
  )

(define (counter ht)
  (define counterval 0)
  (for ([ i (in-range 0 1001)])
    (when (hash-ref ht i #f)
      (for ([ j (in-range 0 1001)])
        (when (hash-ref (hash-ref ht i #f) j #f)
          (when (>= (hash-ref (hash-ref ht i) j) 2)
            (set! counterval (+ counterval 1)))
          )
        )
      )
    )
  counterval)

(define ht (make-hash))
(define instruct (parse_list (file->lines "input.txt")))
(counter (map_out instruct ht))
