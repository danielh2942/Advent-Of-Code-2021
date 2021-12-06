#lang racket
(define (string_list->num_list input)
  (if (null? input) '() (cons (string->number(string-trim (car input))) (string_list->num_list (cdr input)))))

(define (count_into_ht input ht)
  (if (null? input)
      ht
      (begin
        (if (not (hash-ref ht (car input) #f))
            (hash-set! ht (car input) 1)
            (hash-set! ht (car input) (+ (hash-ref ht (car input)) 1)))
        (count_into_ht (cdr input) ht))))

(define (get_next_day_hash ht)
  (define new_ht (make-hash))
  (for ([i (in-range 0 9)])
    (when (hash-ref ht i #f)
      (if (= i 0)
          (begin
            (hash-set! new_ht 6 (hash-ref ht i))
            (hash-set! new_ht 8 (hash-ref ht i)))
          (begin
            (if (not (hash-ref new_ht (- i 1) #f))
                (hash-set! new_ht (- i 1) (hash-ref ht i))
                (hash-set! new_ht (- i 1) (+ (hash-ref ht i) (hash-ref new_ht (- i 1)))))))))
  new_ht)

(define (increment_until_hash ht days)
  (define (increment_until_hash_inner ht current days)
    (if (= current days)
        ht
        (increment_until_hash_inner (get_next_day_hash ht) (+ current 1) days)))
  (increment_until_hash_inner ht 0 days))

(define (count_total ht)
  (define tally 0)
  (for ([i (in-range 0 9)])
    (when (hash-ref ht i #f)
      (set! tally (+ tally (hash-ref ht i)))))
  tally)

(define data (string_list->num_list (string-split (car (file->lines "input.txt")) ",")))
(set! data (sort data <))
(define ht (make-hash))
(count_into_ht data ht)
(count_total (increment_until_hash ht 80))
(count_total (increment_until_hash ht 256))
