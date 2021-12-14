#lang racket
(define (populate_ht ht inputs)
  (define (populate_inner ht input)
    (define one (string->list (string-trim (car input))))
    (hash-set! ht one (list (car one) (car (string->list (string-trim (cadr input)))) (cadr one))))
  (when (not (null? inputs))
    (populate_inner ht (string-split (car inputs) "->"))
    (populate_ht ht (cdr inputs))))

(define (rip_vals ht)
  (define (rip_inner input)
    (if (null? input)
        '()
        (cons (cdar input) (rip_inner (cdr input)))))
  (rip_inner ht))

(define (count_into_buckets ht input)
  (when (not (null? (cdr input)))
      (begin
        (if (not (hash-ref ht (list (car input) (cadr input)) #f))
            (hash-set! ht (list (car input) (cadr input)) 1)
            (hash-set! ht (list (car input) (cadr input)) (+ (hash-ref ht (list (car input) (cadr input))) 1)))
        (count_into_buckets ht (cdr input)))))

(define (count_q_buckets input)
  (define ht (make-hash))
  (define (count_q_inner input)
    (when (not (null? input))
      ;caar - Key
      ;cdar - count
      ;caaar - Left
      ;cadaar - Right
      (if (not (hash-ref ht (caaar input) #f))
          (hash-set! ht (caaar input) (cdar input))
          (hash-set! ht (caaar input) (+ (hash-ref ht (caaar input)) (cdar input))))
      (if (not (hash-ref ht (cadaar input) #f))
          (hash-set! ht (cadaar input) (cdar input))
          (hash-set! ht (cadaar input) (+ (hash-ref ht (cadaar input)) (cdar input))))
      (count_q_inner (cdr input))))
  (count_q_inner input)
  (sort (divide_all (rip_vals (hash->list ht)) 2) <))

(define (divide_all lst val)
  (if (null? lst)
      '()
      (cons (ceiling (/ (car lst) val)) (divide_all (cdr lst) val))))

(define (question rules input limit)
  (define (q_iterate rules ht input)
    (define keys '())
    (if (null? input)
        ht
        (begin
          (set! keys (hash-ref rules (caar input) #f))
          (if keys
              (begin
                (if (not (hash-ref ht (list (car keys) (cadr keys)) #f))
                    (hash-set! ht (list (car keys) (cadr keys)) (cdar input))
                    (hash-set! ht (list (car keys) (cadr keys)) (+ (hash-ref ht (list (car keys) (cadr keys))) (cdar input))))
                (if (not (hash-ref ht (cdr keys) #f))
                    (hash-set! ht (cdr keys) (cdar input))
                    (hash-set! ht (cdr keys) (+ (hash-ref ht (cdr keys)) (cdar input)))))
              (if (not (hash-ref ht (caar input) #f))
                  (hash-set! ht (caar input) (cdar input))
                  (hash-set! ht (caar input) (+ (hash-ref ht (caar input)) (cdar input)))))
          (q_iterate rules ht (cdr input)))))                  
  (define (q_inner rules input count limit)
    (define ht (make-hash))
    (if (not (= count limit))
        (begin
          (q_iterate rules ht input)
          (q_inner rules (hash->list ht) (+ count 1) limit))
        input))
  (count_q_buckets (q_inner rules input 0 limit)))

(define input (file->lines "input.txt"))
(define ht (make-hash))
(define buckets (make-hash))
(populate_ht ht (cddr input))
(count_into_buckets buckets (string->list (car input)))
(define q1 (question ht (hash->list buckets) 10))
(define q2 (question ht (hash->list buckets) 40))
(display "Part 1: ")
(display (- (apply max q1) (apply min q1)))
(display "\nPart 2: ")
(display (- (apply max q2) (apply min q2)))