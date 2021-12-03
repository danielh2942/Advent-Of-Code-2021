#lang racket
(define (read-in-nums file-path)
  (define nums '())
  (define (next-line-it file)
    (let ((line (read-line file 'any)))
      (unless (eof-object? line)
        (set! nums (append nums (list (string->list line))))
        (next-line-it file))))
  (call-with-input-file file-path next-line-it)
  nums)

(define (comp_val list1 list2)
  (if (null? list1)
      '()
      (begin
        (if (char=? (car list1) #\0)
          (append (list (list (+ (caar list2) 1) (cadar list2))) (comp_val (cdr list1) (cdr list2)))
          (append (list (list (caar list2) (+ (cadar list2) 1))) (comp_val (cdr list1) (cdr list2)))
          )
        )
      )
  )

(define (check_list_common list1)
  (define count (list '(0 0)  '(0 0)  '(0 0)  '(0 0)  '(0 0)  '(0 0)  '(0 0)  '(0 0)  '(0 0)  '(0 0)  '(0 0)  '(0 0)))
  (define (list_it list1 list2)
    (if (null? list1)
        list2
        (begin
          (set! list2 (comp_val (car list1) list2))
          (list_it (cdr list1) list2)
          )
        )
    )
  (set! count (list_it list1 count))
  count
  )

(define (counts_to_binary list1)
  (if (null? list1)
      '()
      (begin
        (if (> (caar list1) (cadar list1))
            (append (list 0) (counts_to_binary (cdr list1)))
            (append (list 1) (counts_to_binary (cdr list1)))
            )
        )
      )
  )

(define (str_from_list l1)
  (if (null? l1)
      ""
      (string-append (string (car l1)) (str_from_list (cdr l1)))
      )
  )

(define (filter_nums list1 seq)
  (if (null? list1)
      '()
      (begin
        (if (string-prefix? (str_from_list (car list1)) seq)
            (append (list (car list1)) (filter_nums (cdr list1) seq))
            (filter_nums (cdr list1) seq)
            )
        )
      )
  )

(define (filter-builder counts mode size)
  (if ( or (null? counts) (= size 0))
             ""
             (begin
               (cond [(= 0 (caar counts)) (string-append "1" (filter-builder (cdr counts) mode (- size 1)))]
                     [(= 0 (cadar counts)) (string-append "0" (filter-builder (cdr counts) mode (- size 1)))]
                     [(= mode 1) (begin ;;Oxygen - Default: 1 Otherwise GT
                                   (cond [(= (caar counts) (cadar counts)) (string-append "1" (filter-builder (cdr counts) mode (- size 1)))]
                                         [(> (caar counts) (cadar counts)) (string-append "0" (filter-builder (cdr counts) mode (- size 1)))]
                                         [else (string-append "1" (filter-builder (cdr counts) mode (- size 1)))]
                                   ))]
                     [(= mode 0) (begin ;;CO2 - Default: 0 Otherwise LT
                                   (cond [(= (caar counts) (cadar counts)) (string-append "0" (filter-builder (cdr counts) mode (- size 1)))]
                                         [(> (caar counts) (cadar counts)) (string-append "1" (filter-builder (cdr counts) mode (- size 1)))]
                                         [else (string-append "0" (filter-builder (cdr counts) mode (- size 1)))]
                                   ))]
                     )
               )
             )
  )

(define (filter-data list1 lwidth mode)
  (define (filter-data-loop list1 lwidth llength mode)
    (if (> llength lwidth)
        list1
        (filter-data-loop (filter_nums list1 (filter-builder (check_list_common list1) mode llength)) lwidth (+ llength 1) mode)
        )
    )
  (define output (filter-data-loop list1 lwidth 1 mode))
  output
  )

;;make a binary string into an int
(define (bin_str_to_decimal bstring)
  (define (bstring_dec_inner lchar count)
    (if (null? lchar)
        count
        (begin
          (if (char=? (car lchar) #\0)
              (bstring_dec_inner (cdr lchar) (arithmetic-shift count 1))
              (bstring_dec_inner (cdr lchar) (+ (arithmetic-shift count 1) 1))
          )
        )
    )
  )
 (bstring_dec_inner (string->list bstring) 0)
  )

(define nums (read-in-nums "input.txt"))
(define counts (check_list_common nums))
(counts_to_binary counts)
(bin_str_to_decimal (str_from_list (car (filter-data nums 12 1))))
(bin_str_to_decimal (str_from_list (car (filter-data nums 12 0))))