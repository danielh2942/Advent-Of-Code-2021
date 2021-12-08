#lang racket
(define discrim '(50 68 2 1 69 32 87 10 31 21 78 23 62 98 16 99 65 35 27 96 66 26 74 72 45 52 81 60 38 57 54 19 18 77 71 29 51 41 22 6 58 5 42 92 85 64 94 12 83 11 17 14 37 36 59 33 0 93 34 70 97 7 76 20 3 88 43 47 8 79 80 63 9 25 56 75 15 4 82 67 39 30 89 86 46 90 48 73 91 55 95 28 49 61 44 84 40 53 1324))
(define (file-reader file-path)
  (define output '())
  (define (next-line-it file)
    (let ((line (read-line file 'any)))
      (unless (eof-object? line)
        (set! output (append output (list line)))
        (next-line-it file))))
  (call-with-input-file file-path next-line-it)
    output)

(define (nth index search_list)
  (if (null? search_list)
      '()
      (begin
        (if (= index 0)
            (car search_list)
            (nth (- index 1) (cdr search_list))))))

(define (nthcdr index search_list)
  (if (null? search_list)
      '()
      (begin
        (if (= index 0)
            search_list
            (nthcdr (- index 1) (cdr search_list))))))

(define (nthcol index search_list)
  (if (null? search_list)
      '()
      (append (list (nth index (car search_list))) (nthcol index (cdr search_list)))))

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

(define (numlisttoints input)
  (if (null? input)
      '()
      (append (list (string->number (car input))) (numlisttoints (cdr input)))))

(define (numliststoints input)
  (if (null? input)
      '()
      (begin
        (if (pair? input)
            (append (list (numlisttoints (string-split (car input)))) (numliststoints (cdr input)))
            (list (numlisttoints (string-split input)))))))

(define (generate_bingo_card input)
  (if (null? input)
      '()
      (append (list (numliststoints (xuntily 1 5 input))) (generate_bingo_card (nthcdr 6 input)))))

(define (contains_sublist sublist discrim)
  (define (contains_sublist_inner sublist discrim count)
    (if (null? sublist)
        count
        (begin
          (if (pair? (member (car sublist) discrim)) (contains_sublist_inner (cdr sublist) discrim (+ count 1)) 0))))
  (contains_sublist_inner sublist discrim 0))

(define (check_for_bingo card nums_called)
  (define (check_for_bingo_inner card nums_called index)
    (if (= index 5)
        '()
        (begin
          (if (or (= (contains_sublist (nth index card) nums_called) 5) (= (contains_sublist (nthcol index card) nums_called) 5))
              card
              (check_for_bingo_inner card nums_called (+ index 1))))))
  (check_for_bingo_inner card nums_called 0))

(define (check_for_winners cards nums_called)
  (if (null? cards)
      '()
      (begin
        (if (not (null? (check_for_bingo (car cards) nums_called)))
            (car cards)
            (check_for_winners (cdr cards) nums_called)))))

(define (remove_elems input remove)
  (define (remove_elems_inner input remove)
    (if (or (null? input) (null? remove))
      input
      (begin
        (if (member (car input) remove)
            (remove_elems_inner (cdr input) remove)
            (append (list (car input)) (remove_elems_inner (cdr input) remove))))))
  (remove_elems_inner (sort (flatten input) <) (sort remove <)))

(define (sum_list L)
  (if (null? L)
      0
      (+ (car L) (sum_list (cdr L)))))

(define (bingo_loop cards discrim)
  (define (bingo_loop_inner cards discrim amount_called)
    (define winner (check_for_winners cards (xuntily 0 amount_called discrim)))
    (if (not (null? winner))
        (begin
           (display "\n")
           (display (xuntily 0 amount_called discrim))
           (display "\n")
           (display (* (nth amount_called discrim) (sum_list (remove_elems (flatten winner) (xuntily 0 amount_called discrim)))))
           (display "\n"))
        (bingo_loop_inner cards discrim (+ amount_called 1))))
  (bingo_loop_inner cards discrim 4))

(define (bingo_loop_last cards discrim)
  (define (bingo_loop_last_inner cards discrim amount_called)
    (define winner (check_for_winners cards (xuntily 0 amount_called discrim)))
    (if (not (null? winner))
        (begin
          (if (= amount_called 80)
              (begin
                (display "\n")
                (display (xuntily 0 amount_called discrim))
                (display "\n")
                (display (* (nth amount_called discrim) (sum_list (remove_elems (flatten winner) (xuntily 0 amount_called discrim)))))
                (display "\n")
                amount_called)
              amount_called))
        (bingo_loop_last_inner cards discrim (+ amount_called 1))))
  (bingo_loop_last_inner cards discrim 4))

(define (get_last_winner cards discrim highest)
  (define temp 0)
  (if (null? cards)
      highest
      (begin
        (set! temp (bingo_loop_last (list (car cards)) discrim))
        (if (and (not (void? temp)) (> temp highest))
            (get_last_winner (cdr cards) discrim temp)
            (get_last_winner (cdr cards) discrim highest)))))

(define bingo_text (file-reader "input.txt"))
(define bingo_cards (generate_bingo_card bingo_text))
(bingo_loop bingo_cards discrim)
(get_last_winner bingo_cards discrim 0)
