#lang racket
(define (file-reader file-path)
  (define output '())
  (define (next-line-it file)
    (let ((line (read-line file 'any)))
      (unless (eof-object? line)
        (set! output (append output (list (string-split line)) ))
        (next-line-it file))))
  (call-with-input-file file-path next-line-it)
  output)

(define (get-coord-tr list1 x y)
  (if (null? list1)
      (* x y)
    (begin
      (cond
      [(string=? (caar list1) "forward") (set! x (+ x (string->number (cadar list1))))]
      [(string=? (caar list1) "down") (set! y (+ y (string->number (cadar list1))))]
      [(string=? (caar list1) "up") (set! y (- y (string->number (cadar list1))))]
    )
    (get-coord-tr (cdr list1) x y))
  )
  )

;;Slightly modified version of the answer for q2 part a
(define (get-actual-coord-tr list1 x y aim)
  (if (null? list1)
      (* x y)
    (begin
      (cond
      [(string=? (caar list1) "forward") (begin
                                           (set! x (+ x (string->number (cadar list1))))
                                           (set! y (+ (* aim (string->number (cadar list1))) y))
                                           )]
      [(string=? (caar list1) "down") (set! aim (+ aim (string->number (cadar list1))))]
      [(string=? (caar list1) "up") (set! aim (- aim (string->number (cadar list1))))]
    )
    (get-actual-coord-tr (cdr list1) x y aim))
  )
  )

(define input (file-reader "input.txt"))
(get-coord-tr input 0 0 )
(get-actual-coord-tr input 0 0 0)