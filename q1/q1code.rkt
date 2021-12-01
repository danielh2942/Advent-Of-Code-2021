#lang racket
(define (question1 file-path)
  (define count 0)
  (define current -1) ;;Current is set to -1 as all inputs are positive
  (define (next-line-it file)
    (let ((line (read-line file 'any)))
      (unless (eof-object? line) 
        (define value (string->number line))
        (if (not value)
            count
            (begin
              (when (and (> value current) (> current -1)) ;;I do not need to store the entire list so I do it all on the fly
                (set! count (+ count 1))) ;; Increment count if upper conditions are met value
              (set! current value) ;;set current to the value just read from the file
              )
          )
      (next-line-it file))
      count))
    (call-with-input-file file-path next-line-it)
    count)

(define (read-in-nums file-path)
  (define nums '())
  (define (next-line-it file)
    (let ((line (read-line file 'any)))
      (unless (eof-object? line)
        (set! nums (append nums (list (string->number line))))
        (next-line-it file))))
  (call-with-input-file file-path next-line-it)
  nums)


;;Sliding window calculations
(define (question1b list1 prev_value count)
  (define temp 0)
  (if (or (null? list1) (< (length list1) 3)) ;;Check if empty or undivisible by 3
      count ;;return count
      (begin
        (set! temp (+ (car list1) (+ (cadr list1) (caddr list1)))) ;add three elements and assign to temp
        (when (and (> temp prev_value) (> prev_value -1)) ;;perform two GT checks
          (set! count (+ count 1))) ;;increment count if conditions satisfied
        (question1b (cdr list1) temp count) ;;continue
        )
      )
  )

(question1 "input.txt")
(define nums (read-in-nums "input.txt"))
(question1b nums -1 0)