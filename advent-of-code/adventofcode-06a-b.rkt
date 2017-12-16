#lang racket
(require racket/date)


; convert an index value into an offset in a circular list
(define (circular index length)
  (modulo index length))


; find the biggest value in the banks
(define (biggest-bank-value banks)
  (apply max banks))


; find the position of the biggest bank
(define (biggest-bank banks [pos 0])
  (if (= (list-ref banks pos) (biggest-bank-value banks))
      pos
      (biggest-bank banks (add1 pos))))


; get the first n elements of lst
(define (list-head lst n)
  (let ([len (min n (length lst))])
    (if (or (null? lst) (< len 0))
        '()
        (reverse (list-tail (reverse lst) (- (length lst) len))))))


; replace element pos in list lst with value v
(define (replace lst pos v)
  (if (null? lst)
      '()
      (let ([len (min pos (length lst))])
        (if (>= len (length lst))
            lst
            (append (list-head lst pos) (cons v (list-tail lst (add1 len))))))))


; increase the banks to-go times, starting at curpos
(define (add-to-bank banks cur-pos to-go)
  (if (= to-go 0)
      banks
      (let* ([circ-pos (circular cur-pos (length banks))]
             [cur-val (list-ref banks (circular circ-pos (length banks)))])
        (add-to-bank (replace banks circ-pos (add1 cur-val)) (add1 cur-pos) (sub1 to-go)))))


; empty the largest bank and redistribute its memory
(define (move-memory banks)
  (let* ([biggest (biggest-bank banks)]
         [biggest-val (list-ref banks biggest)]
         [emptied-bank (replace banks biggest 0)])
    (add-to-bank emptied-bank (add1 biggest) biggest-val)))


; return true if v exists in lst
(define (exists-in-list v lst)
  (ormap (lambda (x) (equal? x v)) lst))

(define (find-index v lst [curpos 0])
  (if (null? lst)
      -1
      (if (equal? v (car lst))
          curpos
          (find-index v (cdr lst) (add1 curpos)))))


; keep iterating until we've found a new bank that exists in old-combos.
; Note how old-combos defaults to banks!
; returns list of:  distance between the two identical banks in iterations,
;    the absolute number of iterations to find the identical values,
;    the identical bank values
(define (run-program banks [old-combos (list banks)] [cur-it 1])
  (let ([new-bank (move-memory banks)])
    (if (exists-in-list new-bank old-combos)
        (let ([ first-index (find-index new-bank old-combos)])
          (cons (- cur-it first-index) (cons cur-it (list new-bank))))
        (run-program new-bank (append old-combos (list new-bank)) (add1 cur-it) ))))


; Run the program and print out some stats.
(define start-sec (current-seconds))
(printf "start time: ~s~n" (date->string (current-date) #t))

(run-program '(4 1 15 12 0 9 9 5 5 8 7 3 14 5 12 3))

(printf "end time: ~s~n" (date->string (current-date) #t))
(printf "Program ran for ~s seconds ~n" (- (current-seconds) start-sec))