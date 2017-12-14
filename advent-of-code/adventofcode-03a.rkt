#lang racket

; returns floor(side_len / 2) +1
(define (middle side-length)
  (add1 (exact-floor (/ side-length 2))))


; function that counts 0, 1, 2, ... maxval ... maxval -1, ... 1, 0, 1, 2...
; direction: 1 for counting up, -1 for counting down
(define (oscillate to-go maxval direction curval)
  (if (= to-go 1)
      curval
      (let ([new-dir (if (or (= curval 0) (>= curval maxval))
                         (- direction)
                         direction)])
        (oscillate (sub1 to-go) maxval new-dir (+ curval new-dir)))))

; returns the distance from the current memory location to either
; the column or row where 0 is. 
(define (offset-to-distance offset side-length) 
  (oscillate offset (sub1 (middle side-length)) -1 (sub1 (/ (sub1 side-length) 2))))

; how many memory locations are inscribed in the square of side-length
(define (inscribed-locations side-length)
  (* side-length side-length))

; how many locations are inscribed by shell number "shell". Basically converts the
; shell number to side length
(define (inscribed-by-shell shell)
  (inscribed-locations (+ (* shell 2) 1)))

; find which shell a particular memory location is in.
(define (find-shell memloc [cursize 0])
  (if (< (inscribed-by-shell cursize) memloc)
      (find-shell memloc (add1 cursize))
      cursize ))

; calculates how "far in" into a shell a particular memory location is.
; note: offset = 1 for the first memory location in a particular shell.
(define (find-shell-offset memloc shell)
  (- memloc (inscribed-by-shell (sub1 shell))))

(for ([testloc '(8 9 10 15 25 26 1024 277678)])
  (let* ([ml testloc]
         [shell (find-shell ml)]
         [offset (find-shell-offset ml shell)]
         [side-len (add1 (* shell 2))]
         [distance (offset-to-distance offset side-len)]
         [manhattan (+ shell distance)]
         )
    (displayln "=======")
    (printf "ml: ~s ~n" ml)
    (printf "shell: ~s ~n" shell)
    (printf "offset: ~s ~n" offset)
    (printf "side-len: ~s ~n" side-len)
    (printf "distance: ~s ~n" distance)
    (printf "manhattan dist for memloc ~s is ~s ~n" ml manhattan)
  )
)
