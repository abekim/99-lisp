#lang racket

;P01 (*) Find the last box of a list
(define (lastBox li)
  (car (reverse li)))

;P02 (*) Find the last but one box of a list.
(define (lastButOne li)
  (cadr (reverse li)))

;P03 (*) Find the K'th element of a list.
(define (nth li n x)
  (if (= x n)
      (car li)
      (nth (cdr li) n (+ x 1))))

;P04 (*) Find the number of elements of a list.
(define (len li x)
  (if (empty? li)
      x
      (len (cdr li) (+ x 1))))

;P05 (*) Reverse a list.
(define (rev li res)
  (if (empty? li)
      res
      (rev (cdr li) (append (list (car li)) res))))

;P06 (*) Find out whether a list is a palindrome.
(define (palindr? li back)
  (cond [(empty? li) #t]
        [(equal? (car li) (car back))
         (palindr? (cdr li) (cdr back))]
        [else #f]))

;P07 (**) Flatten a nested list structure.
(define (flatten li res)
  (cond [(empty? li) res]
        [(list? (car li))
         (flatten (cdr li) (append res (flatten (car li) '())))]
        [else
         (flatten (cdr li) (append res (list (car li))))]))

;P08 (**) Eliminate consecutive duplicates of list elements.
(define (compress li res)
  (cond [(empty? li) res]
        [(or (<= (length li) 1) (not (equal? (car li) (cadr li))))
         (compress (cdr li) (append res (list (car li))))]
        [else (compress (cdr li) res)] ))

;P09 (**) Pack consecutive duplicates of list elements into sublists.
(define (pack li res)
  (cond [(empty? li) res]
        [(or (= (length res) 0) (not (equal? (car li) (caar (reverse res)))))
         (pack (cdr li) (append res (list (list (car li)))))]
        [else 
         (pack (cdr li) (append (reverse (cdr (reverse res))) (list (append (car (reverse res)) (list (car li))))))] ))

;P10 (*) Run-length encoding of a list.
(define (encode li)
  (define packed (pack li '()))
  (true-encode packed '()))

(define (true-encode packed res)
  (cond [(empty? packed) res]
        [else (true-encode (cdr packed) (append res (list (append (list (length (car packed))) (list (caar packed))))))] ))

;P11 (*) Modified run-length encoding.
(define (encode-modified li)
  (define encoded (encode li))
  (true-encode-mod encoded '()))

(define (true-encode-mod encoded res)
  (cond [(empty? encoded) res]
        [(> (caar encoded) 1)
         (true-encode-mod (cdr encoded) (append res (list (car encoded))))]
        [else (true-encode-mod (cdr encoded) (append res (list (cadar encoded))))] ))

;P12 (**) Decode a run-length encoded list.
(define (decode li res)
  (cond [(empty? li) res]
        [(pair? (car li)) (decode (cdr li) (append res (decrypt (caar li) (cadar li) '())))]
        [else (decode (cdr li) (append res (list (car li))))] ))

(define (decrypt n e res)
  (cond [(= n 0) res]
        [else (decrypt (- n 1) e (append res (list e)))]))

;P13 (**) Run-length encoding of a list (direct solution).
(define (encode-direct li res)
  (cond [(empty? li) res]
        [(or (= (length res) 0)
             (and (pair? (car (reverse res))) (not (equal? (car li) (cadar (reverse res)))))
             (and (not (pair? (car (reverse res)))) (not (equal? (car li) (car (reverse res))))))
         (define count (consecutive-count li (car li) 0))
         (cond [(= count 1) (encode-direct (cdr li) (append res (list (car li))))]
               [else (encode-direct (cdr li) (append res (list (append (list (consecutive-count li (car li) 0)) (list (car li))))))] )]
        [else (encode-direct (cdr li) res)] ))

(define (consecutive-count li x res)
  (cond [(or (empty? li) (not (equal? x (car li)))) res]
        [else (consecutive-count (cdr li) x (+ res 1))] ))

;P14 (*) Duplicate the elements of a list.
(define (dupli li res)
  (cond [(empty? li) res]
        [else (dupli (cdr li) (append res (list(car li)) (list (car li))))] ))

;P15 (**) Replicate the elements of a list a given number of times.
(define (repli li n res)
  (cond [(empty? li) res]
        [else (repli (cdr li) n (append res (repeat (car li) n '())))] ))

(define (repeat x n res)
  (cond [(= n 0) res]
        [else (repeat x (- n 1) (append res (list x)))]))

;P16 (**) Drop every N'th element from a list.
(define (drop li n x res)
  (cond [(empty? li) res]
        [(and (> x 0) (= (modulo x n) 0)) (drop (cdr li) n (+ x 1) res)]
        [else (drop (cdr li) n (+ x 1) (append res (list (car li))))]))

;P17 (*) Split a list into two parts; the length of the first part is given.
(define (split li n)
  (append (list (reverse (list-tail (reverse li) (- (length li) n)))) (list (list-tail li n))))

;P18 (**) Extract a slice from a list.
(define (slice li x y)
  (reverse (list-tail (reverse (list-tail li (- x 1))) (- (length li) y))))

;P19 (**) Rotate a list N places to the left.
(define (rotate li n)
  (if (< n 0)
      (append (car (reverse (split li (+ (length li) n)))) (cadr (reverse (split li (+ (length li) n)))))
      (append (car (reverse (split li n))) (cadr (reverse (split li n))))))

;P20 (*) Remove the K'th element from a list.
(define (remove-at li n)
  (define sp (split li (- n 1)))
  (append (car sp) (cdadr sp)))

;P21 (*) Insert an element at a given position into a list.
(define (insert-at x li n)
  (define sp (split li (- n 1)))
  (append (car sp) (list x) (cadr sp)))

;P22 (*) Create a list containing all integers within a given range.
(define (range x y res)
  (cond [(> x y) res]
        [else (range (+ x 1) y (append res (list x)))] ))

;P23 (**) Extract a given number of randomly selected elements from a list.
(define (rnd-select li n res)
  (cond [(= (length res) n) res]
        [else 
         (define ind (random (length li)))
         (rnd-select (remove-at li (+ ind 1)) n (append res (list (list-ref li ind))))] ))

;P24 (*) Lotto: Draw N different random numbers from the set 1..M.
(define (lotto-select x n)
  (rnd-select (range 1 n '()) x '()))

;P25 (*) Generate a random permutation of the elements of a list.
(define (rnd-permu li)
  (rnd-select li (length li) '()))

;P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
(define (combination n li lim res)
  (cond [(= lim 0) res]
        [else 
         (define comb (sorted-rnd-select li n))
         (cond [(not (in? comb res)) (combination n li (- lim 1) (append res (list comb)))]
               [else (combination n li (- lim 1) res)] )] ))

(define (factorial n)
  (fact-iter n 1))

(define (fact-iter n ans)
  (if (= n 1)
      ans
      (fact-iter (- n 1) (* ans n))))

(define (math-comb x n)
  (/ (factorial x) (* (factorial n) (factorial (- x n)))))

(define (sorted-rnd-select li n)
  (cond [(= (length li) n) li]
        [else (sorted-rnd-select (remove-at li (+ (random (length li)) 1)) n)] ))

(define (in? x li)
  (cond [(empty? li) #f]
        [(equal? (car li) x) #t]
        [else (in? x (cdr li))] ))

;P27 (**) Group the elements of a set into disjoint subsets.







