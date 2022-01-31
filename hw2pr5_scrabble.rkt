#lang racket

(require rackunit)
(provide subbag? best-word scrabble-tile-bag)


;; subbag?: determines whether list B contains all the elements in list S
;;          (with a count at least as large)
;; inputs: S and B, both lists
;; outputs: #t if and only if B contains all of the elements in S
;;          (with a count at least as large). Else returns #f
(define (subbag? S B)
  (cond
    [(empty? S) #t]
    [(empty? B) #f]
    [(member (first S) B) (subbag? (rest S) (remove (first S) B))]
    [else #f]))


;;   association list: scrabble-tile-bag  
;;   letter tile scores and counts from the game of Scrabble
;;   the counts are not needed
;;   obtained from http://en.wikipedia.org/wiki/Image:Scrabble_tiles_en.jpg
(define scrabble-tile-bag
  '((#\a 1 9) (#\b 3 2) (#\c 3 2) (#\d 2 4) (#\e 1 12)
   (#\f 4 2) (#\g 2 3) (#\h 4 2) (#\i 1 9) (#\j 8 1)
   (#\k 5 1) (#\l 1 4) (#\m 3 2) (#\n 1 6) (#\o 1 8)
   (#\p 3 2) (#\q 10 1)(#\r 1 6) (#\s 1 4) (#\t 1 6)
   (#\u 1 4) (#\v 4 2) (#\w 4 2) (#\x 8 1) (#\y 4 2)
   (#\z 10 1) (#\_ 0 2)) )

;; WL-to-list converts a list of strings to a list of lists
;; input: a list of strings WL
;; output: a list of lists
(define (WL-to-list WL)
  (if (empty? WL)
      '()
      (append (list(string->list (first WL))) (WL-to-list(rest WL)))))

;; word-possible: determines if it is possible to make a word in WL
;;                out of the letters in rack
;; inputs: list rack (a rack of letters) and list of lists (legal words) WL
;; outputs: #t if 1+ in WL can be made from the letters in rack, else #f
(define (word-possible rack WL)
  (cond
    [(empty? WL) #f]
    [(subbag? (first WL) rack) #t]
    [else (word-possible rack (rest WL))]))

;; remove-bad: removes all words in WL that can't be made using letters in rack
;; input: list rack (a rack of letters) and list of lists (legal words) WL
;; output: a list of all the words in WL that can be made using letters in rack
(define (remove-bad rack WL)
  (cond
    [(empty? WL) '()]
    [(subbag? (first WL) rack) (append (list (first WL))(remove-bad rack (rest WL)))]
    [else (remove-bad rack (rest WL))]))


;; enumerate-helper: enumerates the items of a list L  
;; inputs: ind, the index that enumerating starts on 
;;         L, the list to enumerate 
;; outputs: list identical to L, but every element is now a pair (index elem);
;;          index is the element's index (plus the value of ind in the original
;;          function call - 0 in our case) and elem is the original element
(define (enumerate-helper ind L)
  (if (empty? L)
      '()
      (cons (list ind (first L))
            (enumerate-helper (+ 1 ind) (rest L)))))

;; enumerates the items of a list L using 0-indexed indices
;; relies on recursive helper function enumerate-helper
;; inputs: a list L, the list to enumerate
;; outputs: list identical to L, but every element is now a pair (index elem);
;;          index is the element's index and elem is the original element
(define (enumerate L)
  (enumerate-helper 0 L))

;; finds the scrabble letter score of a letter
;; inputs: a symbol, called letter, to be scored
;; outputs: the scrabble score of the letter
(define (score-letter letter)
  (first(rest(assoc letter scrabble-tile-bag)))
  )

;; finds the scrabble word score of a word
;; inputs: a list of letters, word, to be scored
;; outputs: the scrabble score of the word 
(define (score-word word)
  (if
   (empty? word)
   0
   (+ (score-letter (first word)) (score-word (rest word)))))


;; determines the scrabble word score of every item in a list
;; inputs: a list L to be scored
;; outputs: a list with all the scores of the words in the list
(define (score-list L)
  (if
   (empty? L)
   '()
   (append (list(score-word(first L))) (score-list(rest L)))))

;; reverses all the top-level lists in a list L
;; inputs: the list to reverse L, where each element in L is also a list
;; output: a list identical to L, but with all top-level lists reversed
(define (superreverse L)
  (if (empty? L)
      '()
      (append (list(reverse (first L))) (superreverse (rest L)))))

;; finds whether x or y is larger
;; inputs: two integers x and y to compare
;; outputs: x if x is larger, else y
(define (max-element x y) (if (> x y) x y))

;; finds the max element in a list
;; input: a list L to parse
;; outputs: the maximum value in a list L
(define (max-list L)
    (if (null? (cdr L))
        (car L)
        (max-element (car L) (max-list (cdr L)))))

;; determines the index of the word with the max scrabble score
;; inputs: the maximum score of a word in the list
;;         a list of scores and their indices L
;; output: the index of the word with the max scrabble score in L
(define (find-index maximum L)
  (first(rest(assoc maximum (superreverse L))))
  )

;; finds the word at index L
;; input: index-value, an integer that is the index of our desired word
;;        L, a list to index
;; output: the value of list L at index index-value
(define (find-word index-value L)
  (first(rest(assoc index-value L)))
  )
  

;; best-word: finds the best word in WL that the letters in rack can make
;; input: string rack (a rack of letters) and list of strings (legal words) WL
;; outputs: a two-element list, where the first element is the best word in WL
;;          that can be made from the letters in rack, and the second element
;;          is that word's score
(define (best-word rack WL)
  (define list-rack (string->list rack))
  (define list-WL (WL-to-list WL))
  (if
   (word-possible list-rack list-WL)
   (let*([usable-words (remove-bad list-rack list-WL)])
     
     (let*([word-numbered (enumerate usable-words)])
       (let*([list-of-scores (score-list usable-words)])
         (let*([score-numbered (enumerate list-of-scores)])
           (let* ([max-score (max-list list-of-scores)])
             (let* ([max-index (find-index max-score score-numbered)])
               (let* ([max-word (find-word max-index word-numbered)])
                 (cons (list->string max-word) (list max-score)))))))))
   '("" 0)))


;; tests for helper functions

(best-word "appler"  '("peal" "peel" "ape" "paper"))

(check-equal?(WL-to-list '("ace" "ade" "cad" "cay")) 
            '((#\a #\c #\e) (#\a #\d #\e) (#\c #\a #\d) (#\c #\a #\y)))

(check-true
 (word-possible '(#\c #\a #\e)
                '((#\a #\c #\e) (#\a #\d #\e) (#\c #\a #\d) (#\c #\a #\y))))

(check-false
 (word-possible '(#\e #\a #\e)
                '((#\a #\c #\e) (#\a #\d #\e) (#\c #\a #\d) (#\c #\a #\y))))

(check-equal?
 (remove-bad '(#\c #\a #\e)
                '((#\a #\c #\e) (#\a #\e #\c) (#\c #\a #\d) (#\c #\a #\e)))
                '((#\a #\c #\e) (#\a #\e #\c) (#\c #\a #\e)))

(check-equal? (score-letter #\f) 4)

(check-equal?
 (score-word '(#\c #\a #\e)) 5)

(check-equal?
 (score-list '((#\a #\c #\e) (#\a #\e #\c) (#\c #\a #\d) (#\c #\a #\e))) '(5 5 6 5))

(check-equal?
 (find-index 12 '((0 9) (1 12) (2 7))) 1)


