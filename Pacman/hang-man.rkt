;; hang-man for REPL Scheme


;;
(define source-name "glossary.txt")

;; Side effect:
;; Strig-> IO([String])
;; Passed the path, open the file containig glossary
(define (read-words-from filename)
  (let* ((port (open-input-file filename))
         (res (read-word-list port '())))
    (close-input-port port)
    res))

;; Side effect
;; Fd -> [String] -> IO ([String])
;; Passed port and acumulator, return the all the words as strings
(define (read-word-list port acc)
  (let ((stuff (read port)))
    (if (eof-object? stuff)
        acc
        (read-word-list port
                        (cons (symbol->string stuff) acc)))))

(define list-of-words (read-words-from source-name))

;; STATE OF THE GAME
(define word-to-guess null)
(define partial-sol null)

(define hits 0)
(define plays 0)
(define failures 0)
(define total-failures 6)
(define total-hits (length word-to-guess))
(define glossary (map string->list list-of-words))


;; 
;; IO(String)
(define (game-status)
  (begin
    (format "~a H:~a/~a F:~a/~a ~a ~a"
            (list->string partial-sol)
            hits  total-hits
            failures  total-failures
            plays
            (if (and
                 (< hits total-hits)
                 (< failures total-failures))
                ""
                (string-append "GAME-OVER(" (list->string word-to-guess) ")")))))


;;;
;;  PURELY FUNCTIONAL
;;

;; [Char] -> Char -> IO(Int)
;; Counts how many chars are in word.
(define (occurrences word char)
  (define (sum-if-char other-char total)
    (cond ((equal? char other-char) (+ total 1))
          (else total)))
  
  (foldl sum-if-char 0 word))

;; Int -> IO([Int])
;; Returns a list that goes from 0 to n - 1.
(define (list-of-indices n)
  (define (recur n)
    (cond ((equal? n 0) '())
          (else (cons (- n 1) (recur (- n 1))))))
  (reverse (recur n)))

;; [Char] -> Char -> IO([Int])
;; Returns a list of indices that correspond to each position
;; of char in word.
(define (indices word char)

  ; current-char is given by the list word
  ; and index by the list resulting from (list-of-indices (length word))
  (define (accumulate-index-if-char current-char index accumulator)
    (cond ((equal? current-char char)
           (cons index accumulator))
          (else accumulator)))

  ; Reverse for correct order.
  (reverse (foldl accumulate-index-if-char '() word (list-of-indices (length word)))))


;; [Char] -> [Int] -> Char -> IO([Char])
;; Replaces each position given by idx in word by the character new
;; and returns the resulting list.
(define (replace-indices word idx new)
  (map (lambda (char index)
         ; If index is in idx, in the new list that position will be
         ; filled by new.
         (cond ((member index idx) new) 
               (else char)))
       word (list-of-indices (length word))))

;; [Char] -> IO(Int)
;; Counts the number of hits in hidden.
(define (noOfHits hidden)
  ; The number of hits, is the number of non-* characters
  ; in a word. So, the length of hidden minus the count of *.
  (- (length hidden) (occurrences hidden #\*)))


;; Side effects
;; IO(String)
(define (restart)
  ;; IO([Char])
  (define (new-word-to-guess)
    (list-ref glossary (random (length glossary))))

  ;; [Char] -> IO([Char])
  (define (make-partial-solution-from word)
    (map (lambda (x) #\*) (list-of-indices (length word))))

  (begin
    (set! word-to-guess (new-word-to-guess))
    (set! partial-sol (make-partial-solution-from word-to-guess))

    (set! plays 0)
    
    (set! hits 0)
    (set! total-hits (length word-to-guess))
    
    (set! failures 0)
    (set! total-failures 6)
    (game-status)))


;; Char -> IO(String)
;; If the game is not over, check for every character in word-to-guess
;; if they are equal to char, if so, add to the total of hits.
;; If no hits are found add to failures.
;; Increment plays.
;; Regardless, at the end, return game-status.
(define (guess char)
  (cond ((not (or (>= failures total-failures) (>= hits total-hits)))
         (define indices-of-char (indices word-to-guess char))
         (begin
           (set! partial-sol (replace-indices partial-sol indices-of-char char))
           (set! hits (noOfHits partial-sol))
           
           (set! plays (+ plays 1))
           (cond ((empty? indices-of-char)
                  (begin
                    (set! failures (+ failures 1))))))))
  (game-status))
           


;; IO(String)
;; Guess every character in word.
;; Finally, return game-status.
(define (solve word)
  (begin
    (map guess (string->list word))
    (game-status)))


;;
;; EXTRA -F3
;;;;;;;;;;;;;;;;;;;;;;;;;;
   
;; p: all-words as list of list of char
;; [Char] -> Char -> IO([[Char]])
;; Results in all the words in all-words that
;; have the character char.
(define (words-containing all-words char )
  (define (false-if-no-occurrences w)
    (< 0 (occurrences w char)))
  (filter false-if-no-occurrences all-words))
  


;; p: all-words as list of list of char
;;  : chars as a list of char
;; Results in all the words in all-words that have
;; every character in chars.
;; [Char] -> [Char] -> IO([[Char]])
(define (words-containing-ext all-words chars)
  (foldl (lambda (char accumulated)
           ; From the already filtered list of words of the last call
           ; to this lambda.
           
           ; Only get the ones that have the current character.
           (words-containing accumulated char))
         all-words
         chars))

;; IO([String])
  ;;
;; Returns all the strings in the glossary that have the characters chars.
(define (sieve chars)
  (map list->string (words-containing-ext glossary chars)))

