#lang racket
;;;;; Michael Wright
;;;;; mswvgc@umsystems.edu
;;;;; 03/9/2023
;;;;; CS 490 FP Program 4

; Program is unfinished - still working on the ranking of scores

;;;;;;;;;;;;;;;;;;;;;;;preprocessing;;;;;;;;;;;;;;;;;;;;;;;;;;

; Read stop words from a file
(define stop-words-list (file->lines "stop_words_english.txt"))

; Function to remove stop words from a list of words
(define (remove-stop-words words stop-words)
  (filter (lambda (word) (not (member word stop-words))) words))

(define (process-file file-path)
  (define input-string
    (call-with-input-file file-path (lambda (input-port) (port->string input-port))))

  (define char-list (string->list input-string))

  (define normalized-char-list
    (map (lambda (char)
           (cond
             [(member char '(#\newline #\" #\, #\. #\? #\! #\- #\— #\: #\;)) #\space]
             [else (char-downcase char)]))
         char-list))

  (define normalized-string (list->string normalized-char-list)) 

  ; Remove stop words before counting words
  (define words (remove-stop-words (string-split normalized-string) stop-words-list))

  (define (count-words lst)
    (define (process-words ht words)
      (if (null? words)
          ht
          (let* ([word (car words)]
                 [remaining-words (cdr words)]
                 [current-count (hash-ref ht word 0)]
                 [new-ht (hash-set ht word (+ current-count 1))])
            (process-words new-ht remaining-words))))

    (define ht (hash))
    (process-words ht lst))

  (define word-frequencies (count-words words))

  (define (sum-hash hash)
    (apply + (hash-values hash))) 

  (define total (sum-hash word-frequencies))

  (define updated-word-frequencies-alist
    (hash-map
     word-frequencies
     (lambda (word value)
       (cons word (* -1 (log (/ value total) 10))))))

  (define output-hash
    (for/hash ([pair updated-word-frequencies-alist])
      (values (car pair) (cdr pair))))

  ; Save hash to a file only if the file does not exist
  (define hash-file-path (string-append file-path ".hash"))
  (unless (file-exists? hash-file-path)
    (with-output-to-file hash-file-path
      (lambda () (write output-hash))))

  output-hash)

; Function to process all files and generate a hash of hashes
(define (process-all-files file-paths)
  (for/fold ([all-hashes (hash)]) ([file-path file-paths])
    (hash-set all-hashes file-path (process-file file-path))))


; Process all files and save the hash of hashes to a file
(define all-hashes (process-all-files '("Files/001.txt" "Files/002.txt" "Files/003.txt" "Files/004.txt" 
                                        "Files/005.txt" "Files/006.txt" "Files/007.txt" "Files/008.txt" 
                                        "Files/009.txt" "Files/010.txt" "Files/011.txt" "Files/012.txt" 
                                        "Files/013.txt" "Files/014.txt" "Files/015.txt" "Files/016.txt" 
                                        "Files/017.txt" "Files/018.txt" "Files/019.txt" "Files/020.txt" 
                                        "Files/021.txt" "Files/022.txt" "Files/023.txt" "Files/024.txt" 
                                        "Files/025.txt")))

; Save all_hashes to a file only if the file does not exist
(define all-hashes-file "all_hashes")
(unless (file-exists? all-hashes-file)
  (with-output-to-file all-hashes-file
    (lambda () (write all-hashes))))



;;;;;;;;;;;;;;;;;user input;;;;;;;;;;;;;;;;;;;;;;;;

; Load the hashes from the files created during preprocessing.
(define (load-hash file-path)
  (call-with-input-file file-path read))

(define file-paths '("Files/001.txt.hash" "Files/002.txt.hash" "Files/003.txt.hash" 
                     "Files/004.txt.hash" "Files/005.txt.hash" "Files/006.txt.hash" 
                     "Files/007.txt.hash" "Files/008.txt.hash" "Files/009.txt.hash" 
                     "Files/010.txt.hash" "Files/011.txt.hash" "Files/012.txt.hash" 
                     "Files/013.txt.hash" "Files/014.txt.hash" "Files/015.txt.hash" 
                     "Files/016.txt.hash" "Files/017.txt.hash" "Files/018.txt.hash" 
                     "Files/019.txt.hash" "Files/020.txt.hash" "Files/021.txt.hash" 
                     "Files/022.txt.hash" "Files/023.txt.hash" "Files/024.txt.hash" 
                     "Files/025.txt.hash"))


(define loaded-hashes (map load-hash file-paths))




; Prompt the user to enter a search word or words.
(display "Enter your search word(s), separated by spaces: ")
(define search-words (string-split (read-line)))



; For each word the user enters, iterate over each file in the corpus, and if the word appears in the file, retrieve its score.
(define (score-word-in-file word hash-table)
  (hash-ref hash-table word 0)) ; Use 0 as a default score if the word is not found in the file

(define (score-words-in-file words hash-table)
  (map (lambda (word) (score-word-in-file word hash-table)) words))


(define all-scores (map (lambda (hash-table) (score-words-in-file search-words hash-table)) loaded-hashes))

