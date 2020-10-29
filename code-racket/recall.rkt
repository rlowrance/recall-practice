#lang racket
(require racket/string)
(require racket/date)
(require racket/hash)
(require racket/struct)
(require debug)

; -> hash
; create runtime options (global variable)
; TO IMPLEMENT
;  -n N limit number of items to review in this session
;  -t N time limit in minutes for this session
(define options
  (hash 'path-to-data "../data/"
        'in-schedule-filename "schedule.txt"
        'out-schedule-filename "schedule-new.txt"))

; run-time control
(date-display-format 'iso-8601); otherwise, dates are displayed in American format

; global types

; symbol symbol string -> item-id
(struct item-id (source-id section-id stem) #:transparent)

; string string number seconds days -> fib
(struct fib (stem choice weight due) #:transparent)

; symbol symbol days -> schedule
(struct schedule (date time interval) #:transparent)

; string -> string (with IO)
; read line of text after prompting user
(define (prompt/read prompt)
  (display prompt)
  (read-line))

; string -> (values string-stem string-choice)
; parse line, pulling out stem and choices
; ex (parse-fib-line "ab[cd[e]f]g.") -> (values "ab___g" "cd[ef]f")
(define (parse-fib-line line)
  (define (as-string list)(list->string (reverse list)))
  (define (prepend-blank list) (let ((blank #\_ )) (cons blank (cons blank (cons blank list)))))
  ; boolean list-chars int list-chars list-chars -> (values string string)
  (define (iter in-stem? remainder-chars nested stem-chars choice-chars)
    ;(printf "iter ~a ~a ~a ~a ~a ~n" in-stem? (list->string remainder-chars) nested (as-string stem-chars) (as-string choice-chars))
    (cond
      ((null? remainder-chars) (let ((stem (list->string (reverse stem-chars)))
                                     (choice (list->string (reverse choice-chars))))
                                 ;(printf "parse-fib-line~n ~a~n ~a~n ~a~n" line stem choice)
                                 (values stem choice)))
      (#t (let ((first (car remainder-chars))
                (rest (cdr remainder-chars)))
            ;(printf "first:~a ~n" first)
            (cond
              ((equal? first #\[)
               (cond (in-stem? (iter #f rest nested (prepend-blank stem-chars) choice-chars));start [...]
                     (#t (iter #f rest (add1 nested) stem-chars (cons first (choice-chars))))));nested [] in the blank
              ((equal? first #\])
               (cond (in-stem? (iter #t rest nested (cons first stem-chars) choice-chars));save ] not in blank
                     (#t (cond
                           ((> nested 0) (iter #f rest (sub1 nested) stem-chars (cons first choice-chars))); unnest blank
                           (#t (iter #t rest nested stem-chars choice-chars)))))); restart stem
              (in-stem? (iter #t rest nested (cons first stem-chars) choice-chars)); accumulate stem
              (#t (iter #f rest nested stem-chars (cons first choice-chars)))))))); accumulate blank
  (iter #t (string->list line) 0 '() '())
  )

; path number string string int -> hash
; return a hash containing any items (possibly none, one, or many [in a future version])
(define (process-a-section-file path-to-section-file weight source-id section-id)
  ;(printf "process-a-section-file: ~a ~a ~a ~a ~a ~n" path-to-section-file weight source-id section-id fibs)
  ; string hash -> hash'
  ; augment hash to possibly reflect info in the line
  (define (process-section-line line result)
    ;(printf "process-section-line: ~a ~a ~n" line fibs)
    (cond ((equal? 0 (string-length line)) result)
          (#t (let ((first-char (string-ref line 0)))
                (cond
                  ((equal? first-char #\.) result);for now, ignore .cite and other dotted markup
                  ((equal? first-char #\#) result);skip comment lines
                  (#t (let-values (((stem choice) (parse-fib-line line)))
                        (hash-set result (item-id source-id section-id stem) (fib stem choice weight 0)))))))))
                        
  ; port hash -> hash'
  ; augment hash to reflect the info in the lines from a port
  (define (process-section-lines port result)
    (let ((line (read-line port)))
      (cond ((eof-object? line) result)
            (#t (process-section-lines port (process-section-line line result))))))
  
  (let ((port (open-input-file path-to-section-file #:mode 'text)))
    (begin0
      (process-section-lines port (hash))
      (close-input-port port))))

; path string -> boolean
; is the first word the given string?
(define (path-starts-with? path s)
  (define verbose #f)
  (let* ((pieces (string-split (path->string path) " "))
         (result (and (> (length pieces) 0) (equal? (car pieces) s))))
    (when verbose (printf "path-starts-with? ~a ~a -> ~a~n" path s result))
    result))

; path -> boolean
(define (hidden? path)
  (equal? #\. (string-ref (path->string path) 0)))

; path -> hash
; read and return active items
(define (build-active-items path-to-data-dir)
  (define verbose #f)
  (when verbose (printf "build-active-items: ~a ~n" path-to-data-dir))

  ; path number string string -> hash
  (define (process-a-source-dir path-to-source-dir weight source-id section-id)
    (when verbose (printf "process-a-source-dir: ~a ~a ~a ~a~n" path-to-source-dir weight source-id section-id))
    (define (iter objects result)
      (when verbose (printf "iter: ~a ~a~n" objects result))
      (cond ((null? objects) result)
            (#t (let* ((first (car objects))
                       (rest (cdr objects))
                       (path-to-section-file (build-path path-to-source-dir first)))
                  (iter rest
                        (cond ((and (not (hidden? first))
                                    (path-starts-with? first section-id)
                                    (file-exists? path-to-section-file))
                               (hash-union result (process-a-section-file path-to-section-file weight source-id section-id)))
                              (#t result)))))))
    (iter (directory-list path-to-source-dir) (hash)))
  
  ; path num string string -> hash
  (define (process-sources-dir path-to-sources-dir weight source-id section-id)
    (when verbose (printf "process-sources-dir: ~a ~a ~a ~a~n" path-to-sources-dir weight source-id section-id))
    ; (list objects) hash -> hash'
    (define (iter file-system-objects result)
      (when verbose (printf "iter: ~a ~a~n" file-system-objects result))
      (cond ((null? file-system-objects) result)
            (#t (let* ((first (car file-system-objects))
                       (rest (cdr file-system-objects))
                       (path-to-source-dir (build-path path-to-sources-dir first)))
                  (iter rest
                        (cond ((and (not (hidden? first))
                                    (path-starts-with? first source-id)
                                    (directory-exists? path-to-source-dir))
                               (hash-union result (process-a-source-dir path-to-source-dir weight source-id section-id)))
                              (#t result)))))))
    (iter (directory-list path-to-sources-dir) (hash)))

  ; string -> hash
  (define (process-active-line active-line)
    (let* ((pieces (string-split active-line))
           (weight (string->number (list-ref pieces 0)))
           (source-id (list-ref pieces 1))
           (section-id (list-ref pieces 2)))
      (when verbose (printf "process-active-line weight ~a source-id ~a section-id ~a ~n" weight source-id section-id))
      (process-sources-dir (build-path path-to-data-dir "sources/") weight source-id section-id)))

  ; port hash -> hash'
  (define (process-active-lines port result)
    (let ((active-line (read-line port)))
      (cond ((eof-object? active-line) result)
            (#t (hash-union result (process-active-line active-line))))))
  
  ; main
  (let* ((path-to-active-file (build-path path-to-data-dir "active.txt"))
         (port (open-input-file path-to-active-file #:mode 'text)))
    (begin0
      (process-active-lines port (hash))
      (close-input-port port))))

; string string -> seconds
; convert date ("2020-10-15") and time ("12:56") to seconds past an epoch
(define (datetime->seconds date-string time-string)
  (define verbose #f)
  (when verbose (printf "datetime->seconds: ~a ~a~n" date-string time-string))
  (let* ((date-pieces (string-split date-string "-"))
         (year (string->number (list-ref date-pieces 0)))
         (month (string->number (list-ref date-pieces 1)))
         (day (string->number (list-ref date-pieces 2)))
         (time-pieces (string-split time-string ":"))
         (hour (string->number (list-ref time-pieces 0)))
         (minute (string->number (list-ref time-pieces 1)))
         (second (cond
                   ((= 3 (length time-pieces)) (list-ref time-pieces 2))
                   (#t 0)))
         (week-day 0);0 for Sunday
         (year-day 0);0 for January 1
         (dst? #t)
         (time-zone-offset 0))
    (date->seconds (date second minute hour day month year week-day year-day dst? time-zone-offset))))
    
; path -> hash
; read schedule file, returning a hash table with keys=item-id and values=schedule
(define (read-schedule-file path-to-file)
  (define verbose #t)
  (when verbose (printf "read-schedule-file: ~a~n" path-to-file))
  ; (list item-id schedule) hash -> hash'
  (define (process-datum datum result)
    (when verbose (printf "~nprocess-datum:~n datum:~a~n result:~a~n" datum result))
    (let* ((ks (car datum))
           (vs (cadr datum))
           (key (item-id (car ks) (cadr ks) (caddr ks)))
           (value (schedule (car vs) (cadr vs) (caddr vs))))
      (when verbose (printf "datum:~a~n   key:~a~nvalue:~a~n" datum key value))
      (cond ((hash-has-key? result key)(printf "~nerror in schedule file: duplicate item-id:~a~n" key) (exit 1))
            (#t (hash-set result key value)))))
  ; port hash -> hash'
  (define (process-data port result)
    (let ((datum (read port)))
      (cond ((eof-object? datum) result)
            (#t (process-datum datum result)))))
  (cond ((file-exists? path-to-file) (let ((port (open-input-file path-to-file #:mode 'text)))
                                       (begin0 (process-data port (hash))
                                               (close-input-port port))))
        (#t (hash))));return empty hash when schedule file does not exist

; number -> number
; convert number of days to number of seconds
(define (days->seconds x) (* x 60 24))

; hash hash -> hash
; set the last-presented and interval fields for each of the fibs, using the schedule when it is available
(define (add-schedule schedule fibs)
  (define verbose #t)
  (when verbose (printf "add-schedule~n"))
  (print-hash "schedule" schedule)
  (define (update-due-date old-fib new-schedule)
    (when verbose "updating old-fib to new-schedule:~n old-fib:~a~n new-schedule:~a~n" old-fib new-schedule)
    (let* ((scheduled-datetime (datetime->seconds (schedule-date new-schedule) (schedule-time new-schedule)))
           (scheduled-interval (schedule-interval new-schedule))
           (due (+ scheduled-datetime scheduled-interval)))
      (fib (fib-stem old-fib) (fib-choice old-fib) (fib-weight old-fib) due)))

  (for/hash (((k v) fibs))
    (when verbose (printf "~nk:~a~nv:~a~n" k v))
    (cond ((hash-has-key? schedule k) (values k (update-due-date v (hash-ref schedule k))))
          (#t (values k v)))))
           
; (listof number) -> number/index
; select random index drawn proportional to list of weights
(define (select-weighted xs); return index
  (define verbose #f)
  (define (weights->probabilities xs)
    (let ((sum (for/sum ((x xs)) x)))
      ;(printf "weights-probabilities: xs:~a sum:~a~n" xs sum)
      (for/list ((x xs)) (/ x sum))))
  (define (iter xs cum-probability n)
    (when verbose (printf "select-weighted::iter xs:~a cum-probability:~a n:~a~n" xs cum-probability n))
    (let* ((x (car xs))
           (rest (cdr xs))
           (cum (+ x cum-probability)))
      (cond ((null? rest) n)
            ((<= (random) cum) n)
            (else (iter rest cum (+ n 1))))))
  (iter (weights->probabilities xs) 0 0))
  
(define (test-select-weighted verbose)
  (for ((i (in-range 0 10)))
    (let ((xs '(1 1 1 1 1 1 1 1 1 1)))
      (when verbose (printf "xs:~a~n" xs))
      (when verbose (printf "test-selected-weight: xs:~a i:~a selected:~a~n" xs i (select-weighted xs)))
      )))

; hash -> item-id
; randomly select one item drawn proportion to weights
(define (select-random-item items)
  (define verbose #f)
  (when verbose (printf "select-random-item:~a~n" items))
  (let* ((xs (for/list ((v (hash-values items))) (fib-weight v))); list of weights
         (i (select-weighted xs)))
    (when verbose (printf "select-random-item: i:~a~n" i))
    (list-ref (hash-keys items) i)))


; item-id fib -> string
; present one item
(define (present-item-with-random-results id item); STEM: produce random scores
  (define verbose #t)
  (let ((answers '("again" "hard" "good" "easy"))
        (weights '(1 1 1 1)))
    (let ((r (list-ref answers (select-weighted weights))))
      (when verbose (printf "present-item: id:~a answer:~a~n" id r))
      r)))

(struct command (action detail)
  #:transparent
  #:guard (lambda (action detail name)
            (printf "construction command with ~a ~a ~a~n" action detail name)
            (cond ((and (equal? action "continue") (member detail '("again" "hard" "good" "easy"))) (values action detail))
                  ((and (equal? action "stop") (string? detail)) (values action detail))
                  ((and (equal? action "note") (string? detail)) (values action detail))
                  (#t (error (format "cannot construct a score with action ~a and detail ~a" action detail))))))

; -> item-id fib (listof commands) (listof commands)
; present item and retrieve user's score or commands
(define (present-item1 id item result)
  (define verbose #t)
  (printf "present-item1:~a ~a ~a~n" id item result)
  ; -> string? (or/c list? f#)
  (define (is-help s)  (member s '("help" "?")))
  (define (is-again s) (member s '("again" "a" "1")))
  (define (is-hard s)  (member s '("hard" "h" "2")))
  (define (is-good s)  (member s '("good" "g" "3" " ")))
  (define (is-easy s)  (member s '("easy" "e" "4")))
  (define (is-stop s)  (member s '("stop" "s" "\\stop" "\\s")))
  (define (is-note answer)
    (let* ((parts (string-split answer))
           (head (car parts)))
      (or (equal? head "note") (equal? head "\\note"))))
  ; -> string string
  (define (note-part answer)
    (let ((parts (string-split answer)))
      (string-join (cdr parts))))
  ; -> <void>
  (define (help)
    (printf "Valid commands are~n")
    (printf " again|a|1       ==> you got the wrong answer~n")
    (printf " hard |h|2       ==> you got the right answer, but you struggled, so decrease the repetition interval~n")
    (printf " good |g|3|SPACE ==> you got the right answer, and keep the repetition interval the same~n")
    (printf " easy |e|4       ==> you got the right answer, and increase the repetition interval~n")
    (printf " stop |s|9       ==> stop the recall exercise~n")
    (printf " note TEXT       ==> record a note for the item~n")
    (printf " help |?         ==> print this help and continue~n"))
  ; -> (listof command) (listof command)
  (define/debug (get-command result)
    (let ((str (prompt/read "enter your command or ? for help: ")))
      (printf "read was |~a|~n" str)
      (printf "result so far is ~a~n" result)
      (cond ((is-help str) (help) (get-command result))
            ((is-note str) (get-command (cons (command "note" (note-part str)) result)))
            ((is-again str) (cons (command "continue" "again") result))
            ((is-hard str) (when verbose (printf "is-hard")) (cons (command "continue" "hard") result))
            ((is-good str) (cons (command "continue" "good") result))
            ((is-easy str) (cons (command "continue" "easy") result))
            ((is-stop str) (cons (command "stop" "") result))
            (#t (printf "command \"~a\" not recognized~n" str)
                (help)
                (get-command result)))))
  (printf "source-id: ~a~nsection-id:~a~n~n~a~n" (item-id-source-id id) (item-id-section-id id) (item-id-stem id))
  (let ((str (prompt/read "Type your answer or \\stop or \\s to stop or \\note <text>: ")))
    (cond ((is-stop str) (cons (command "stop" "") result))
          ((is-note str) (present-item1 id item (cons (command "note" (note-part str)) result)))
          (#t (printf "~ncorrect answer:~a~n" (fib-choice item))
              (get-command result)))))



(define (test-present-item)
  (present-item (item-id "test source" "test section" "test stem") (fib "test stem" "test choice" 1 0)))

(define (test) (test-present-item))
          
; -> (hash/c item-id fib) (hash/c item-id (listof command))
; present the items to the user, capture user's commands for each item
(define (present-items fibs)
  (define verbose #t)
  (when verbose (printf "~npresent-items:~a~n" (length (hash-keys fibs))))
  ; item-id fib (listof command)
  (define (present-item id item)
    (define verbose #t)
    (let ((commands (present-item1 id item (list))))
      (when verbose (for ((command commands))
                      (printf "present-item::command:~a~n" commands)))
      commands))
  (define (contains-stop? commands)
    (cond ((null? commands) #f)
          ((equal? (command-action (car commands)) "stop") #t)
          (#t (contains-stop? (cdr commands)))))
  (define (iter todo result)
    (cond ((hash-empty? todo) (when verbose (print-hash "present-item::result" result)) result)
          (#t (let* ((next-item-id (select-random-item todo))
                     (next-item (hash-ref todo next-item-id))
                     (commands (present-item next-item-id next-item)))
                (cond ((contains-stop? commands) (hash-set result next-item-id commands))
                      (#t (iter (hash-remove todo next-item-id) (hash-set result next-item-id command))))))))
  (iter fibs (hash)))
  
; string -> number
; convert quiz result into interval multiplier
(define (increment r)
  (cond ((equal? r "again") 0)
        ((equal? r "hard") .8)
        ((equal? r "good") 2.5)
        ((equal? r "easy") 4)
        (else (printf "bad result (r):~a~n") (exit r))))

; unit tests
(define (unit-tests)
  (test-select-weighted #f))

; study (let <id> (<bindings>) <body>)

  
(define (sum xs)
  (let iter ((rest xs) (result 1))
    (cond ((null? xs) result)
          (else (iter (sub1 (car xs)) (+ (car xs) result))))))

; hash/schedule hash/quiz-results -> updated-schedule
; (hash/c item-id schedule) (hash/c item-id (listof command)) (hash/c item-id schedule)
; apply results for quiz to created an updated schedule
(define (update-schedule old-schedule quiz-result)
  (define verbose #t)
  (when verbose (printf "update-schedule:~n ~a~n ~a~n" old-schedule quiz-result))
  #;(when verbose (print-hash "update-schedule::old-schedule" old-schedule))
  #;(when verbose (print-hash "update-schedule::quiz-result" quiz-result))
  (define now (current-seconds))
  (define datetime (seconds->date now))
  (define date (string-append (number->string (date-year datetime))
                              "-"
                              (number->string (date-month datetime))
                              "-"
                              (number->string (date-day datetime))))
  (define time (string-append (number->string (date-hour datetime))
                              ":"
                              (number->string (date-minute datetime))))
  (define/debug (multiplier score)
    ((equal? score "hard") 0.8)
    ((equal? score "good") 2.5)
    ((equal? score "easy") 4.0))
  (define/debug (update-schedule item-id detail result)
    (when verbose (printf "update-schedule:~n ~a~n ~a~n ~a~n" item-id detail result))
    (hash-set result
              item-id
              (schedule date
                        time
                        (cond ((hash-has-key? old-schedule item-id)
                               (* (multiplier detail) (schedule-interval (hash-ref old-schedule item-id))))
                              (#t 1.0)))))
  (define/debug (iter2 item-id commands result)
    (when verbose (printf "iter2:~n ~a~n ~a~n ~a~n" item-id commands result))
    (cond ((null? commands) result)
          (#t (let ((command (car commands)))
                (cond ((equal? (command-action command "continue"))
                       (iter2 item-id (cdr commands) (update-schedule item-id (command-detail (car commands)))))
                      (#t (iter2 (cdr commands) result)))))))
  (define/debug (iter item-ids result)
    (when verbose (printf "iter:~n ~a~n ~a~n" item-ids result))
    (cond ((null? item-ids) result)
          (#t (let* ((item-id (car item-ids))
                     (commands (hash-ref quiz-result item-id)))
                (iter (cdr item-ids) (iter2 item-id commands result))))))
  (iter (hash-keys quiz-result) (hash)))
  

; string hash -> hash
; print hash and return it
(define (print-hash message h)
  (printf "~a~n" message)
  (for (((k v) h))
    (printf " k:~a~n" k)
    (printf " v:~a~n" v)
    (newline))
  h)

; hash hash path -> <void>
; write an updated schedule file
(define (write-schedule new-schedule path-to-file)
  (define verbose #t)
  (when verbose (print-hash "new-schedule" new-schedule))
  ; item-id schedule port -> IO
  (define (write-line item-id schedule port)
    (when verbose (printf "write-line:~a ~a~n" item-id schedule))
    (define line (list item-id schedule))
    (write line port)
    (newline))
  ; item item -> boolean
  (define (item-id-less<? a b)
    (let ((source-a (item-id-source-id a))
          (source-b (item-id-source-id b))
          (section-a (item-id-section-id a))
          (section-b (item-id-section-id b))
          (stem-a (item-id-stem a))
          (stem-b (item-id-stem b)))
      (cond ((string<? source-a source-b) #t)
            ((string>? source-a source-b) #f)
            (#t (cond ((string<? section-a section-b) #t)
                      ((string>? section-a section-b) #t)
                      (#t (cond ((string<? stem-a stem-b) #t)
                                (#t #f))))))))
  (let ((port (open-output-file path-to-file
               #:mode 'text
               #:exists 'replace)))
    (write-string "; lines are in Racket reader format" port) (newline port)
    (write-string "; each line is a list, with elements enclosed in parens" port) (newline port)
    (write-string "; fields: (source-id section-id item-stem) (last-date last-time interval-in-days)" port) (newline port)
    (define sorted-keys (sort (hash-keys new-schedule) item-id-less<?))
    (for ((sorted-key sorted-keys))
      (write-line sorted-key (hash-ref new-schedule sorted-key) port))
    (close-output-port port)))
                          

; driver
(define (go)
  (unit-tests)
  (random-seed 123); repeat random number draws
  (let* ((now (current-seconds)); time in seconds path UTC 1970-01-01
         (path-to-data-dir (string->path (hash-ref options 'path-to-data)))
         (path-to-input-schedule-file (build-path path-to-data-dir (string->path (hash-ref options 'in-schedule-filename))))
         (path-to-output-schedule-file (build-path path-to-data-dir (string->path (hash-ref options 'out-schedule-filename))))
         (old-schedule (read-schedule-file path-to-input-schedule-file))
         (raw-fibs (build-active-items path-to-data-dir))
         ;(_ (print-hash "raw-fibs" raw-fibs))
         (augmented-fibs (add-schedule old-schedule raw-fibs))
         ;(_ (print-hash "augmented-fibs" augmented-fibs))
         #;(_ (for (((k v) augmented-fibs))
              (printf "~nk:~a~nv:~a~n" k v)
              (printf "due:~a now:~a will-be-kept:~a~n" (fib-due v) now (<= (fib-due v) now))
              ))
         (active-fibs (for/hash (((k v) augmented-fibs); keep only fibs that are due
                            #:when (<= (fib-due v) now))
                        (printf "will be active: k:~a v:~a~n fib-due:~a now:~a~n" k v (fib-due v) now)
                        (values k v))))
    (print-hash "old-schedule" old-schedule)
    (print-hash "active-fibs" active-fibs)
    (let ((quiz-results (present-items active-fibs))); quiz-results: (hash/c item-id (listof command))
      (printf "quiz-results:~a~n" quiz-results)
      (for (((k v) quiz-results)) (printf "~nquiz-result:key:~a~nquiz-result:value:~a~n" k v))
      ; TODO: process any notes by writing them to DIR/notes.txt
      (define updated-schedule (update-schedule old-schedule quiz-results))
      (print-hash "updated-schedule" updated-schedule)
      (write-schedule updated-schedule path-to-output-schedule-file)
      (printf "count of active fibs:~a~n" (hash-count active-fibs)))))

  
    
