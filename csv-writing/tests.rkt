#lang racket

(require "main.rkt")

(module+ test
  (require rackunit)
  
  (check-equal? (table->string '(("abcdef"))
                  #:printing-params
                  (make-csv-printing-params
                   #:quotes-only-when-needed? #f))
                "\"abcdef\"")
  (check-equal? (table-row->string '("abcdef")
                  #:printing-params
                  (make-csv-printing-params
                   #:quotes-only-when-needed? #f))
                "\"abcdef\"")
  (check-equal? (table-cell->string "abcdef"
                  (make-csv-printing-params
                   #:quotes-only-when-needed? #f))
                "\"abcdef\"")
  (check-equal? (table-cell->string "abcdef"
                  (make-csv-printing-params
                   #:string-cell->string (λ (x) "ABC")))
                "ABC")

  (check-equal? (table-row->string '(342 bc "def" #t))
                "342,bc,def,TRUE")
  (check-exn #px"string, number, boolean, or symbol"
             (λ ()
               (table-cell->string ''bc)))

  (check-equal? (table->string '((name title)
                                 ("joey" bottle-washer)
                                 ("margo" sign-painter 34)))
                "name,title
joey,bottle-washer
margo,sign-painter,34")

  (check-equal? (table->string
                 '((a b) (c d))
                 #:printing-params
                 (make-csv-printing-params
                  #:table-cell->string (λ (str) "X")))
                "X,X\nX,X")

  (let ()
    (define op (open-output-string))
    (parameterize ([current-output-port op])
      (display-table '((name title)
                       ("joey" bottle-washer)
                       ("margo" sign-painter 34))))
    (check-equal? (get-output-string op)
                  "name,title
joey,bottle-washer
margo,sign-painter,34\n"))

  (let ()
    (define op2 (open-output-string))
    (display-table '((name title)
                     ("joey" bottle-washer)
                     ("margo" sign-painter 34 #f))
                   op2)
    (check-equal? (get-output-string op2)
                  "name,title
joey,bottle-washer
margo,sign-painter,34,FALSE\n"))

  (check-equal? (table? '()) #t)
  (check-equal? (table? 14) #f)
  (check-equal? (table? '((3 4) (4 5 6 "hamburger"))) #t)
  (check-equal? (table? (list (list (void)))) #t)

  ;; how about TSVs?

  ;; strings with tabs cause errors, others are passed unchanged
  (define (tsv-string-converter str)
    (match str
      [(regexp #px"\t")
       (error 'tsv-string-converter "no tabs allowed: ~e" str)]
      [other str]))
  
  (check-equal? (table->string
                 '(("a" "b") ("c" "d e"))
                 #:printing-params
                 (make-csv-printing-params
                  #:string-cell->string tsv-string-converter
                  #:column-separator "\t"))
                "a\tb\nc\td e")



  ;; checks of error code
  (check-exn #px"expected: table" (λ () (display-table 1234)))
  (check-exn #px"expected: port" (λ () (display-table '((1234))
                                                       4)))
  (check-exn #px"expected: csv-printing-params"
             (λ () (display-table '((1234))
                                  #:printing-params 287)))
  (check-exn #px"expected: list"
             (λ () (display-table-row 999 (open-output-string))))
  (check-exn #px"expected: output port"
             (λ () (display-table-row '(999) 888)))
  (check-exn #px"expected: csv-printing-params"
             (λ () (display-table-row '(999) (open-output-string)
                                      #:printing-params 287)))
  (check-exn #px"expected: table"
             (λ () (table->string 34)))
  (check-exn #px"expected: csv-printing-params"
             (λ () (table->string '((34))
                                  #:printing-params 7)))

  (check-exn #px"expected: list"
             (λ () (table-row->string 7)))
  (check-exn #px"expected: csv-printing-params"
             (λ () (table-row->string '(7)
                                      #:printing-params 8)))

  (check-exn #px"expected: procedure"
             (λ () (make-csv-printing-params #:string-cell->string 'abc)))

  )


