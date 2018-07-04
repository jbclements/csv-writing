#lang racket/base

(provide table?
         display-table
         table->string
         table-row->string
         table-cell->string)

(require (only-in racket/list add-between)
         (only-in racket/format ~r))

;; a table is a list of lists, that's all.
(define (table? t)
  (and (list? t)
       (andmap list? t)))

(struct csv-printing-params
  (table-cell->string
   string-cell->string
   number-cell->string
   boolean-cell->string
   symbol-cell->string
   quotes-only-when-needed?
   quoted-double-quote))

(define (make-csv-printing-params
         #:table-cell->string [a default-table-cell->string]
         #:string-cell->string [b default-string-cell->string]
         #:number-cell->string [c default-number-cell->string]
         #:boolean-cell->string [d default-boolean-cell->string]
         #:symbol-cell->string [e default-symbol-cell->string]
         #:quotes-only-when-needed? [f #t]
         #:quoted-double-quote [g "\"\""])
  (csv-printing-params
   a b c d e f g))


;; given a table and a port, write the table as a CSV to the port
(define (display-table t [port (current-output-port)]
                       #:printer-params
                       [printer-params
                        default-csv-printer-params])
  (unless (table? t)
    (raise-argument-error 'display-table
                          "table"
                          0 t port))
  (unless (output-port? port)
    (raise-argument-error 'display-table
                          "table"
                          0 t port))
  (for ([row (in-list t)])
    (display-table-row row port
                       #:printer-params printer-params)))

;; given a table, return a string representing that CSV
(define (table->string t
                       #:printer-params
                       [printer-params
                        default-csv-printer-params])
  (unless (table? t)
    (raise-argument-error 'table->string
                          "table"
                          0 t))
  (apply
   string-append
   (add-between
    (for/list ([row (in-list t)])
      (table-row->string row #:printer-params printer-params))
    "\n")))

;; given a row of a table and a port, write the row as a CSV line
;; to the port
(define (display-table-row row port
                           #:printer-params
                           [printer-params
                            default-csv-printer-params])
  (unless (list? row)
    (raise-argument-error 'display-table-row
                          "list"
                          0 row port))
  (unless (output-port? port)
    (raise-argument-error 'display-table
                          "table"
                          0 row port))
  (display (table-row->string row #:printer-params printer-params)
           port)
  (newline port))

;; given a row of a table, return a string representing the row
;; as a CSV line
(define (table-row->string row
                           #:printer-params
                           [printer-params
                            default-csv-printer-params])
  (unless (list? row)
    (raise-argument-error 'table-row->string
                          "list"
                          0 row))
  (apply
   string-append
   (add-between
    (for/list ([cell (in-list row)])
      (table-cell->string cell))
    ",")))

;; given a single cell, return the cell as a string
(define (table-cell->string cell
                            #:printer-params
                            [printer-params
                             default-csv-printer-params])
  ((csv-printing-params-table-cell->string
    default-csv-printer-params)
   cell))

(define (default-table-cell->string cell
          #:printer-params [printer-params
                            default-csv-printer-params]
          )
  (cond [(string? cell)
         ((csv-printing-params-string-cell->string
           default-csv-printer-params) cell)]
        [(number? cell)
         ((csv-printing-params-number-cell->string
           default-csv-printer-params) cell)]
        [(boolean? cell)
         ((csv-printing-params-boolean-cell->string
           default-csv-printer-params) cell)]
        [(symbol? cell)
         ((csv-printing-params-symbol-cell->string
           default-csv-printer-params) cell)]
        [else
         (raise-argument-error 'default-table-cell->string
                               "string, number, boolean, or symbol"
                               0 cell)]))

;; the standard conversion from a racket string to a CSV
;; string.
(define (default-string-cell->string str
          #:printer-params [printer-params
                            default-csv-printer-params])
  (unless (string? str)
    (raise-argument-error 'default-string-cell->string
                          "string"
                          0 str))
  (cond [(and (has-no-danger-chars? str)
              (csv-printing-params-quotes-only-when-needed?
               printer-params))
         str]
        [else
         (string-append
          "\""
          (regexp-replace #px"\"" str
                          (csv-printing-params-quoted-double-quote
                           printer-params))
          "\"")]))

(define (has-no-danger-chars? str)
  (not (regexp-match? #px"[,\"\n]" str)))

;; the standard conversion from a racket number to a CSV string
;; NB: this is probably the function you're most likely to want
;; to alter...
;; in my experience, most clients *hate* fractions.
;; also, the default handler doesn't accept complex numbers
(define (default-number-cell->string num)
  (unless (rational? num)
    (raise-argument-error 'default-string-cell->string
                          "rational number"
                          0 num))
  (~r num))

;; more hard choices here... the default TRUE/FALSE will collide
;; with string representations
(define (default-boolean-cell->string bool)
  (unless (boolean? bool)
    (raise-argument-error 'default-boolean-cell->string
                          "boolean"
                          0 bool))
  (cond [bool "TRUE"]
        [else "FALSE"]))

;; again, symbols are just going to collide hard with strings.
(define (default-symbol-cell->string sym)
  (unless (symbol? sym)
    (raise-argument-error 'default-symbol-cell->string
                          "symbol"
                          0 sym))
  (default-string-cell->string (symbol->string sym)))


(define default-csv-printer-params
  (make-csv-printing-params))

(module+ test
  (require rackunit)
  (check-equal? (has-no-danger-chars? "abcdef") #t)
  (check-equal? (has-no-danger-chars? "abc,def") #f)
  (check-equal? (has-no-danger-chars? "abcde\"f") #f)
  (check-equal? (has-no-danger-chars? "a\nbcdef") #f)
  (check-equal? (default-string-cell->string "abc\"def")
                "\"abc\"\"def\"")
  (check-equal? (default-string-cell->string "abcdef")
                "abcdef")
  (check-equal? (default-string-cell->string "abc\"def"
                  #:printer-params
                  (make-csv-printing-params
                   #:quoted-double-quote "##"))
                "\"abc##def\"")

  (check-equal? (default-string-cell->string "abcdef"
                  #:printer-params
                  (make-csv-printing-params
                   #:quotes-only-when-needed? #f))
                "\"abcdef\"")
  
  (check-equal? (default-number-cell->string 234)
                "234")
  (check-equal? (default-number-cell->string 3/2)
                "1.5")
  (check-equal? (default-number-cell->string 4/3)
                "1.333333")
  (check-equal? (default-number-cell->string -1242134.02)
                "-1242134.02")

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

  )