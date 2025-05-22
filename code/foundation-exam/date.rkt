#lang racket/base
(require racket/date
         racket/contract
         (only-in srfi/19 string->date))

(define (parse-date text)
  (define date* (string->date text "~Y-~m-~d"))
  (date (date-second date*)
        (date-minute date*)
        (date-hour date*)
        (date-day date*)
        (date-month date*)
        (date-year date*)
        (date-week-day date*)
        (date-year-day date*)
        #f 0)) ; normalize

(module+ test
  (require rackunit)
  (check-equal? (parse-date "2024-08-01")
                (date 0 0 0 1 8 2024 4 213 #f 0)))

(provide
 (contract-out
  (parse-date (string? . -> . date?))))
