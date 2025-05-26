#lang racket/base
(require (only-in racket/string string-replace)
         racket/contract)

(define (copy-file/replacing in-path out-path replace-alist)
  (call-with-input-file in-path
    (lambda (in-port)
      (call-with-output-file out-path
        (lambda (out-port)
          (let loop ()
            (define line (read-line in-port 'any))
            (unless (eof-object? line)
              (define new-line
                (foldl (lambda (pair line)
                         (string-replace line (car pair) (cdr pair)))
                       line
                       replace-alist))
              (display new-line out-port)
              (newline out-port)
              (loop))))
        #:exists 'replace))))
            

(provide (contract-out
          (copy-file/replacing
           ((or/c path? string?) (or/c path? string?) (listof (cons/c string? string?)) . -> . any))))

