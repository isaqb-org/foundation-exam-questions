#lang racket/base

(require racket/cmdline
         (only-in racket/string string-trim))

(require "question.rkt"
         "parse.rkt"
         "print-latex.rkt")

(define (read-ids filename)
  (call-with-input-file
    filename
    (lambda (port)
      (let loop ((rev-lines '()))
        (define line (read-line port 'any))
        (if (eof-object? line)
            (reverse rev-lines)
            (let ()
              (define clean (string-trim line))
              (if (string=? clean "")
                  (loop rev-lines)
                  (loop (cons clean rev-lines)))))))))

(define (check-unound-ids questions ids)
  (for-each (lambda (id)
              (unless (memf (lambda (question)
                              (string=? (question-id question)
                                        id))
                            questions)
                (display "WARNING: question id not found: ") (display id) (newline)))
            ids))

(define (main . argv)
  (define template-filename (make-parameter #f))
  (define out-filename (make-parameter #f))
  (define language (make-parameter #f))
  (define selected-ids-filename (make-parameter #f))
  (command-line
   #:program "make-exam"
   #:argv argv
   #:once-each
   [("-o" "--out") filename
                   "Output filename"
                   (out-filename filename)]
   [("-t" "--template") filename
                        "Template filename"
                        (template-filename filename)]
   [("-l" "--language") lang
                        "Language (de or en)"
                        (language lang)]
   [("-i" "--ids-file") filename
                        "File with filter ids"
                        (selected-ids-filename filename)]
   #:args question-filenames
   (cond
     ((not (template-filename))
      (display "you must specify a template filename via --template <filename>\n"))
     ((not (out-filename))
      (display "you must specify a output filename via --out <filename>\n"))
     ((not (language))
      (display "you must specify a language via --language <language>\n"))
     (else
      (define all-questions (map parse-question-file question-filenames))
      (define filtered
        (cond
          ((selected-ids-filename)
           => (lambda (filename)
                (define ids (read-ids filename))
                (check-unound-ids all-questions ids)
                (filter (lambda (question)
                          (member (question-id question) ids))
                        all-questions)))
          (else all-questions)))
      (make-exam filtered
                 (template-filename)
                 (out-filename)
                 (language))))))

(apply main (vector->list (current-command-line-arguments)))
