#lang racket/base

#|
Usage:

racket --require print-latex.rkt --main -- --template exam.tex --out exam.tex  ../../pool/active-group/*.xml

Additional flags:
--language de  specify language
--language en  specify language
|#

(require racket/match
         racket/contract
         (only-in racket/string string-trim string-join string-split)
         (only-in racket/port with-output-to-string)
         (only-in srfi/19 date->string)
         xml)
(require "question.rkt"
         "parse.rkt"
         "file-template.rkt")

(define (question-word language)
  (match language
    ("de" "Frage")
    ("en" "Question")))

(define (write-question+explanation/language question language)
  (display "\\begin{examQuestionStem}{")
  (display (question-id question))
  (display "}{")
  (display (question-points question))
  (display "}{")
  (write-points-word (question-points question) language)
  (display "}")
  (newline)
  (write-localized-text (question-stem question) language)
  (newline)
  (display "\\end{examQuestionStem}")
  (newline)
           
  (write-question/language question language)
  
  (cond
    ((question-explanation question)
     => (lambda (explanation)
          (newline)
          (display "\\begin{examQuestionExplanation}") (newline)
          (write-localized-text explanation language)
          (display "\\end{examQuestionExplanation}") (newline))))
  (newline))

(define (write-question/language question language)
  (match question
    ((pick-question id points release history learning-goals stem explanation pick-options)
     (write-pick-header pick-options points language)
     (newline)
     (display "\\begin{examPQuestion}")
     (for-each (lambda (pick-option)
                 (newline)
                 (write-pick-option pick-option language))
               pick-options)
     (display "\\end{examPQuestion}") (newline))
    ((category-question id points release history learning-goals stem explanation categories statements)
     (write-category-header points language)
     (newline)
     (display "\\begin{examKQuestion}")
     (newline)
     (display "\\examQuestionKHeader")
     (for-each (lambda (category)
                 (display "{")
                 (write-localized-text (category-text category) language)
                 (display "}"))
               categories)
     (newline)
     (for-each (lambda (statement)
                 (newline)
                 (write-statement statement categories language))
               statements)
     (display "\\end{examKQuestion}") (newline))))

(define (write-statement statement categories language)
  (display "\\examQuestionKItem{")
  (display (string-downcase (statement-identifier statement)))
  (display "}{")
  (write-localized-text (statement-text statement) language)
  (display "}")
  (for-each (lambda (category)
              (display
               (if (equal? category (statement-correct-category statement))
                   "{\\examYes}"
                   "{\\examNo}")))
            categories)
  (newline))

(define (write-category-header points language)
  (display "\\examQuestionInstructions{")
  (match language
    ("de"
     (display "K-Frage: Bitte ordnen Sie jede Antwort einer Kategorie zu.") (newline))
    ("en"
     (display "K-Question:  Assign all the answers.") (newline)))
  (display "}"))

(define (write-pick-header pick-options points language)
  (define pick-options-count (length pick-options))
  (define correct-pick-options-count (count-correct-pick-options pick-options))

  (display "\\examQuestionInstructions{")
  (match (cons correct-pick-options-count language)
    ((cons 1 "de")
     (display "A-Frage: Bitte kreuzen Sie die richtige Antwort an.") (newline))
    ((cons 1 "en")
     (display "A-Question: Choose one answer.") (newline))
    ((cons _ "de")
     (display "P-Frage: Wählen Sie aus den folgenden ")
     (display (number-word/de pick-options-count))
     (display " Anworten die \\textbf{")
     (display (number-word/de correct-pick-options-count))
     (display "} Antworten aus, die am besten passen."))
    ((cons _ "en") 
     (display "P-Question: From the following ")
     (display (number-word/en pick-options-count))
     (display " answers select \\textbf{")
     (display (number-word/en correct-pick-options-count))
     (display "} that fit best.")))
  (display "}"))
  
(define (write-points points language)
  (display points) (display " ")
  (write-points-word points language))

(define (write-points-word points language)
  (display
   (match (cons points language)
     ((cons 1 "de") "Punkt")
     ((cons 1 "en") "Point")
     ((cons _ "de") "Punkte")
     ((cons _ "en") "Points"))))
  
(define (points-word/de points)
  (case points
    ((1) "Punkt")
    (else "Punkte")))

(define (points-word/en points)
  (case points
    ((1) "point")
    (else "points")))

(define (number-word/de n)
  (match n
    (0 "null")
    (1 "eine")
    (2 "zwei")
    (3 "drei")
    (4 "vier")
    (5 "fünf")
    (6 "sechs")
    (7 "sieben")
    (8 "acht")
    (9 "neun")
    (10 "zehn")
    (11 "elf")
    (12 "zwölf")
    (_ (number->string n))))

(define (number-word/en n)
  (match n
    (0 "zero")
    (1 "one")
    (2 "two")
    (3 "three")
    (4 "four")
    (5 "five")
    (6 "six")
    (7 "seven")
    (8 "eight")
    (9 "nine")
    (10 "ten")
    (11 "eleven")
    (12 "twelve")
    (_ (number->string n))))


(define (count-correct-pick-options pick-options)
  (length
   (filter (lambda (pick-option)
             (eq? 'correct (pick-option-validity pick-option)))
           pick-options)))

(define (write-pick-option pick-option language)
  (display "\\examQuestionPItem{")
  (display (string-downcase (pick-option-identifier pick-option)))
  (display "}{")
  (write-localized-text (pick-option-text pick-option) language)
  (display "}{")
  (display
   (case (pick-option-validity pick-option)
     ((correct) "\\examYes")
     (else "\\examNo")))
  (display "}")
  (newline))

(define (write-localized-text localized-text language)
  (write-multi-line (quote-latex (get-localized-text localized-text language) language)))

(define (write-multi-line text)
  (for-each (lambda (line)
              (display (string-trim line))
              (newline))
            (string-split text #rx" *\n *")))

(define (get-localized-text localized-text language)
  (cond
    ((assoc language (localized-text-langs+texts localized-text))
     => cdr)
    (else
     (error "localized text not available for language"
            localized-text language))))

(define (quote-latex text language)
  (define-values (open-quote* closed-quote*)
    (match language
      ("de" (values "\"`" "\"'"))
      ("en" (values "``" "''"))
      (_
       (error "unhandled language" language))))
  (define open-quote (reverse (string->list open-quote*)))
  (define closed-quote (reverse (string->list closed-quote*)))

  (let loop ((chars (string->list text)) (open-quote? #f) (rev-result '()))
    (if (null? chars)
        (list->string (reverse rev-result))
        (case (car chars)
          ((#\&) (loop (cdr chars) open-quote? (cons #\& (cons #\\ rev-result))))
          ((#\#) (loop (cdr chars) open-quote? (cons #\# (cons #\\ rev-result))))
          ((#\%) (loop (cdr chars) open-quote? (cons #\% (cons #\\ rev-result))))
          ((#\") (loop (cdr chars) (not open-quote?)
                       (if open-quote?
                           (append closed-quote rev-result)
                           (append open-quote rev-result))))
          (else (loop (cdr chars) open-quote? (cons (car chars) rev-result)))))))

(define (min-points-as-string total-points language)
  (define min-points100  (* total-points 60))
  (if (zero? min-points100)
      "0"
      (let* ((digits (number->string min-points100))
             (count (string-length digits)))
        (string-append (substring digits 0 (- count 2))
                       (match language
                         ("de" ",")
                         ("en" "."))
                       (substring digits 2 count)))))

(define (make-exam questions template-filename out-filename language)
  (define total-points (apply + (map question-points questions)))
  (define questions-text
    (with-output-to-string
      (lambda ()
        (for-each (lambda (question)
                    (write-question+explanation/language question language))
                  questions))))
  (copy-file/replacing template-filename out-filename
                       `(("%EXAMQUESTIONS%" . ,questions-text)
                         ("%EXAMQUESTIONCOUNT%" . ,(number->string (length questions)))
                         ("%EXAMTOTALPOINTS%" . ,(number->string total-points))
                         ("%EXAMMINPOINTS%" . ,(min-points-as-string total-points language)))))
  
(module+ test
  (require rackunit)
  (require "parse.rkt")

  (check-equal? (min-points-as-string 88 "de")
                "52,80")
  (check-equal? (min-points-as-string 88 "en")
                "52.80")

  (define q1
    (pick-question
     "Q-20-04-01"
     1
     #f
     (list (history-item (date* 0 0 0 8 8 2024 4 220 #f 7200 0 "") "content identical to mock exam 2024.1-rev0-DE-20240202"))
     (list (learning-goal-reference (curriculum-version 2025 1) (lg-number 1 1)) (learning-goal-reference (curriculum-version 2023 1) (lg-number 1 1)))
     (localized-text
      '(("de" . "\n        Wie viele Definitionen des Begriffes \"Softwarearchitektur\" gibt es?\n    ")
        ("en" . "\n        How many definitions of “software architecture” exist?\n    ")))
     (localized-text
      '(("de"
         .
         "\n      Die Vielfalt der Definitionen von Softwarearchitektur resultiert unter anderem aus unterschiedlichen Perspektiven, Zielgruppen und Entwicklungsmethoden.\n    ")
        ("en"
         .
         "\n      The variety of definitions of software architecture results, among other things, from different perspectives, target groups and development methods.\n    ")))
     (list
      (pick-option
       'distractor
       "A"
       (localized-text
        '(("de" . "\n        Genau eine für alle Arten von Systemen.\n      ") ("en" . "\n        Exactly one for all kinds of systems.\n      "))))
      (pick-option
       'distractor
       "B"
       (localized-text
        '(("de"
           .
           "\n        Eine für jede Art von Softwaresystem (z. B. \"eingebettet\",\"Echtzeit\", \"Entscheidungsunterstützung\", \"Web\", \"Batch\", …).\n      ")
          ("en" . "\n        One for every kind of software system (e.g. “embedded”, “real-time”, “decision support”, “web”, “batch”, …).\n      "))))
      (pick-option
       'correct
       "C"
       (localized-text
        '(("de" . "\n        Ein Dutzend oder mehr unterschiedliche Definitionen.\n      ")
          ("en" . "\n        A dozen ore more different definitions.\n      ")))))))

  (define q2
    (pick-question
     "Q-20-04-02"
     1
     #f
     (list
      (history-item
       (date* 0 0 0 8 8 2024 4 220 #f 3600 0 "")
       "content identical to mock exam 2024.1-rev0-DE-20240202"))
     (list
      (learning-goal-reference (curriculum-version 2025 1) (lg-number 1 1))
      (learning-goal-reference (curriculum-version 2023 1) (lg-number 1 1)))
     (localized-text
      '(("de"
         .
         "\n        Welche DREI der folgenden Aspekte werden durch den Begriff „Softwarearchitektur“ abgedeckt? \n    ")
        ("en"
         .
         "\n        Which THREE of the following aspects are covered by the term “software architecture”? \n    ")))
     (localized-text
      '(("de"
         .
         "\n      Programmierkonventionen und Hardware-Sizing sind in der Regel keine Hauptbestandteile der Softwarearchitektur, da die Architektur sich auf die logische Struktur des Systems konzentriert, während diese Aspekte eher auf der Implementierungsebene und den Ressourcenanforderungen liegen. \n    ")
        ("en"
         .
         "\n      Coding conventions and hardware sizing are usually not major components of software architecture, since architecture focuses on the logical structure of the system, while these aspects are more at the implementation level and resource requirements.\n    ")))
     (list
      (pick-option
       'correct
       "A"
       (localized-text
        '(("de" . "\n        Komponenten\n      ") ("en" . "\n        Components\n      "))))
      (pick-option
       'correct
       "B"
       (localized-text
        '(("de" . "\n        Querschnittskonzepte\n      ")
          ("en" . "\n        Cross-cutting concepts\n      "))))
      (pick-option
       'correct
       "C"
       (localized-text
        '(("de" . "\n        (interne und externe) Schnittstellen\n      ")
          ("en" . "\n        (internal and external) Interfaces\n      "))))
      (pick-option
       'distractor
       "D"
       (localized-text
        '(("de" . "\n        Programmierkonventionen (\"coding conventions\")\n      ")
          ("en" . "\n        Coding conventions\n      "))))
      (pick-option
       'distractor
       "E"
       (localized-text
        '(("de" . "\n        Hardware-Sizing\n      ")
          ("en" . "\n        Hardware sizing\n      ")))))))
  
    (define q4
    (category-question
     "Q-17-13-02"
     2
     #f
     (list (history-item (date* 0 0 0 8 8 2024 4 220 #f 7200 0 "") "content identical to mock exam 2024.1-rev0-DE-20240202"))
     (list (learning-goal-reference (curriculum-version 2023 1) (lg-number 3 2)))
     (localized-text
      '(("de"
         .
         "\n      Bei Ihrem Projekt arbeiten drei Architekt:innen und sieben\n      Entwickler:innen an der Dokumentation der\n      Softwarearchitektur. Welche Methoden eignen sich zur\n      Gewährleistung einer konsistenten und zweckmäßigen Dokumentation\n      und welche nicht?\n    ")
        ("en"
         .
         "\n      In your project, three architects and seven developers are working on the\n      documentation of the software architecture. Which methods are\n      appropriate in order to achieve a consistent and adequate\n      documentation, and which are not?\n    ")))
     (localized-text
      '(("de"
         .
         "\n      Um eine konsistente und zweckmäßige Dokumentation sicherzustellen, eignet sich die Koordination durch die leitende Architekt:in sowie die Verwendung identischer Vorlagen. Die automatische Extraktion aller Teile der Dokumentation aus dem Quellcode ist weniger empfehlenswert, da dies möglicherweise nicht alle relevanten Informationen oder Kontexte berücksichtigt und die Qualität der Dokumentation beeinträchtigen kann.\n      Aspekte wie Begründungen oder Alternativen sind nicht im Code enthalten, sondern müssen in die Dokumentation aufgenommen werden, daher können nicht alle Teile der Dokumentation aus dem Quellcode extrahiert werden. \n    ")
        ("en"
         .
         "\n      To ensure consistent and useful documentation, coordination by the lead architect and the use of identical templates are recommended. Automatic extraction of all parts of the documentation from the source code is less recommended, as this may not include all relevant information or contexts and may affect the quality of the documentation.\n      Things like reasoning or alternatives won’t be contained in code, but need to be included in documentation, therefore not all parts of documentation can be extracted from source code. \n    ")))
     (list
      (category "a" (localized-text '(("de" . "Geeignet") ("en" . "appropriate"))))
      (category "b" (localized-text '(("de" . "Nicht geeignet") ("en" . "not appropriate")))))
     (list
      (statement
       (category "a" (localized-text '(("de" . "Geeignet") ("en" . "appropriate"))))
       "A"
       (localized-text
        '(("de" . "\n          Die/der leitende Architekt:in koordiniert die Erstellung der Dokumentation.\n        ")
          ("en" . "\n          The lead architect coordinates the creation of the documentation.\n        "))))
      (statement
       (category "a" (localized-text '(("de" . "Geeignet") ("en" . "appropriate"))))
       "B"
       (localized-text
        '(("de" . "\n          Für die Dokumentation werden identische Vorlagen verwendet.\n        ")
          ("en" . "\n          Identical templates are used for the documentation.\n        "))))
      (statement
       (category "b" (localized-text '(("de" . "Nicht geeignet") ("en" . "not appropriate"))))
       "C"
       (localized-text
        '(("de" . "\n          Alle Teile der Dokumentation werden automatisch aus dem Quellcode extrahiert.\n        ")
          ("en" . "\n          All parts of the documentation are automatically extracted from the source code.\n        ")))))))
    
    (write-question+explanation/language q1 "de")
    (write-question+explanation/language q2 "de")
    (write-question+explanation/language q4 "de")

    (write-question+explanation/language q1 "en")
    (write-question+explanation/language q2 "en")
    (write-question+explanation/language q4 "en")

    (check-equal? (quote-latex "Hello \"Mike\" & \"Gernot\"" "de")
                  "Hello \"`Mike\"' \\& \"`Gernot\"'")
    (check-equal? (quote-latex "Hello \"Mike\" & \"Gernot\"" "en")
                  "Hello ``Mike'' \\& ``Gernot''")
    )

(provide (contract-out
          (make-exam ((listof question?) (or/c path? string?) (or/c path? string?) string? . -> . any))))
          
