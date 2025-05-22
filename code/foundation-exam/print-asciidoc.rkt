#lang racket/base

(require racket/match
         racket/contract
         (only-in racket/string string-trim string-join string-split)
         (only-in srfi/19 date->string)
         xml)
(require "question.rkt"
         "parse.rkt")

(define (write-question question number languages)
  (for-each (lambda (language)
              (display (string-append "// tag::" (string-upcase language) "[]"))
              (newline) (newline)
              (display (string-append "=== " (question-word language) " "(number->string number)))
              (newline)
              (display (string-append "**ID: " (question-id question) "**"))
              (newline)
              (newline)
              (write-question/language question language)
              (display (string-append "// end::" (string-upcase language) "[]"))
              (newline) (newline))
            languages)
  (cond
    ((question-explanation question)
     => (lambda (explanation)
          (display "// tag::EXPLANATION[]") (newline)
          (for-each (lambda (language)
                      (display (string-append "// tag::" (string-upcase language) "[]")) (newline)
                      (write-multi-line (get-localized-text explanation language))
                      (display (string-append "// end::" (string-upcase language) "[]")) (newline))
                    languages)
          (display "// end::EXPLANATION[]") (newline)))))

(define (question-word language)
  (match language
    ("de" "Frage")
    ("en" "Question")))

(define (write-question/language question language)
  (match question
    ((pick-question id points release history learning-goals stem explanation pick-options)
     (write-pick-header pick-options points language)
     (write-multi-line (get-localized-text stem language))
     (newline)
     (display "[cols=\"1a,1,8\", frame=\"none\", grid=\"none\"]") (newline)
     (display "|===") (newline)
     (for-each (lambda (pick-option)
                 (newline)
                 (write-pick-option pick-option language))
               pick-options)
     (newline)
     (display "|===") (newline) (newline))
    ((category-question id points release history learning-goals stem explanation categories statements)
     (write-category-header points language)
     (write-multi-line (get-localized-text stem language))
     (newline)
     (display "[cols=\"2a,2a,1, 7\", frame=none, grid=none]") (newline)
     (display "|===") (newline) (newline)
     (for-each (lambda (category)
                 (display "| ")
                 (display (get-localized-text (category-text category)
                                              language))
                 (newline))
               categories)
     (display "|") (newline)
     (display "|") (newline)
     (for-each (lambda (statement)
                 (newline)
                 (write-statement statement categories language))
               statements)
     (newline)
     (display "|===") (newline) (newline))
    ))

(define (write-statement statement categories language)
  (for-each (lambda (category)
              (display "| {")
              (display
               (if (equal? category (statement-correct-category statement))
                   "y"
                   "n"))
              (display "}")
              (newline))
            categories)
  (display "| (")
  (display (string-downcase (statement-identifier statement)))
  (display ")") (newline)
  (display "| ")
  (write-bar-line
   (get-localized-text (statement-text statement) language))
  (newline))

(define (write-category-header points language)
  (display "[cols=\"2,8,2\", frame=ends, grid=rows]") (newline)
  (display "|===") (newline)
  (match language
    ("de"
     (display "| K-Frage:") (newline)
     (display "| Bitte ordnen Sie jede Antwort einer Kategorie zu.") (newline)
     (display "| ") (display points) (display " ") (display (points-word/de points)))
    ("en"
     (display "| K-Question:") (newline)
     (display "| Assign all the answers.") (newline)
     (display "| ") (display points) (display " ") (display (points-word/en points))))
  (newline)
  (display "|===") (newline)
  (newline))

(define (write-pick-header pick-options points language)
  (display "[cols=\"2,8,2\", frame=ends, grid=rows]") (newline)
  (display "|===") (newline)
  (define pick-options-count (length pick-options))
  (define correct-pick-options-count (count-correct-pick-options pick-options))

  (match (cons correct-pick-options-count language)
    ((cons 1 "de")
     (display "| A-Frage:") (newline)
     (display "| Bitte kreuzen Sie die richtige Antwort an.") (newline))
    ((cons 1 "en")
     (display "| A-Question:") (newline)
     (display "| Choose one answer.") (newline))
    ((cons _ "de")
     (display "| P-Frage:") (newline)
     (display "| Wählen Sie aus den folgenden ")
     (display (number-word/de pick-options-count))
     (display " Anworten die **")
     (display (number-word/de correct-pick-options-count))
     (display "** Antworten aus, die am besten passen."))
    ((cons _ "en") 
     (display "| P-Question:") (newline)
     (display "| From the following ")
     (display (number-word/en pick-options-count))
     (display " answers select **")
     (display (number-word/en correct-pick-options-count))
     (display "** that fit best.")))
  (newline)
  (write-points points language)
  (display "|===") (newline) (newline))

(define (write-points points language)
  (display "| ") (display points) (display " ")
  (display
   (match (cons points language)
     ((cons 1 "de") "Punkt")
     ((cons 1 "en") "Point")
     ((cons _ "de") "Punkte")
     ((cons _ "en") "Points")))
  (newline))
  
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
  (display "| {")
  (display
   (case (pick-option-validity pick-option)
     ((correct) "y")
     (else "n")))
  (display "}") (newline)
  (display "| (")
  (display (string-downcase (pick-option-identifier pick-option)))
  (display ")") (newline)
  (display "| ")
  (write-bar-line
   (get-localized-text (pick-option-text pick-option) language))
  (newline))

  ;; has to be on line
(define (write-bar-line line)
  (display
   (string-join
    (string-split 
     (string-trim line)
     #rx" *\n *")
    " ")))

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

(define (convert-directory in-directory out-directory languages)
  (let ((paths
         (filter
          (lambda (path) (regexp-match? #rx"\\.xml$" path))
          (directory-list in-directory))))
    (for-each (lambda (path number)
                (let ((question (parse-question-file (build-path in-directory path))))
                  (with-output-to-file
                    (build-path out-directory (path-replace-extension path #".adoc"))
                    (lambda ()
                      (write-question question number languages))
                    #:exists 'replace)))
              paths (map path-extract-number paths))))

(define (main in-directory out-directory . languages)
  (convert-directory in-directory out-directory languages))

; extract question number from path
(define (path-extract-number path)
  (cond
    ((regexp-match #rx"[0-9]+" path) => (lambda (list) (string->number (car list))))
    (else
     (error "unable to extract number from path" path))))

(module+ test
  (require rackunit)
  (require "parse.rkt")

  (define q1
    (pick-question
     "Q-20-04-01"
     1
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
    
    (write-question q1 1 '("de" "en"))
    (write-question q2 2 '("de" "en"))
    (write-question q4 4 '("de" "en")))

(provide (contract-out
          (convert-directory ((or/c path? string?) (or/c path? string?) (listof string?) . -> . any))
          (main ((or/c path? string?) (or/c path? string?) ... string? . -> . any))))
          
