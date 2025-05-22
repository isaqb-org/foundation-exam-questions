#lang racket/base

; Parse schema-conformant exam questions from XML

(require racket/match; racket/date
         xml
         (only-in racket/string string-trim)
         racket/contract)
(require "question.rkt"
         "match-xml.rkt"
         "date.rkt")

(struct exn:parse-error exn:fail
  (base-exn filename))

(define (parse-question-file filename)
  (with-handlers
      ((exn:fail? (lambda (exn)
                    (raise (exn:parse-error (string-append "parse error in file " (path->string (build-path filename)) ": " (exn-message exn))
                                            (exn-continuation-marks exn)
                                            exn filename)))))
    (parse-question-document (call-with-input-file filename read-xml))))

(define structure-tags
  '(pickQuestion
    categoryQuestion
    history lg stem pickOptions option explanation
    categoryStatements categories category statements statement))

(define cleanup
  (eliminate-whitespace structure-tags values)) 

(define (parse-question-document xml)
  (match xml
    ((document _ element _)
     (parse-question (cleanup element)))))

(define (parse-question xml)
  (match xml
    ((element* pickQuestion
               ((id id)
                (points points))
               ((history history)
                (refersToLgs refers-to-lgs)
                (stem stem)
                (pickOptions (element* pickOptions () (((option) pick-options))))
                (explanation explanation)))
     (pick-question id
                    (string->number points)
                    (parse-history history)
                    (parse-refers-to-lgs refers-to-lgs)
                    (parse-stem stem)
                    (parse-explanation explanation)
                    (map parse-option pick-options)))
    ((element* categoryQuestion
               ((id id)
                (points points))
               ((history history)
                (refersToLgs refers-to-lgs)
                (stem stem)
                (categoryStatements
                 (element* categoryStatements
                           ()
                           ((categories (element* categories
                                                  ()
                                                  (((category) categories-xml))))
                            (statements (element* statements
                                                  ()
                                                  (((statement) statements)))))))
                (explanation explanation)))
     (define categories (map parse-category categories-xml))
     (category-question id
                        (string->number points)
                        (parse-history history)
                        (parse-refers-to-lgs refers-to-lgs)
                        (parse-stem stem)
                        (parse-explanation explanation)
                        categories
                        (map (lambda (xml) (parse-statement xml categories))
                             statements)))))

(define (parse-history xml)
  (match xml
    ((element* history
               ()
               (((item) items)))
     (map parse-item items))))

(define (parse-item xml)
  (match xml
    ((element* item
               ((date date))
               text)
     (history-item (parse-date date)
                   (string-trim text)))))

(define (parse-refers-to-lgs xml)
  (match xml
    ((element* refersToLgs
               ()
               (((lg) lgs)))
     (map parse-lg lgs))))

(define (parse-lg xml)
  (match xml
    ((element* lg
               ((curriculumVersion (regexp #rx"([0-9]+)\\.([0-9]+)"
                                           (list _
                                                 curriculum-year
                                                 curriculum-number)))
                (lg lg-string))
               ())
     (learning-goal-reference (curriculum-version (string->number curriculum-year)
                                                  (string->number curriculum-number))
                                                     
                              (cond
                                ((regexp-match #rx"([0-9]+)-([0-9]+)" lg-string)
                                 => (lambda (list)
                                      (lg-number (string->number (cadr list))
                                                 (string->number (caddr list)))))
                                ((string=? lg-string "prerequisite")
                                 'prerequisite))))))

(define (parse-stem xml)
  (match xml
    ((element* stem
               ()
               (((text) texts)))
     (parse-texts texts))))

(define (parse-texts xmls)
  (localized-text (map parse-text xmls)))

(define (parse-text xml)
  (match xml
    ((element* text
               ((xml:lang lang))
               text)
     (cons lang (string-trim text)))))
               

(define (parse-explanation xml)
  (and xml
       (match xml
         ((element* explanation
                    ()
                    (((text) texts)))
          (parse-texts texts)))))

(define (parse-option xml)
  (match xml
    ((element* option
               ((identifier identifier)
                (correct correct-value)
                (false false-value)
                (distractor distractor-value))
               (((text) texts)))
     (pick-option
      (parse-option-validity correct-value false-value distractor-value)
      identifier
      (parse-texts texts)))))

(define (parse-option-validity correct-value false-value distractor-value)
  (cond
    ((equal? correct-value "correct") 'correct)
    ((equal? false-value "false") 'false)
    ((equal? distractor-value "distractor") 'distractor)))

(define (parse-category xml)
  (match xml
    ((element* category
               ((label label))
               (((text) texts)))
     (category label
               (parse-texts texts)))))

(define (parse-statement xml categories)
  (match xml
    ((element* statement
               ((correctCategory correct-category)
                (identifier identifier))
               (((text) texts)))
     (statement (find-category correct-category categories) identifier
                (parse-texts texts)))))

(define (find-category label categories)
  (or (findf (lambda (category)
               (equal? label (category-label category)))
             categories)
      (error 'find-category "category label not found")))

(module+ test
  (require rackunit)

  (check-equal?
   (parse-question-document (call-with-input-file "../../mock/questions/mock-01.xml" read-xml))
   (pick-question
    "Q-20-04-01"
    1
    (list
     (history-item
      (date 0 0 0 8 8 2024 4 220 #f 0)
      "content identical to mock exam 2024.1-rev0-DE-20240202"))
    (list
     (learning-goal-reference (curriculum-version 2025 1) (lg-number 1 1))
     (learning-goal-reference (curriculum-version 2023 1) (lg-number 1 1)))
    (localized-text
     '(("de"
        .
        "Wie viele Definitionen des Begriffes \"Softwarearchitektur\" gibt es?")
       ("en" . "How many definitions of “software architecture” exist?")))
    (localized-text
     '(("de"
        .
        "Die Vielfalt der Definitionen von Softwarearchitektur resultiert unter anderem aus unterschiedlichen Perspektiven, Zielgruppen und Entwicklungsmethoden.")
       ("en"
        .
        "The variety of definitions of software architecture results, among other things, from different perspectives, target groups and development methods.")))
    (list
     (pick-option
      'distractor
      "A"
      (localized-text
       '(("de" . "Genau eine für alle Arten von Systemen.")
         ("en" . "Exactly one for all kinds of systems."))))
     (pick-option
      'false
      "B"
      (localized-text
       '(("de"
          .
          "Eine für jede Art von Softwaresystem (z. B. \"eingebettet\",\"Echtzeit\", \"Entscheidungsunterstützung\", \"Web\", \"Batch\", …).")
         ("en"
          .
          "One for every kind of software system (e.g. “embedded”, “real-time”, “decision support”, “web”, “batch”, …)."))))
     (pick-option
      'correct
      "C"
      (localized-text
       '(("de" . "Ein Dutzend oder mehr unterschiedliche Definitionen.")
         ("en" . "A dozen ore more different definitions.")))))))


  (check-equal?
   (parse-question-document (call-with-input-file "../../mock/questions/mock-04.xml" read-xml))
   (category-question
    "Q-17-13-02"
    2
    (list
     (history-item
      (date 0 0 0 8 8 2024 4 220 #f 0)
      "content identical to mock exam 2024.1-rev0-DE-20240202"))
    (list
     (learning-goal-reference (curriculum-version 2025 1) (lg-number 4 2))
     (learning-goal-reference (curriculum-version 2023 1) (lg-number 3 2)))
    (localized-text
     '(("de"
        .
        "Bei Ihrem Projekt arbeiten drei Architekt:innen und sieben\n      Entwickler:innen an der Dokumentation der\n      Softwarearchitektur. Welche Methoden eignen sich zur\n      Gewährleistung einer konsistenten und zweckmäßigen Dokumentation\n      und welche nicht?")
       ("en"
        .
        "In your project, three architects and seven developers are working on the\n      documentation of the software architecture. Which methods are\n      appropriate in order to achieve a consistent and adequate\n      documentation, and which are not?")))
    (localized-text
     '(("de"
        .
        "Um eine konsistente und zweckmäßige Dokumentation sicherzustellen, eignet sich die Koordination durch die leitende Architekt:in sowie die Verwendung identischer Vorlagen. Die automatische Extraktion aller Teile der Dokumentation aus dem Quellcode ist weniger empfehlenswert, da dies möglicherweise nicht alle relevanten Informationen oder Kontexte berücksichtigt und die Qualität der Dokumentation beeinträchtigen kann.\n      Aspekte wie Begründungen oder Alternativen sind nicht im Code enthalten, sondern müssen in die Dokumentation aufgenommen werden, daher können nicht alle Teile der Dokumentation aus dem Quellcode extrahiert werden.")
       ("en"
        .
        "To ensure consistent and useful documentation, coordination by the lead architect and the use of identical templates are recommended. Automatic extraction of all parts of the documentation from the source code is less recommended, as this may not include all relevant information or contexts and may affect the quality of the documentation.\n      Things like reasoning or alternatives won’t be contained in code, but need to be included in documentation, therefore not all parts of documentation can be extracted from source code.")))
    (list
     (category "a" (localized-text '(("de" . "Geeignet") ("en" . "appropriate"))))
     (category "b" (localized-text '(("de" . "Nicht geeignet") ("en" . "not appropriate")))))
    (list
     (statement
      (category "a" (localized-text '(("de" . "Geeignet") ("en" . "appropriate"))))
      "A"
      (localized-text
       '(("de"
          .
          "Die/der leitende Architekt:in koordiniert die Erstellung der Dokumentation.")
         ("en"
          .
          "The lead architect coordinates the creation of the documentation."))))
     (statement
      (category "a" (localized-text '(("de" . "Geeignet") ("en" . "appropriate"))))
      "B"
      (localized-text
       '(("de"
          .
          "Für die Dokumentation werden identische Vorlagen verwendet.")
         ("en"
          .
          "Identical templates are used for the documentation."))))
     (statement
      (category "b" (localized-text '(("de" . "Nicht geeignet") ("en" . "not appropriate"))))
      "C"
      (localized-text
       '(("de"
          .
          "Alle Teile der Dokumentation werden automatisch aus dem Quellcode extrahiert.")
         ("en"
          .
          "All parts of the documentation are automatically extracted from the source code.")))))))
  
   )
  
(provide (contract-out
          (parse-question-file ((or/c path? string?) . -> . question?))
          (parse-question-document (document? . -> . question?))))
