#lang racket/base
(require racket/match
         (for-syntax racket/base)
         xml)

(provide element*)

(define-match-expander element*
  (lambda (stx)
    (syntax-case stx ()
      ((_ name
          ((attribute-name attribute-pattern) ...)
          ((element-tag element-pattern) ...))
       #'(element _ _ 'name
                  (app (find-attribute-values '(attribute-name ...))
                       attribute-pattern ...)
                  (app (find-subelements '(element-tag ...))
                       element-pattern ...)))
       ((_ name
          ((attribute-name attribute-pattern) ...)
          text-name)
        #'(element _ _ 'name
                  (app (find-attribute-values '(attribute-name ...))
                       attribute-pattern ...)
                  (list (pcdata _ _ text-name)))))))

(define ((find-attribute-value attributes) name)
  (cond
    ((findf (lambda (attribute)
              (eq? name (attribute-name attribute)))
            attributes)
     => attribute-value)
    (else #f)))

(define ((find-attribute-values names) attributes)
  (apply values
         (map (find-attribute-value attributes)
              names)))

(define ((find-element elements) tag)
  (let-values (((tag process)
                (cond
                  ((symbol? tag) (values tag findf))
                  ((list? tag) (values (car tag) filter)))))
    (process
     (lambda (element)
       (and (element? element)
            (eq? tag (element-name element))))
     elements)))

(define ((find-subelements tags) content)
  (apply values
         (map (find-element content)
              tags)))
