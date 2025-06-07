#lang racket

;; SCIM DSL Parser Implementation
;; Defines a parsable structure and parser for SCIM expressions

(provide parse-scim-expr
         scim-expr?
         scim-user-expr?
         scim-group-expr?
         scim-expr->data
         scim-expr->json
         read-scim-file)

;; AST structures for parsed SCIM expressions
(struct scim-expr (type attributes metadata) #:transparent)
(struct scim-attr (name value type constraints) #:transparent)
(struct scim-metadata (schema-uri created-at modified-at) #:transparent)

;; Predicate functions
(define (scim-user-expr? expr)
  (and (scim-expr? expr) (eq? (scim-expr-type expr) 'user)))

(define (scim-group-expr? expr)
  (and (scim-expr? expr) (eq? (scim-expr-type expr) 'group)))

;; Parser for SCIM expressions
(define (parse-scim-expr expr)
  (match expr
    ;; Parse user expression
    [`(scim-user ,@clauses)
     (let ([attrs (parse-user-clauses clauses)]
           [meta (make-default-metadata 'user)])
       (scim-expr 'user attrs meta))]

    ;; Parse group expression
    [`(scim-group ,@clauses)
     (let ([attrs (parse-group-clauses clauses)]
           [meta (make-default-metadata 'group)])
       (scim-expr 'group attrs meta))]

    ;; Parse resource with explicit metadata
    [`(scim-resource (type ,type-val) (metadata ,@meta-clauses) ,@attr-clauses)
     (let ([attrs (parse-attribute-clauses attr-clauses)]
           [meta (parse-metadata-clauses meta-clauses type-val)])
       (scim-expr type-val attrs meta))]

    [else (error "Invalid SCIM expression:" expr)]))

;; Parse user-specific clauses
(define (parse-user-clauses clauses)
  (map parse-user-clause clauses))

(define (parse-user-clause clause)
  (match clause
    [`(id ,val) (scim-attr 'id val 'string '(required unique))]
    [`(username ,val) (scim-attr 'username val 'string '(required unique))]
    [`(email ,val) (scim-attr 'email val 'string '(required email-format))]
    [`(given-name ,val) (scim-attr 'given-name val 'string '())]
    [`(family-name ,val) (scim-attr 'family-name val 'string '())]
    [`(display-name ,val) (scim-attr 'display-name val 'string '())]
    [`(active ,val) (scim-attr 'active val 'boolean '())]
    [`(password ,val) (scim-attr 'password val 'string '(sensitive write-only))]
    [else (error "Invalid user clause:" clause)]))

;; Parse group-specific clauses
(define (parse-group-clauses clauses)
  (map parse-group-clause clauses))

(define (parse-group-clause clause)
  (match clause
    [`(id ,val) (scim-attr 'id val 'string '(required unique))]
    [`(display-name ,val) (scim-attr 'display-name val 'string '(required))]
    [`(description ,val) (scim-attr 'description val 'string '())]
    [`(external-id ,val) (scim-attr 'external-id val 'string '())]
    [else (error "Invalid group clause:" clause)]))

;; Parse generic attribute clauses
(define (parse-attribute-clauses clauses)
  (map parse-attribute-clause clauses))

(define (parse-attribute-clause clause)
  (match clause
    [`(attr ,name ,val ,type) (scim-attr name val type '())]
    [`(attr ,name ,val ,type ,constraints) (scim-attr name val type constraints)]
    [else (error "Invalid attribute clause:" clause)]))

;; Parse metadata clauses
(define (parse-metadata-clauses clauses type)
  (let ([schema-uri (get-clause-value clauses 'schema-uri (default-schema-uri type))]
        [created-at (get-clause-value clauses 'created-at (current-seconds))]
        [modified-at (get-clause-value clauses 'modified-at (current-seconds))])
    (scim-metadata schema-uri created-at modified-at)))

;; Helper functions
(define (get-clause-value clauses key default)
  (let ([found (assoc key clauses)])
    (if found (cadr found) default)))

(define (default-schema-uri type)
  (format "urn:ietf:params:scim:schemas:core:2.0:~a" type))

(define (make-default-metadata type)
  (scim-metadata (default-schema-uri type) (current-seconds) (current-seconds)))

;; Convert parsed expression to data structure for Go
(define (scim-expr->data expr)
  (match expr
    [(scim-expr type attrs meta)
     `((type . ,type)
       (attributes . ,(map attr->data attrs))
       (metadata . ,(metadata->data meta)))]))

(define (attr->data attr)
  (match attr
    [(scim-attr name value type constraints)
     `((name . ,name)
       (value . ,value)
       (type . ,type)
       (constraints . ,constraints))]))

(define (metadata->data meta)
  (match meta
    [(scim-metadata schema-uri created-at modified-at)
     `((schema-uri . ,schema-uri)
       (created-at . ,created-at)
       (modified-at . ,modified-at))]))

;; Convert to JSON for Go consumption
(define (scim-expr->json expr)
  (define (data->json-string data)
    (cond
      [(string? data) (format "\"~a\"" data)]
      [(number? data) (number->string data)]
      [(boolean? data) (if data "true" "false")]
      [(symbol? data) (format "\"~a\"" data)]
      [(list? data)
       (if (and (pair? data) (pair? (car data)))
           ;; Association list
           (format "{~a}"
                   (string-join
                    (map (lambda (pair)
                           (format "\"~a\": ~a"
                                   (car pair)
                                   (data->json-string (cdr pair))))
                         data) ", "))
           ;; Regular list
           (format "[~a]"
                   (string-join (map data->json-string data) ", ")))]
      [else "null"]))

  (data->json-string (scim-expr->data expr)))

;; Read and parse SCIM file
(define (read-scim-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ([exprs '()])
        (let ([expr (read)])
          (if (eof-object? expr)
              (reverse exprs)
              (loop (cons (parse-scim-expr expr) exprs))))))))

;; Validation functions
(define (validate-scim-expr expr)
  (match expr
    [(scim-expr type attrs meta)
     (and (validate-attributes attrs type)
          (validate-metadata meta))]))

(define (validate-attributes attrs type)
  (define required-attrs
    (case type
      [(user) '(id username)]
      [(group) '(id display-name)]
      [else '()]))

  (let ([attr-names (map scim-attr-name attrs)])
    (andmap (lambda (req) (member req attr-names)) required-attrs)))

(define (validate-metadata meta)
  (match meta
    [(scim-metadata schema-uri created-at modified-at)
     (and (string? schema-uri)
          (number? created-at)
          (number? modified-at))]))

;; Usage examples and tests
(module+ main
  ;; Example 1: Simple user
  (define user-expr
    '(scim-user
      (id "123e4567-e89b-12d3-a456-426614174000")
      (username "jdoe")
      (email "john.doe@example.com")
      (given-name "John")
      (family-name "Doe")
      (display-name "John Doe")
      (active #t)))

  ;; Example 2: Simple group
  (define group-expr
    '(scim-group
      (id "456e7890-e89b-12d3-a456-426614174111")
      (display-name "Developers")
      (description "Software development team")))

  ;; Example 3: Complex resource with metadata
  (define complex-expr
    '(scim-resource
      (type user)
      (metadata
       (schema-uri "urn:ietf:params:scim:schemas:core:2.0:User")
       (created-at 1640995200)
       (modified-at 1640995200))
      (attr id "789" string (required unique))
      (attr username "admin" string (required))
      (attr password "secret123" string (sensitive write-only))))

  ;; Parse expressions
  (define parsed-user (parse-scim-expr user-expr))
  (define parsed-group (parse-scim-expr group-expr))
  (define parsed-complex (parse-scim-expr complex-expr))

  ;; Display results
  (displayln "=== Parsed User ===")
  (pretty-print parsed-user)

  (displayln "\n=== User Data for Go ===")
  (pretty-print (scim-expr->data parsed-user))

  (displayln "\n=== User JSON for Go ===")
  (displayln (scim-expr->json parsed-user))

  (displayln "\n=== Parsed Group ===")
  (pretty-print parsed-group)

  (displayln "\n=== Group JSON for Go ===")
  (displayln (scim-expr->json parsed-group))

  (displayln "\n=== Complex Resource Data ===")
  (pretty-print (scim-expr->data parsed-complex))

  ;; Validation
  (displayln "\n=== Validation Results ===")
  (printf "User valid: ~a\n" (validate-scim-expr parsed-user))
  (printf "Group valid: ~a\n" (validate-scim-expr parsed-group)))
