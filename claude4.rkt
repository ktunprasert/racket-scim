#lang racket

;; SCIM DSL Implementation
;; This provides a clean syntax for defining SCIM resources without arrays

(provide scim-user scim-group scim-resource->json scim-resource->go-struct)

;; Core data structures
(struct scim-resource (type attributes) #:transparent)
(struct scim-attribute (name value type) #:transparent)

;; Macro for defining SCIM users
(define-syntax scim-user
  (syntax-rules (id username email given-name family-name display-name active)
    [(_ (id id-val)
        (username username-val)
        (email email-val)
        (given-name given-val)
        (family-name family-val)
        (display-name display-val)
        (active active-val))
     (scim-resource
      'user
      (list (scim-attribute 'id id-val 'string)
            (scim-attribute 'username username-val 'string)
            (scim-attribute 'email email-val 'string)
            (scim-attribute 'given-name given-val 'string)
            (scim-attribute 'family-name family-val 'string)
            (scim-attribute 'display-name display-val 'string)
            (scim-attribute 'active active-val 'boolean)))]))

;; Macro for defining SCIM groups
(define-syntax scim-group
  (syntax-rules (id display-name description)
    [(_ (id id-val)
        (display-name display-val)
        (description desc-val))
     (scim-resource
      'group
      (list (scim-attribute 'id id-val 'string)
            (scim-attribute 'display-name display-val 'string)
            (scim-attribute 'description desc-val 'string)))]))

;; Convert SCIM resource to JSON string
(define (scim-resource->json resource)
  (define (attr->json attr)
    (match attr
      [(scim-attribute name value 'string)
       (format "\"~a\": \"~a\"" name value)]
      [(scim-attribute name value 'boolean)
       (format "\"~a\": ~a" name (if value "true" "false"))]
      [(scim-attribute name value 'number)
       (format "\"~a\": ~a" name value)]))

  (match resource
    [(scim-resource type attrs)
     (string-append
      "{\n"
      "  \"schemas\": [\"urn:ietf:params:scim:schemas:core:2.0:"
      (symbol->string type) "\"],\n"
      (string-join (map (lambda (attr) (string-append "  " (attr->json attr)))
                        attrs) ",\n")
      "\n}")]))

;; Convert SCIM resource to Go struct format (for code generation)
(define (scim-resource->go-struct resource)
  (define (racket-type->go-type type)
    (match type
      ['string "string"]
      ['boolean "bool"]
      ['number "int64"]))

  (define (attr->go-field attr)
    (match attr
      [(scim-attribute name value type)
       (format "    ~a ~a `json:\"~a\"`"
               (string-titlecase (symbol->string name))
               (racket-type->go-type type)
               name)]))

  (match resource
    [(scim-resource type attrs)
     (string-append
      (format "type ~a struct {\n" (string-titlecase (symbol->string type)))
      "    Schemas []string `json:\"schemas\"`\n"
      (string-join (map attr->go-field attrs) "\n")
      "\n}")]))

;; Helper function to extract values for Go interface
(define (scim-resource->values resource)
  (match resource
    [(scim-resource type attrs)
     (cons type (map scim-attribute-value attrs))]))

;; Usage examples and tests
(module+ main
  ;; Example SCIM user definition
  (define user1
    (scim-user
     (id "123e4567-e89b-12d3-a456-426614174000")
     (username "jdoe")
     (email "john.doe@example.com")
     (given-name "John")
     (family-name "Doe")
     (display-name "John Doe")
     (active #t)))

  ;; Example SCIM group definition
  (define group1
    (scim-group
     (id "456e7890-e89b-12d3-a456-426614174111")
     (display-name "Developers")
     (description "Software development team")))

  ;; Display results
  (displayln "=== User JSON ===")
  (displayln (scim-resource->json user1))

  (displayln "\n=== Group JSON ===")
  (displayln (scim-resource->json group1))

  (displayln "\n=== User Go Struct ===")
  (displayln (scim-resource->go-struct user1))

  (displayln "\n=== Group Go Struct ===")
  (displayln (scim-resource->go-struct group1))

  ;; Values for Go interface
  (displayln "\n=== User Values for Go ===")
  (displayln (scim-resource->values user1)))
