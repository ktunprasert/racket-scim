#lang racket

;; SCIM Filter Expression Parser
;; Parses SCIM filter queries like: userName eq "john" and active eq true

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(provide parse-scim-filter
         scim-filter->data
         scim-filter->json
         scim-filter->go-query)

;; AST structures for SCIM filter expressions
(struct scim-logical-expr (op left right) #:transparent)
(struct scim-comparison-expr (attr op value) #:transparent)
(struct scim-group-expr (expr) #:transparent)
(struct scim-not-expr (expr) #:transparent)
(struct scim-present-expr (attr) #:transparent)

;; Attribute path structure
(struct scim-attr-path (name sub-attr filter) #:transparent)

;; Tokens
(define-tokens value-tokens (IDENTIFIER STRING NUMBER BOOLEAN DATETIME))
(define-empty-tokens op-tokens
                     ;; Comparison operators
                     (EQ NE
                         GT
                         GE
                         LT
                         LE
                         CO
                         SW
                         EW
                         PR
                         ;; Logical operators
                         AND
                         OR
                         NOT
                         ;; Grouping
                         LPAREN
                         RPAREN
                         LBRACKET
                         RBRACKET
                         ;; Path separator
                         DOT
                         EOF))

;; Lexer for SCIM filter expressions
(define scim-filter-lexer
  ;; Skip whitespace
  (lexer [(:or #\space #\tab #\newline #\return) (scim-filter-lexer input-port)]
         ;; Comparison operators (case insensitive)
         [(:or "eq" "EQ") (token-EQ)]
         [(:or "ne" "NE") (token-NE)]
         [(:or "gt" "GT") (token-GT)]
         [(:or "ge" "GE") (token-GE)]
         [(:or "lt" "LT") (token-LT)]
         [(:or "le" "LE") (token-LE)]
         [(:or "co" "CO") (token-CO)] ; contains
         [(:or "sw" "SW") (token-SW)] ; starts with
         [(:or "ew" "EW") (token-EW)] ; ends with
         [(:or "pr" "PR") (token-PR)] ; present
         ;; Logical operators
         [(:or "and" "AND") (token-AND)]
         [(:or "or" "OR") (token-OR)]
         [(:or "not" "NOT") (token-NOT)]
         ;; Grouping
         ["(" (token-LPAREN)]
         [")" (token-RPAREN)]
         ["[" (token-LBRACKET)]
         ["]" (token-RBRACKET)]
         ;; Path separator
         ["." (token-DOT)]
         ;; Strings (quoted)
         [(:seq #\" (:* (:or (:seq #\\ any-char) (:~ #\"))) #\")
          (token-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
         ;; DateTime (ISO 8601 format)
         [(:seq (:+ numeric)
                #\-
                (:+ numeric)
                #\-
                (:+ numeric)
                #\T
                (:+ numeric)
                #\:
                (:+ numeric)
                #\:
                (:+ numeric)
                (:? (:seq #\. (:+ numeric)))
                (:? #\Z))
          (token-DATETIME lexeme)]
         ;; Booleans
         [(:or "true" "false" "TRUE" "FALSE")
          (token-BOOLEAN (member (string-downcase lexeme) '("true")))]
         ;; Numbers
         [(:seq (:? (:or #\+ #\-)) (:+ numeric) (:? (:seq #\. (:+ numeric))))
          (token-NUMBER (string->number lexeme))]
         ;; Identifiers (attribute names)
         [(:seq (:or alphabetic #\_) (:* (:or alphabetic numeric #\_ #\-)))
          (token-IDENTIFIER (string->symbol lexeme))]
         ;; EOF
         [(eof) (token-EOF)]))

;; Parser for SCIM filter expressions
(define scim-filter-parser
  (parser (start filter-expression)
          (end EOF)
          (tokens value-tokens op-tokens)
          (error (lambda (tok-ok? tok-name tok-value)
                   (error (format "SCIM filter parse error: unexpected ~a (~a)" tok-name tok-value))))
          ;; Operator precedence (lowest to highest)
          (precs (left OR) (left AND) (right NOT) (left EQ NE GT GE LT LE CO SW EW PR))
          (grammar
           ;; Main filter expression
           (filter-expression [(or-expression) $1])
           ;; Logical OR (lowest precedence)
           (or-expression [(and-expression) $1]
                          [(or-expression OR and-expression) (scim-logical-expr 'or $1 $3)])
           ;; Logical AND
           (and-expression [(not-expression) $1]
                           [(and-expression AND not-expression) (scim-logical-expr 'and $1 $3)])
           ;; Logical NOT
           (not-expression [(comparison-expression) $1]
                           [(NOT comparison-expression) (scim-not-expr $2)])
           ;; Comparison expressions
           (comparison-expression [(attr-path EQ value) (scim-comparison-expr $1 'eq $3)]
                                  [(attr-path NE value) (scim-comparison-expr $1 'ne $3)]
                                  [(attr-path GT value) (scim-comparison-expr $1 'gt $3)]
                                  [(attr-path GE value) (scim-comparison-expr $1 'ge $3)]
                                  [(attr-path LT value) (scim-comparison-expr $1 'lt $3)]
                                  [(attr-path LE value) (scim-comparison-expr $1 'le $3)]
                                  [(attr-path CO value) (scim-comparison-expr $1 'co $3)]
                                  [(attr-path SW value) (scim-comparison-expr $1 'sw $3)]
                                  [(attr-path EW value) (scim-comparison-expr $1 'ew $3)]
                                  [(attr-path PR) (scim-present-expr $1)]
                                  [(LPAREN filter-expression RPAREN) (scim-group-expr $2)])
           ;; Attribute paths (e.g., userName, emails.value, meta.created)
           (attr-path [(IDENTIFIER) (scim-attr-path $1 #f #f)]
                      [(IDENTIFIER DOT IDENTIFIER) (scim-attr-path $1 $3 #f)]
                      [(IDENTIFIER LBRACKET filter-expression RBRACKET) (scim-attr-path $1 #f $3)]
                      [(IDENTIFIER DOT IDENTIFIER LBRACKET filter-expression RBRACKET)
                       (scim-attr-path $1 $3 $5)])
           ;; Values
           (value [(STRING) $1] [(NUMBER) $1] [(BOOLEAN) $1] [(DATETIME) $1]))))

;; Main parsing function
(define (parse-scim-filter filter-string)
  (let ([in (open-input-string filter-string)])
    (scim-filter-parser (lambda () (scim-filter-lexer in)))))

;; Convert AST to data structure for Go
(define (scim-filter->data filter)
  (match filter
    [(scim-logical-expr op left right)
     `((type . "logical") (operator . ,op)
                          (left . ,(scim-filter->data left))
                          (right . ,(scim-filter->data right)))]

    [(scim-comparison-expr attr op value)
     `((type . "comparison") (attribute . ,(attr-path->data attr)) (operator . ,op) (value . ,value))]

    [(scim-present-expr attr) `((type . "present") (attribute . ,(attr-path->data attr)))]

    [(scim-not-expr expr) `((type . "not") (expression . ,(scim-filter->data expr)))]

    [(scim-group-expr expr) `((type . "group") (expression . ,(scim-filter->data expr)))]))

(define (attr-path->data attr-path)
  (match attr-path
    [(scim-attr-path name sub-attr filter)
     `((name . ,(symbol->string name)) (sub-attribute . ,(if sub-attr
                                                             (symbol->string sub-attr)
                                                             null))
                                       (filter . ,(if filter
                                                      (scim-filter->data filter)
                                                      null)))]))

;; Convert to JSON
(define (scim-filter->json filter)
  (define (data->json data)
    (cond
      [(null? data) "null"]
      [(string? data) (format "\"~a\"" data)]
      [(number? data) (number->string data)]
      [(boolean? data) (if data "true" "false")]
      [(symbol? data) (format "\"~a\"" data)]
      [(list? data)
       (if (and (pair? data) (pair? (car data)))
           ;; Object
           (format "{~a}"
                   (string-join (map (lambda (p) (format "\"~a\":~a" (car p) (data->json (cdr p))))
                                     data)
                                ","))
           ;; Array
           (format "[~a]" (string-join (map data->json data) ",")))]))

  (data->json (scim-filter->data filter)))

;; Convert to Go query structure (example)
(define (scim-filter->go-query filter)
  (match filter
    [(scim-comparison-expr (scim-attr-path name sub #f) op value)
     (format "query.Where(\"%s %s ?\", \"%s\", %s)"
             (if sub
                 (format "%s.%s" name sub)
                 (symbol->string name))
             (op->sql op)
             (value->go-literal value))]

    [(scim-logical-expr 'and left right)
     (format "(%s) AND (%s)" (scim-filter->go-query left) (scim-filter->go-query right))]

    [(scim-logical-expr 'or left right)
     (format "(%s) OR (%s)" (scim-filter->go-query left) (scim-filter->go-query right))]

    [else "/* complex query */"]))

(define (op->sql op)
  (case op
    [(eq) "="]
    [(ne) "!="]
    [(gt) ">"]
    [(ge) ">="]
    [(lt) "<"]
    [(le) "<="]
    [(co) "LIKE"]
    [(sw) "LIKE"]
    [(ew) "LIKE"]
    [else "="]))

(define (value->go-literal value)
  (cond
    [(string? value) (format "\"%s\"" value)]
    [(number? value) (number->string value)]
    [(boolean? value) (if value "true" "false")]
    [else "null"]))

;; Usage examples
(module+ main
  ;; Test various SCIM filter expressions
  (define test-filters
    '("userName eq \"john.doe\"" "userName eq \"john\" and active eq true"
                                 "emails.value co \"@example.com\""
                                 "meta.created gt \"2011-05-13T04:42:34Z\""
                                 "userName sw \"john\" or familyName ew \"doe\""
                                 "(userName eq \"john\" or userName eq \"jane\") and active eq true"
                                 "emails[type eq \"work\"].value co \"@corp.com\""
                                 "addresses.locality pr"
                                 "not (userName eq \"admin\")"))

  (displayln "=== SCIM Filter Expression Parser ===\n")

  (for ([filter-str test-filters])
    (displayln (format "Filter: ~a" filter-str))
    (define parsed (parse-scim-filter filter-str))
    (displayln (format "AST: ~a" parsed))
    (displayln (format "JSON: ~a" (scim-filter->json parsed)))
    (displayln "---\n")
    ;; (displayln (format "Go Query: ~a" (scim-filter->go-query parsed)))
    ))
;; (displayln "---")))
