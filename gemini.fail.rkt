#lang racket

(require json)
(require parser-tools/lex) ; For tokenizing
(require parser-tools/yacc) ; For parsing grammar

;; --- Step 2a & 2b: Define Lexer and Parser for User-Facing DSL ---

;; Lexer definition
;; (alpha (char-set-union (char-set-letter) (char-set-digit) #\.) ) ; Original definition, good
;; More robust alpha that includes common attribute separators like hyphens, underscores too
(define-lex-abbrevs (alpha (char-set-union (char-set-letter) (char-set-digit) #\- #\_ #\.)))

(define scim-lexer
  (lexer
   ;; Skip whitespace
   ["\\s+" (token 'WHITESPACE)] ; Corrected: Use plain string, \\s+ for regex whitespace
   ;; Operators
   ["eq" (token 'EQ)]
   ["ne" (token 'NE)]
   ["co" (token 'CO)]
   ["sw" (token 'SW)]
   ["ew" (token 'EW)]
   ["pr" (token 'PR)]
   ["and" (token 'AND)]
   ["or" (token 'OR)]
   ;; Parentheses
   ["(" (token 'LPAREN)]
   [")" (token 'RPAREN)]
   ;; Corrected String literal (handles escaped quotes and backslashes)
   ;; Regex: Starts with ", then non-" or non-\, OR (\ followed by " or \), repeated. Ends with ".
   ;; Remember: In Racket string literals, \\ means a literal \.
   ["\"(?:[^\"\\\\]|\\\\\"|\\\\\\\\)*\""
    (token 'STRING
           (let ([s (lexer-str)])
             ;; Remove outer quotes
             (define inner-s (substring s 1 (- (string-length s) 1)))
             ;; Unescape escaped quotes and backslashes
             (string-replace (string-replace inner-s "\\\"" "\"") "\\\\" "\\")))]
   ;; Boolean literals
   ["true" (token 'BOOLEAN #t)]
   ["false" (token 'BOOLEAN #f)]
   ;; Attribute names (symbols) - can contain dots, hyphens, underscores
   ;; CORRECTED: Use pregexp s-expression for "one or more"
   ;; [(pregexp (list 'pregexp-+-once alpha)) ; This is the correct way to use pregexp with +
   [(pregexp (list "-" alpha)) ; This is the correct way to use pregexp with +
    (token 'SYMBOL (string->symbol (lexer-str)))]))

;; Parser definition (using YACC-like rules)
(define scim-parser
  (parser (start filter-expression)
          (tokens (EQ NE CO SW EW PR AND OR LPAREN RPAREN STRING BOOLEAN SYMBOL))
          ;; Top-level filter expression
          (grammar (filter-expression [(term AND filter-expression) (list 'and $1 $2)] [term $0])
                   ;; Terms can be 'or' expressions
                   (term [(factor OR term) (list 'or $0 $2)] [factor $0])
                   ;; Factors are comparisons or parenthesized expressions
                   (factor [(SYMBOL EQ STRING) (list 'eq $1 $3)]
                           [(SYMBOL EQ BOOLEAN) (list 'eq $1 $3)]
                           [(SYMBOL NE STRING) (list 'ne $1 $3)]
                           [(SYMBOL NE BOOLEAN) (list 'ne $1 $3)]
                           [(SYMBOL CO STRING) (list 'co $1 $3)]
                           [(SYMBOL SW STRING) (list 'sw $1 $3)]
                           [(SYMBOL EW STRING) (list 'ew $1 $3)]
                           [(SYMBOL PR) (list 'pr $1)]
                           [(LPAREN filter-expression RPAREN) $2]) ; Parenthesized sub-expressions
                   )))

;; --- Step 2c: Serialization Function ---

;; Convert Racket S-expression (list of symbols/strings/booleans) to JSON-compatible data
(define (sexp->json-value sexp)
  (cond
    [(symbol? sexp) (symbol->string sexp)]
    [(string? sexp) sexp]
    [(boolean? sexp) sexp]
    [(number? sexp) sexp]
    [(list? sexp) (map sexp->json-value sexp)]
    [else (error 'sexp->json-value "Unsupported S-expression element: ~a" sexp)]))

;; --- Main Function to Process Input ---

(define (process-scim-filter-string user-input-string)
  ;; Step 2a: Tokenize the input
  (define tokens (scim-lexer user-input-string))
  (displayln (map token-value tokens)) ; Uncomment to see tokens for debugging

  ;; Step 2b: Parse tokens into an S-expression AST
  (define scim-sexp (scim-parser tokens))

  ;; Step 2c: Convert S-expression to JSON-compatible data and then to JSON string
  (define json-data (sexp->json-value scim-sexp))
  (jsexpr->string json-data))

;; --- Example Usage ---

(define user-input-filter "userName eq \"bjensen\" and (title pr or active eq false)")
(define user-input-filter-with-escapes
  "description co \"this is a \\\"test\\\" with\\\\backslashes\"")

(displayln "Processing: userName eq \"bjensen\" and (title pr or active eq false)")
(displayln (process-scim-filter-string user-input-filter))

(displayln "\nProcessing: description co \"this is a \\\"test\\\" with\\\\backslashes\"")
(displayln (process-scim-filter-string user-input-filter-with-escapes))

(displayln "\nProcessing: simple eq \"simple value\"")
(displayln (process-scim-filter-string "simple eq \"simple value\""))

(displayln "\nProcessing: another eq true")
(displayln (process-scim-filter-string "another eq true"))
