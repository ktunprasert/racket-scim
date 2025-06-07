#lang racket

;;–– for regexp-based keyword stripping
(require racket/string
         json) ; provides jsexpr->string, string->jsexpr, etc. :contentReference[oaicite:0]{index=0}

;;–– Helper: turn a sequence of (sym . val) pairs into a mutable hash
(define (make-scim-object . kvs)
  (define h (make-hash)) ; fresh mutable hash
  (for ([kv kvs])
    (hash-set! h (car kv) (cdr kv))) ; mutate in place
  h) ; return the hash

;;–– scim macro: (scim key1: val1  key2: val2  …)
(define-syntax (scim stx)
  (syntax-case stx ()
    [(_ kv ...)
     (let* ([items (syntax->list #'(kv ...))]
            [pairs (let loop ([rem items]
                              [out '()])
                     (if (null? rem)
                         (reverse out)
                         (let ([k-stx (car rem)]
                               [v-stx (cadr rem)])
                           (unless (and (identifier? k-stx)
                                        (regexp-match #px":$" (symbol->string (syntax->datum k-stx))))
                             (raise-syntax-error 'scim "expected a keyword ending in ‘:’" k-stx))
                           (define bare
                             (regexp-replace #px":$" (symbol->string (syntax->datum k-stx)) ""))
                           (define key-sym (datum->syntax k-stx (string->symbol bare)))
                           (loop (cddr rem) (cons #`(cons ',key-sym #,v-stx) out)))))])
       ;; splice those (cons 'key val) forms into a normal call
       #`(make-scim-object #,@pairs))]))

;;–– Example usage
(define user
  (scim schemas:
        (list "urn:ietf:params:scim:schemas:core:2.0:User")
        id:
        "2819c223-7f76-453a-919d-413861904646"
        userName:
        "bjensen"
        name:
        (scim givenName: "Barbara" familyName: "Jensen")))

;;–– Convert directly to JSON text
(define user-json
  (jsexpr->string user)) ; user is a hash? symbol? jsexpr? :contentReference[oaicite:1]{index=1}

;; (printf "SCIM JSON →~a\n" user-json)
