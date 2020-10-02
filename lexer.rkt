#lang racket

;Aiden perera
;16926800

(define KEYWORDS '("KEYWORD_LET" "if" "else" "true" "false" "KEYWORD_FN" "L_PAREN" "R_PAREN" "L_CURL" "R_CURL" "COMMA" "R_SQRBRACE" "L_SQRBRACE" "KEYWORD_RETURN" "KEYWORD_IF" "KEYWORD_ELSE"
                                 "OPT_ASSIGN" "OPT_ADD" "OPT_SUB" "OPT_DIV" "OPT_MULT" "OPT_EQUALITY" "BOOL(false)" "BOOL(true)"))
;::::helper functions::::
(define (str-clean src remov) ;function to remove already parsed source code
  (string-trim (string-replace src remov "")))

(define (str-append str append end) ;string concat wrapper function
  (define x (list append end))
  (string-append* str x))

(define (dup-rem lst) ;function to convert list into a unique list
  (cond
    [(empty? lst) empty]
    [else (cons (first lst) (dup-rem (filter (lambda (x) (not (equal? (first lst) x))) lst)))]))

(define (file-reader file-path) ;function to handle reading file
  (define src "")
  (define currentline "")
  (with-input-from-file file-path
    (thunk
     (for ([line (in-lines)])
       (set! currentline (string-replace line #px"//.*$" "")); handle removing comments.       
       (set! src(str-append src currentline "")))))
  (set! src (regexp-replace* #px"\\s{2,}" src " "))src); handle removing unwanted white space.


(define (set-tokens stmt out); handles setting tokens

  ; using regex, replaces certain symbols/keywords with tokens
  (set! stmt (regexp-replace* #px"==(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " OPT_EQUALITY "))
  (set! stmt (regexp-replace* #px"if(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " KEYWORD_IF "))
  (set! stmt (regexp-replace* #px"else(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " KEYWORD_ELSE "))
  (set! stmt (regexp-replace* #px"=(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " OPT_ASSIGN "))
  (set! stmt (regexp-replace* #px"\\+(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " OPT_ADD "))
  (set! stmt (regexp-replace* #px"-(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " OPT_SUB "))
  (set! stmt (regexp-replace* #px"\\*(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " OPT_MULT "))
  (set! stmt (regexp-replace* #px"/(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " OPT_DIV "))
  (set! stmt (regexp-replace* #px",(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " COMMA "))
  (set! stmt (regexp-replace* #px"\\((?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " L_PAREN "))
  (set! stmt (regexp-replace* #px"\\)(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " R_PAREN "))
  (set! stmt (regexp-replace* #px"let(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " KEYWORD_LET "))
  (set! stmt (regexp-replace* #px"fn(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " KEYWORD_FN "))
  (set! stmt (regexp-replace* #px"return(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " KEYWORD_RETURN "))
  (set! stmt (regexp-replace* #px"\\{(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " L_CURL "))
  (set! stmt (regexp-replace* #px"\\}(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " R_CURL "))
  (set! stmt (regexp-replace* #px"\\[(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " L_SQRBRACE "))
  (set! stmt (regexp-replace* #px"\\](?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " R_SQRBRACE "))

  ;grab all the possible ids and removes them if it matches a defined keyword/symbol
  (define posibleids (regexp-match* #px"[a-zA-Z_]+[a-zA-Z_0-9]*(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt))
  (for/list ([n posibleids])
    (for/list ([key KEYWORDS])
              (cond[(equal? n key)(set! posibleids (remove key posibleids))])))
  (set! posibleids (dup-rem posibleids))
  (for/list ([id posibleids])
    (set! stmt(regexp-replace* (str-append "[^a-zA-Z0-9_]" id "(?=([^\"]*\"[^\"]*\")*[^\"]*$)[^a-zA-Z0-9_]") stmt (str-append " IDENTIFIER(" id ") "))))

  ;sets tokens for strings, bools and numerics
  (define strings (dup-rem(regexp-match* #px"\".*?\"" stmt)))
  (define nums (dup-rem(regexp-match* #px"[^a-zA-Z_]\\d+(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt)))

  (for/list([n strings])
    (set! stmt (regexp-replace* n stmt (str-append " STRING(" n ")"))))
  (for/list([n nums])
    (set! stmt (regexp-replace* n stmt (str-append " NUM(" n ") "))))

  (set! stmt (regexp-replace* #px"\\s(?=\\d+)" stmt ""))
  (set! stmt (regexp-replace* #px"true(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " BOOL(true) "))
  (set! stmt (regexp-replace* #px"false(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " BOOL(false) "))

  (set! stmt (regexp-replace* #px"\\;(?=([^\"]*\"[^\"]*\")*[^\"]*$)" stmt " SEMI"))
  (set! stmt (string-replace stmt #px"\\s{2,}" " "))
  
  (displayln stmt out);writes statement to file
  (displayln stmt))
(define (validate-stmt src currentstmt out); using regex validates all possible statements (recursive function).
  (define VALID_LET_STMT (regexp-match #px"(let\\s*[a-zA-Z_]+[a-zA-Z0-9_]*\\s*=\\s*(\\[\\s*((?<!\")\"\\s*.+?(?=\")\\s*\"\\s*:\\s*((true|false|(?<!\")\"\\s*.+?(?=\")\\s*\"|(\\d+|(?:[0-9 ()]+[*+\\/-]*)+[0-9 ()]+))\\s*,*\\s*))+\\]\\s*;|false\\s*;|true\\s*;|\\d+;|(\\d+|(?:[0-9a-zA-Z_ ()]+[*+\\/-]*)+[0-9a-zA-Z_()]+)\\s*;|(?<!\")\"\\s*.+?(?=\")\\s*\"\\s*;|fn\\s*\\((\\s*([a-zA-Z0-9]+[a-zA-Z0-9_]*|true|false|\\d+|(?<!\")\"\\s*.+?(?=\")\\s*\")\\s*,*)*\\s*\\)\\s*\\{.*\\}\\s*;*|\\[(\\s*([a-zA-Z0-9]+[a-zA-Z0-9_]*|true|false|\\d+|(?<!\")\"\\s*.+?(?=\")\\s*\")\\s*,*)+\\s*\\]\\s*;)|\\s*[a-zA-Z_]+[a-zA-Z0-9_]*\\s*\\((\\s*([a-zA-Z0-9]+[a-zA-Z0-9_]*|true|false|\\d+|(?<!\")\"\\s*.+?(?=\")\\s*\")\\s*,*)*\\s*\\)\\s*;|[a-zA-Z_]+[a-zA-Z0-9]*\\s*\\[\\s*(?<!\")\"\\s*.+?(?=\")\\s*\"\\s*\\]\\s*;*)" src))
  (cond[(not(equal? VALID_LET_STMT false))
          (set! currentstmt (first VALID_LET_STMT))
          (set! src (string-replace src currentstmt ""))
               (set! src (str-clean src (first VALID_LET_STMT)))
               (set-tokens currentstmt out)
               (validate-stmt src currentstmt out)]
    [(equal? (non-empty-string? src) true)(str-append "error at > " src ".")]; if regex returns false and the source file is not fully read, then error has occured.
    [else "lexer completed..."]))

(define (lexer sourcefile outfile)
  (define src (file-reader sourcefile))
  (define out (open-output-file outfile))
  (validate-stmt src "" out)
  (close-output-port out)
  )

(lexer "mlang.src.rkt" "lexer.out.txt")