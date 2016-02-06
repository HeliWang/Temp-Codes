#lang racket

;; scan is the main function provided, which uses the data definitions
;; and helper functions that follow. Sample tests are at the bottom of the file.
(define (scan str)
  (scan-func str asmtrlst 'start asmfinal))

;; scan-func: (listof char) trans-table symbol (listof symbol) -> (listof token)

(define (scan-func str trans start final)
  (scan-acc (string->list str) trans start final empty empty))

;; Next we specify the data definitions for tokens and the various components
;; of an FSM.

(define-struct token (kind lexeme) #:transparent)

;; A token is a (make-token k l), where k is a symbol
;;  and l is (union (list char) int).

(define-struct transition (state charset next) #:transparent)

;; A transition table is a list of transitions.
;; A transition is a (make-transition s cs ns), where s and ns are symbols,
;;  and cs is a function char->boolean that tests whether the transition applies.

;; The sample FSM provided is defined by (asmtrlst, 'start, asmfinal).
;; Definitions of asmtrlst and asmfinal follow.

;; functions used in defining smple transition table

(define (one-to-nine? ch)
  (and (char<=? #\1 ch) (char<=? ch #\9)))

(define (hex-digit? ch)
  (or
   (char-numeric? ch)
   (and (char<=? #\a ch) (char<=? ch #\f))
   (and (char<=? #\A ch) (char<=? ch #\F))))

(define (chartest ch)
  (lambda (x) (char=? x ch)))

;; sample transition table

(define asmtrlst
  (list
   (make-transition 'start char-whitespace? 'whitespace)
   (make-transition 'start char-alphabetic? 'id)
   (make-transition 'id char-alphabetic? 'id)
   (make-transition 'id char-numeric? 'id)
   (make-transition 'start one-to-nine? 'int)
   (make-transition 'int char-numeric? 'int)
   (make-transition 'start (chartest #\-) 'minus)
   (make-transition 'minus char-numeric? 'int)
   (make-transition 'start (chartest #\,) 'comma)
   (make-transition 'start (chartest #\() 'lparen)
   (make-transition 'start (chartest #\)) 'rparen)
   (make-transition 'start (chartest #\$) 'dollar)
   (make-transition 'dollar char-numeric? 'register)
   (make-transition 'register char-numeric? 'register)
   (make-transition 'start (chartest #\0) 'zero)
   (make-transition 'zero (chartest #\x) 'zerox)
   (make-transition 'zero char-numeric? 'int)
   (make-transition 'zerox hex-digit? 'hexint)
   (make-transition 'hexint hex-digit? 'hexint)
   (make-transition 'id (chartest #\:) 'label)
   (make-transition 'start (chartest #\;) 'comment)
   (make-transition 'comment (lambda (x) true) 'comment)
   (make-transition 'start (chartest #\.) 'dot)
   (make-transition 'dot (chartest #\w) 'dotw)
   (make-transition 'dotw (chartest #\o) 'dotwo)
   (make-transition 'dotwo (chartest #\r) 'dotwor)
   (make-transition 'dotwor (chartest #\d) 'dotword)
   ))

;; sample list of final states

(define asmfinal
  (list
    'register
    'int
    'id
    'label
    'comma
    'lparen
    'rparen
    'zero
    'hexint
    'comment
    'dotword
    'whitespace
    ))

;; scan-acc is the main workhorse of the lexer. It uses accumulative recursion
;; to run the FSM specified by (trans, state, final) on the list of characters cl.
;; acc accumulates the characters of the current token in reverse order, and
;; tacc accumulates the token list in reverse order.

;; scan-acc: (listof char) trans-table symbol (listof symbol) (listof char) (listof token) -> (listof token)

(define (scan-acc cl trans state final acc tacc)
  (cond
    [(empty? cl)
       (if (member state final)
           (if (or (symbol=? state 'whitespace) (symbol=? state 'comment))
               (reverse tacc)
               (reverse (cons (finalize-token state (reverse acc)) tacc)))
           (error 'ERROR "unexpected end of string\n"))]
    [else
      (let ([trl (memf (lambda (x) (found-trans? state (first cl) x)) trans)])
        (cond
          [(and (boolean? trl) (member state final))
             (if (symbol=? state 'whitespace)
                 (scan-acc cl trans 'start final empty tacc)
                 (scan-acc cl trans 'start final empty (cons (finalize-token state (reverse acc)) tacc)))]
          [(boolean? trl)
             (error 'ERROR "left to parse:~a ~a\n" state (list->string cl))]
          [(symbol=? state 'comment)
             (reverse tacc)]
          [else
             (scan-acc (rest cl) trans (transition-next (first trl)) final (cons (first cl) acc) tacc)]))]))

;; helper functions for scan-acc

(define (found-trans? state ch tr)
  (and (symbol=? state (transition-state tr))
       ((transition-charset tr) ch)))

;; finalize-token symbol (listof char) -> token
(define (finalize-token state l)
  (cond
    [(symbol=? state 'int) (make-token 'int (check-int-range (list->number l)))]
    [(symbol=? state 'zero) (make-token 'int 0)]
    [(symbol=? state 'hexint) (make-token 'hexint (check-hexint-range (list->hexint (rest (rest l)))))]
    [(symbol=? state 'register) (make-token 'register (check-reg-range (list->number (rest l))))]
    [else (make-token state l)]))

;; helper functions for finalize-token

(define (list->number lst) (string->number (list->string lst)))

(define (list->hexint lst) (string->number (list->string lst) 16))

;; Scheme supports unbounded integers but MIPS doesn't
(define (check-int-range n)
  (cond
    [(<= -2147483648 n 4294967295) n]
    [else (error 'ERROR "integer out of range: ~a" n)]))

(define (check-hexint-range n)
  (cond
    [(<= 0 n 4294967295) n]
    [else (error 'ERROR "integer out of range: ~a" n)]))

(define (check-reg-range n)
  (cond
    [(<= 0 n 31) n]
    [else (error 'ERROR "register out of range: ~a" n)]))


(define label-ht (make-hash)) ;store the labels


; This file just uses scan to tokenize each line of the input
(define (scan-input)
  (define line (read-line))
  (cond
    [(eof-object? line) empty]
    [(> (string-length line) 0) ; Ignore blank lines
        ; Ignore comment-only lines as well
        ; When a comment-only line is scanned, it is omitted
        (define scanned (scan line))
        (cond
          [(empty? scanned) (scan-input)]
          ;[else (printf "~a~n" scanned)(scan-input)])]
          [else (cons scanned (scan-input))])] 
    [else (scan-input)]))

(define (output token-lexeme-value)
  (cond [(= token-lexeme-value -2) (error 'ERROR "no existing label\n")]
        [else
  (write-byte (bitwise-and (arithmetic-shift token-lexeme-value -24) #xff))
  (write-byte (bitwise-and (arithmetic-shift token-lexeme-value -16) #xff))
  (write-byte (bitwise-and (arithmetic-shift token-lexeme-value -8) #xff))
  (write-byte (bitwise-and token-lexeme-value #xff)); code from Tory's slides
  (void)]))

(define (dotword-parse word-token)
    (match word-token
      [(or (token 'int token-lexeme-value) (token 'hexint token-lexeme-value)) (output token-lexeme-value)]
      [(token 'id token-lexeme-value) ; (printf "~a" (list->string token-lexeme-value))
                                      (output (hash-ref label-ht (list->string token-lexeme-value) -2))]
      [_ (error 'ERROR "unexpected commend line in parse\n")]))


(define (add-label-to-heaptable name line-num)
  (if (empty? (hash-ref label-ht (substring name 0 (- (string-length name) 1)) empty))
  (hash-set! label-ht (substring name 0 (- (string-length name) 1)) line-num) false))

; This function is to add and clean labels
(define (clean-label lines line-num)
  (cond [(empty? lines) empty]
        [else
         (local
           [(define single-line (first lines))
           (define (single-line-add-label line-with-labels)
               (cond
                 [(empty? line-with-labels) empty]
                 [(equal? (token-kind (first line-with-labels)) 'label)
                  (if (add-label-to-heaptable (list->string (token-lexeme (first line-with-labels))) line-num)
                  (single-line-add-label (rest line-with-labels))
                  (error 'ERROR "Existing label can not be added\n"))]
                 [(not (= 0 (length (filter (lambda (single-token) (equal? (token-kind single-token) 'label)) line-with-labels))))
                    (error 'ERROR "Label not at the beginning of a line\n")]
                 [else (cons (first line-with-labels) (single-line-add-label (rest line-with-labels)))]))]
           (cond
             [(equal? (token-kind (first single-line)) 'label)
                ; (cond [(andmap (lambda (single-token) (equal? (token-kind single-token) 'label)) single-line) (set! line-num (- line-num 4))])
                (local [(define after-label (single-line-add-label single-line))]
                (cond [(empty? after-label) (clean-label (rest lines) line-num)] ;If it is all labels in one line, then the line-num should not be changed
                      [else (cons after-label (clean-label (rest lines) (+ 4 line-num)))]))]
             [else (cons single-line (clean-label (rest lines) (+ 4 line-num)))]))]))


; translate beq/bne $_, $_, label to beq/bne $_, $_, number
(define (translate-label lines line-num)
  (cond [(empty? lines) empty]
        [else
         (local
           [(define single-line (first lines))
           (define (single-line-skip-label line-with-labels)`
               (cond
                 [(empty? line-with-labels) empty]
                 [(equal? (token-kind (first line-with-labels)) 'label) (single-line-skip-label (rest line-with-labels))]
                 [else (cons (first line-with-labels) (single-line-skip-label (rest line-with-labels)))]))]
           (cond
             [(equal? (token-kind (first single-line)) 'label)
                (local [(define after-label (single-line-skip-label single-line))]
                (cond [(empty? after-label) (clean-label (rest lines) line-num)] ;If it is all labels in one line, then the line-num should not be changed
                      [else (cons after-label (translate-label (rest lines) (+ 4 line-num)))]))]
             [else
              (match single-line
             [(list (token 'id '(#\b #\e #\q)) (token 'register d) (token 'comma '(#\,)) (token 'register s) (token 'comma '(#\,)) (token 'id i))
                      (if (= (hash-ref label-ht (list->string i) -2) -2) (error 'ERROR "No such a label\n")
                          (cons (list (token 'id '(#\b #\e #\q)) (token 'register d) (token 'comma '(#\,)) (token 'register s) (token 'comma '(#\,))
                                      (token 'int (- (/ (- (hash-ref label-ht (list->string i) -2) line-num) 4) 1)))
                                (translate-label (rest lines) (+ 4 line-num))))]
                [(list (token 'id '(#\b #\n #\e)) (token 'register d) (token 'comma '(#\,)) (token 'register s) (token 'comma '(#\,)) (token 'id i))
                      (if (= (hash-ref label-ht (list->string i) -2) -2) (error 'ERROR "No such a label\n")
                          (cons (list (token 'id '(#\b #\n #\e)) (token 'register d) (token 'comma '(#\,)) (token 'register s) (token 'comma '(#\,))
                                      (token 'int (- (/ (- (hash-ref label-ht (list->string i) -2) line-num) 4) 1)))
                                (translate-label (rest lines) (+ 4 line-num))))]
                [_ (cons single-line (translate-label (rest lines) (+ 4 line-num)))])]))]))

; jr option = 8, jalr option = 9
(define (jr-jalr-parse par option)
  (match par
      [(token 'register token-lexeme-value)
         (output bitwise-ior (arithmetic-shift 0 26) (arithmetic-shift token-lexeme-value 21)  (arithmetic-shift option 0))
         void]
      [_ (error 'ERROR "unexpected commend line jr/jalr in parse\n")]))



; This function is provided to recognize the instructions and Error checking
(define (recog-ins single-line)
  (local
    [(define first-token (first single-line))
    (define first-token-kind (token-kind first-token))
    (define token-amount (length single-line))]
    (cond
      [(empty? first-token) empty]
      [(and (equal? first-token-kind 'dotword) (= token-amount 2)) (dotword-parse (second single-line))]
      [(and (equal? first-token-kind 'id) (= token-amount 2)(equal? (list->string (token-lexeme first-token)) "jr")) (jr-jalr-parse (second single-line) 8)]
      [(and (equal? first-token-kind 'id) (= token-amount 2)(equal? (list->string (token-lexeme first-token)) "jalr")) (jr-jalr-parse (second single-line) 9)]
      [else
       (match single-line
         ;add sub slt sltu
    [(list (token 'id '(#\a #\d #\d)) (token 'register d) (token 'comma '(#\,)) (token 'register s) (token 'comma '(#\,)) (token 'register t))
     (output (bitwise-ior (arithmetic-shift 0 26) (arithmetic-shift s 21) (arithmetic-shift t 16) (arithmetic-shift d 11) (arithmetic-shift 32 0)))]
    [(list (token 'id '(#\s #\u #\b)) (token 'register d) (token 'comma '(#\,)) (token 'register s) (token 'comma '(#\,)) (token 'register t))
     (output (bitwise-ior (arithmetic-shift 0 26) (arithmetic-shift s 21) (arithmetic-shift t 16) (arithmetic-shift d 11) (arithmetic-shift 34 0)))]
    [(list (token 'id '(#\s #\l #\t)) (token 'register d) (token 'comma '(#\,)) (token 'register s) (token 'comma '(#\,)) (token 'register t))
     (output (bitwise-ior (arithmetic-shift 0 26) (arithmetic-shift s 21) (arithmetic-shift t 16) (arithmetic-shift d 11) (arithmetic-shift 42 0)))]
    [(list (token 'id '(#\s #\l #\t #\u)) (token 'register d) (token 'comma '(#\,)) (token 'register s) (token 'comma '(#\,)) (token 'register t))
     (output (bitwise-ior (arithmetic-shift 0 26) (arithmetic-shift s 21) (arithmetic-shift t 16) (arithmetic-shift d 11) (arithmetic-shift 43 0)))]
         ;beq, bne
    [(list (token 'id '(#\b #\e #\q)) (token 'register d) (token 'comma '(#\,)) (token 'register s) (token 'comma '(#\,)) (token 'int i))
     (if (and (>= i -32768)(<= i 32767))
         (output (bitwise-ior (arithmetic-shift 4 26) (arithmetic-shift d 21) (arithmetic-shift s 16)  (bitwise-and i #xffff)))
         (error 'ERROR "exceed 0xffff\n"))]
    [(list (token 'id '(#\b #\n #\e)) (token 'register d) (token 'comma '(#\,)) (token 'register s) (token 'comma '(#\,)) (token 'int i))
     (if (and (>= i -32768)(<= i 32767))
         (output (bitwise-ior (arithmetic-shift 5 26) (arithmetic-shift d 21) (arithmetic-shift s 16)  (bitwise-and i #xffff)))
         (error 'ERROR "exceed 0xffff\n"))]
    [(list (token 'id '(#\b #\e #\q)) (token 'register d) (token 'comma '(#\,)) (token 'register s) (token 'comma '(#\,)) (token 'hexint i))
     (if (and (>= i 0)(<= i 65535))
         (output (bitwise-ior (arithmetic-shift 4 26) (arithmetic-shift d 21) (arithmetic-shift s 16)  (bitwise-and i #xffff)))
         (error 'ERROR "exceed 0xffff\n"))]
    [(list (token 'id '(#\b #\n #\e)) (token 'register d) (token 'comma '(#\,)) (token 'register s) (token 'comma '(#\,)) (token 'hexint i))
     (if (and (>= i 0)(<= i 65535))
         (output (bitwise-ior (arithmetic-shift 5 26) (arithmetic-shift d 21) (arithmetic-shift s 16)  (bitwise-and i #xffff)))
         (error 'ERROR "exceed 0xffff\n"))]
         ; lis, mflo, mfhi
    [(list (token 'id '(#\l #\i #\s)) (token 'register d))
     (output (bitwise-ior (arithmetic-shift 0 16) (arithmetic-shift d 11) (arithmetic-shift 20 0)))]
    [(list (token 'id '(#\m #\f #\l #\o)) (token 'register d))
     (output (bitwise-ior (arithmetic-shift 0 16) (arithmetic-shift d 11) (arithmetic-shift 18 0)))]
    [(list (token 'id '(#\m #\f #\h #\i)) (token 'register d))
     (output (bitwise-ior (arithmetic-shift 0 16) (arithmetic-shift d 11) (arithmetic-shift 16 0)))]
         ; mult, multu, div, divu
    [(list (token 'id '(#\m #\u #\l #\t)) (token 'register d) (token 'comma '(#\,)) (token 'register s))
     (output (bitwise-ior (arithmetic-shift 0 26) (arithmetic-shift d 21) (arithmetic-shift s 16) (arithmetic-shift 24 0)))]
    [(list (token 'id '(#\m #\u #\l #\t #\u)) (token 'register d) (token 'comma '(#\,)) (token 'register s))
     (output (bitwise-ior (arithmetic-shift 0 26) (arithmetic-shift d 21) (arithmetic-shift s 16) (arithmetic-shift 25 0)))]
    [(list (token 'id '(#\d #\i #\v)) (token 'register d) (token 'comma '(#\,)) (token 'register s))
     (output (bitwise-ior (arithmetic-shift 0 26) (arithmetic-shift d 21) (arithmetic-shift s 16) (arithmetic-shift 26 0)))]
    [(list (token 'id '(#\d #\i #\v #\u)) (token 'register d) (token 'comma '(#\,)) (token 'register s))
     (output (bitwise-ior (arithmetic-shift 0 26) (arithmetic-shift d 21) (arithmetic-shift s 16) (arithmetic-shift 27 0)))]
    [(list (token 'id '(#\s #\w)) (token 'register d)  (token 'comma '(#\,)) (token 'int i) (token 'lparen '(#\())  (token 'register s) (token 'rparen '(#\))))
     (output (bitwise-ior (arithmetic-shift 43 26) (arithmetic-shift s 21) (arithmetic-shift d 16) (bitwise-and i #xffff)))]
    [(list (token 'id '(#\s #\w)) (token 'register d)  (token 'comma '(#\,)) (token 'hexint i) (token 'lparen '(#\())  (token 'register s) (token 'rparen '(#\))))
     (output (bitwise-ior (arithmetic-shift 43 26) (arithmetic-shift s 21) (arithmetic-shift d 16) (bitwise-and i #xffff)))]
    [(list (token 'id '(#\l #\w)) (token 'register d)  (token 'comma '(#\,)) (token 'int i) (token 'lparen '(#\())  (token 'register s) (token 'rparen '(#\))))
     (output (bitwise-ior (arithmetic-shift 35 26) (arithmetic-shift s 21) (arithmetic-shift d 16) (bitwise-and i #xffff)))]
    [(list (token 'id '(#\l #\w)) (token 'register d)  (token 'comma '(#\,)) (token 'hexint i) (token 'lparen '(#\())  (token 'register s) (token 'rparen '(#\))))
     (output (bitwise-ior (arithmetic-shift 35 26) (arithmetic-shift s 21) (arithmetic-shift d 16) (bitwise-and i #xffff)))]
    [else (error 'ERROR "unexpected commend line\n")])])))






(define (scan-all)  (map recog-ins (translate-label (clean-label (scan-input) 0) 0)) (void))

(scan-all)
;label tables
(hash-for-each label-ht (lambda (x y) (fprintf  (current-error-port) "~a ~a\n" x y)))

