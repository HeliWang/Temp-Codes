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

;; functions used in defining sample transition table

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
(define (check-beq-hexint-range n)
  (cond
    [(<= 0 n 65535) n]
    [else (error 'ERROR "beq/ben integer out of range: ~a" n)]))
(define (check-beq-int-range n)
  (cond
    [(<= -32768 n 32767) n]
    [else (error 'ERROR "beq/ben integer out of range: ~a" n)]))

(define (check-reg-range n)
  (cond
    [(<= 0 n 31) n]
    [else (error 'ERROR "register out of range: ~a" n)]))

;; Some very basic tests
;(scan "01")
;(scan "0xabcd ; should be ignored")
;(scan ".word 01234")
;(scan "0add")
;(scan "foo:     add $1, $2, $3   ; A comment.")

;a label(n,l) is a label with name n and located at postion l
(define-struct label (name location) #:transparent)
;labels, a list of labels
(define labels '())

; This file just uses scan to tokenize each line of the input
(define (scan-input)
  (define line (read-line))
  (cond
    [(eof-object? line) empty]
    [(> (string-length line) 0) ; Ignore blank lines
        ; Ignore comment-only lines as well
        ; When a comment-only line is scanned, an empty struct is returned
        (define scanned (scan line))
        (cond
          [(empty? scanned) (scan-input)]
          [else (cons scanned (scan-input))])]
    [else (scan-input)]))


;add-labels:(list of tokens)
 (define (add-labels lst loc)
   (cond
    [(empty? lst)  empty]
    [(eq? (token-kind (first lst)) 'label)
     (cond
       [(not (empty? (filter
                      (lambda (lb)
                        (string=? (get-labelname  (token-lexeme(first lst)))
                             (label-name lb))) labels)))
        (error 'ERROR "duplicate label")]
       [else 
        (set! labels (cons (make-label (get-labelname
                                        (token-lexeme(first lst))) loc) labels))
        (add-labels (rest lst) loc)])]
    [(not (empty? (filter (lambda (x) (eq? 'label (token-kind x))) lst)))
           (error 'ERROR "labels not in the beginning")]
    [else lst]
))
;get-labelname:string->sting
(define (get-labelname str)
  (list->string (get-labelname-rec str))
  )
;get-labelname-rec:list of char -> list of char
(define (get-labelname-rec lst)
  (cond
    [(empty? lst) empty]
    [(not (or (char-numeric? (first lst)) (char-alphabetic? (first lst))))
     (get-labelname-rec (rest lst))]
    [else (cons (first lst) (get-labelname-rec (rest lst)))]))
  
;print label tables to std error
(define (print-labels lbls)
  (cond
    [(empty? lbls) (printf "")]
    [else (fprintf  (current-error-port) "~a ~a\n" (label-name (first lbls)) (label-location (first lbls))
                 )
          (print-labels (rest lbls))]))

(define (get-label-loc str)
  (get-label-loc-rec str labels))
(define (get-label-loc-rec str lbls)
  (cond
    [(empty? lbls) empty]
    [(string=? str (label-name (first lbls))) (label-location (first lbls))]
    [else (get-label-loc-rec str (rest lbls))]))
      
;add labels, clean inputs
(define (clean input loc)
  (cond
    [(empty? input) empty]
    [(empty? (first input)) (clean (rest input) loc)]
    [(andmap (lambda (token) (eq? (token-kind token) 'label)) (first input))
     (cons (add-labels (first input) loc) (clean (rest input) loc))]
    [else (cons (add-labels (first input) loc) (clean (rest input) (+ 4 loc)))]))

;;Number -> bytes: number ,number (<= 8,>=0), returns bytes 
(define (Number->Bytes int length)
  (rm-n (bytes->list(integer->integer-bytes int 8 true true)) (- 8 length))
)
(define (rm-n lst n)
  (cond
    [(= n 0) (list->bytes lst)]
    [else (rm-n (rest lst) (sub1 n))]))


;parseWord: token, print bytes
;translate the .word to bytes
(define (parseWord token)
  (cond
    [(or (eq? (token-kind token) 'int)(eq? (token-kind token) 'hexint))
      (list->bytes
                    (cdr(cdr(cdr(cdr (bytes->list
                                      (integer->integer-bytes (token-lexeme token) 8 true true)))))))]
    [(eq? (token-kind token) 'id)
     (cond
       [(empty? (filter (lambda (lbl) (string=? (label-name lbl) (list->string (token-lexeme token)))) labels))
        (error 'ERROR "labels not found")]
       [else (list->bytes(cdr(cdr(cdr(cdr (bytes->list (integer->integer-bytes
                                            (label-location(first (filter (lambda (lbl) (string=? (label-name lbl)
                                                                                        (list->string (token-lexeme token)))) labels))) 8 true true)))))))])]
    [else (error 'ERROR "not allowed")])
)

;Decimal to binary (return is a tring)
(define (dec->bin num length)
  (local ([define result (if (>= num 0) (number->string (dec->bin-rec num 0))
                             (number->string (dec->bin-rec (abs num) 0)))])
  (if (>= num 0)
      (string-append (make-string (- length (string-length result))  #\0) result)
      (negate-bin (string-append (make-string (- length (string-length result))  #\0) result))
  )))

(define (negate-bin str)
  (list->string (reverse (bin-add-one (reverse (negate-bin-rec (string->list str))))))
  )
(define (negate-bin-rec lst)
  (cond
    [(empty? lst) empty]
    [else (cons (if (char=? (first lst) #\0) #\1 #\0) (negate-bin-rec (rest lst)))])
  )

(define (bin-add-one lst)
  (cond
    [(empty? lst) empty]
    [(char=? (first lst) #\0) (cons #\1 (rest lst))]
    [(char=? (first lst) #\1) (cons #\0 (bin-add-one(rest lst)))]))

         
(define (dec->bin-rec number count)
  (cond
    [(<= number 0) 0]
    [(if (= 1 (remainder number 2))
     (+ (dec->bin-rec (quotient number 2) (add1 count)) (expt 10 count))
     (dec->bin-rec (quotient number 2) (add1 count)))]))

(define (forth lst)
  (list-ref lst 3))
(define (fifth lst)
  (list-ref lst 4))
(define (sixth lst)
  (list-ref lst 5))
(define (seventh lst)
  (list-ref lst 6))

(define (bin->dec n)
  (bin->dec-rec (string->number n)))
(define  (bin->dec-rec n)
 (if (= 0 n) n (+ (modulo n 10) (* 2 (bin->dec-rec (quotient n 10))))))

;PARSE1: to add labels and clean inputs
(define (PASS1 input)
  (filter (lambda (lst) (not (empty? lst))) (clean input 12)))

(define reloc '())

;;find place need relocating 
(define (find-reloc lst pc)
  (local [(define line (if (empty? lst) empty (first lst)))]
  (cond
    [(empty? lst) empty]
    [(= (length line) 1) (error 'ERROR "invalid input")]
    [(and (eq? (token-kind (first line)) 'dotword) (eq? (token-kind (second line)) 'id))
     (set! reloc (cons pc reloc))
     (find-reloc (rest lst) (+ 4 pc))]
    [(empty? line)(find-reloc (rest lst) pc)]
    [else (find-reloc (rest lst) (+ 4 pc))])))

(define (print-reloc lst)
  (cond
    [(empty? lst) (printf "")]
    [else
     (write-bytes (Number->Bytes 1 4))
     (write-bytes (Number->Bytes (first lst) 4))
     (print-reloc (rest lst))]))
  


(define (PASS2 lst)
  (write-bytes (Number->Bytes 268435458 4))
  (find-reloc lst 12)
  (write-bytes (Number->Bytes (+ 12 (* 4(length lst))
         (* 8 (length reloc))) 4))
  (write-bytes (Number->Bytes (+ 12 (* 4(length lst))) 4))
  (PASS2-rec lst 12)
  (print-reloc reloc)
 )
(define (PASS2-rec lst pc)
  (local
    [(define inst (if (empty? lst) empty (first lst)))]
  (cond
  [(empty? lst) (printf "")]
  [(empty? inst) (PASS2-rec (rest lst) pc)]
  [else
   (cond 
  [(eq? 'dotword (token-kind (first inst)));if first label is .word
   (cond
     [(not (= (length inst) 2)) (error 'ERROR "to much/less input")]
     [(write-bytes (parseWord (second inst)))])]
  [(eq? 'id (token-kind (first inst)))
   (cond
     ;instruction jr and jalr
     [(member (list->string (token-lexeme (first inst))) '("jr" "jalr")) 
      (cond
        [(not (= (length inst) 2)) (error 'ERROR "to much/less input")]
        [(not (eq?  (token-kind (second inst)) 'register))
         (error 'ERROR "second input type invalid for jr and jalr")]
        [else
         (if (string=? "jr" (list->string (token-lexeme (first inst))))
             (write-bytes (Number->Bytes (bin->dec (string-append "000000" (dec->bin (token-lexeme  (second inst)) 5) "000000000000000001000")) 4))
             (write-bytes (Number->Bytes (bin->dec (string-append "000000" (dec->bin (token-lexeme  (second inst)) 5) "000000000000000001001")) 4))
             )])
      ]
     ;instruction add sub slt sltu
     [(member (list->string (token-lexeme (first inst))) '("add" "sub" "slt" "sltu"))
      (cond
        [(not (= (length inst) 6)) (error 'ERROR "to much/less input")]
        [(not (and (eq?  (token-kind (second inst)) 'register)
                   (eq?  (token-kind (third inst)) 'comma)
                   (eq?  (token-kind (forth inst)) 'register)
                   (eq?  (token-kind (fifth inst)) 'comma)
                   (eq?  (token-kind (sixth inst)) 'register)))
         (error 'ERROR "invalid input for add/sub/slt/sltu")]
        [else
         (cond
           [(string=? (list->string (token-lexeme (first inst))) "add")
            (write-bytes (Number->Bytes (bin->dec (string-append "000000"
                (dec->bin (token-lexeme  (forth inst)) 5)
                (dec->bin (token-lexeme  (sixth inst)) 5)
                (dec->bin (token-lexeme  (second inst)) 5)
                "00000100000")) 4))]
           [(string=? (list->string (token-lexeme (first inst))) "sub")
            (write-bytes (Number->Bytes (bin->dec (string-append "000000"
                (dec->bin (token-lexeme  (forth inst)) 5)
                (dec->bin (token-lexeme  (sixth inst)) 5)
                (dec->bin (token-lexeme  (second inst)) 5)
                "00000100010")) 4))]
           [(string=? (list->string (token-lexeme (first inst))) "slt")
            (write-bytes (Number->Bytes (bin->dec (string-append "000000"
                (dec->bin (token-lexeme  (forth inst)) 5)
                (dec->bin (token-lexeme  (sixth inst)) 5)
                (dec->bin (token-lexeme  (second inst)) 5)
                "00000101010")) 4))]
           [(string=? (list->string (token-lexeme (first inst))) "sltu")
            (write-bytes (Number->Bytes (bin->dec (string-append "000000"
                (dec->bin (token-lexeme  (forth inst)) 5)
                (dec->bin (token-lexeme  (sixth inst)) 5)
                (dec->bin (token-lexeme  (second inst)) 5)
                "00000101011")) 4))]
        )]
     )]
     ;instruction beq and bne
     [(member (list->string (token-lexeme (first inst))) '("beq" "bne"))
      (cond
        [(not (= (length inst) 6)) (error 'ERROR "to much/less input")]
        [(not (and (eq?  (token-kind (second inst)) 'register)
                   (eq?  (token-kind (third inst)) 'comma)
                   (eq?  (token-kind (forth inst)) 'register)
                   (eq?  (token-kind (fifth inst)) 'comma)
                   (or (and (eq? (token-kind (sixth inst)) 'id)
                            (not (empty? (get-label-loc (list->string (token-lexeme (sixth inst)))))))
                    (and (eq?  (token-kind (sixth inst)) 'int)
                         (check-beq-int-range (token-lexeme (sixth inst))))
                    (and (eq?  (token-kind (sixth inst)) 'hexint)
                         (check-beq-hexint-range (token-lexeme (sixth inst)))))))
         (error 'ERROR "invalid offset for beq/bne")]
        [else
         (cond
           [(and (not (eq? (token-kind (sixth inst)) 'id))(string=? (list->string (token-lexeme (first inst))) "beq"))
            (write-bytes (Number->Bytes (bin->dec (string-append "000100"
                (dec->bin (token-lexeme  (second inst)) 5)
                (dec->bin (token-lexeme  (forth inst)) 5)
                (dec->bin (token-lexeme  (sixth inst)) 16)
                )) 4))]
           [(and (not (eq? (token-kind (sixth inst)) 'id))(string=? (list->string (token-lexeme (first inst))) "bne"))
            (write-bytes (Number->Bytes (bin->dec (string-append "000101"
                (dec->bin (token-lexeme  (second inst)) 5)
                (dec->bin (token-lexeme  (forth inst)) 5)
                (dec->bin (token-lexeme  (sixth inst)) 16)
                )) 4))]
           [(and (eq? (token-kind (sixth inst)) 'id)(string=? (list->string (token-lexeme (first inst))) "beq"))
            (write-bytes (Number->Bytes (bin->dec (string-append "000100"
                (dec->bin (token-lexeme  (second inst)) 5)
                (dec->bin (token-lexeme  (forth inst)) 5)
                (dec->bin (check-beq-int-range (quotient (- (get-label-loc (list->string (token-lexeme  (sixth inst)))) (+ 4 pc))4)) 16)
                )) 4))]
           [(and (eq? (token-kind (sixth inst)) 'id)(string=? (list->string (token-lexeme (first inst))) "bne"))
            (write-bytes (Number->Bytes (bin->dec (string-append "000101"
                (dec->bin (token-lexeme  (second inst)) 5)
                (dec->bin (token-lexeme  (forth inst)) 5)
                (dec->bin (check-beq-int-range (quotient (- (get-label-loc (list->string (token-lexeme  (sixth inst)))) (+ 4 pc)) 4)) 16)
                )) 4))]
        )]
     )]
     ;instruction lis mflo mfhi
     [(member (list->string (token-lexeme (first inst))) '("lis" "mflo" "mfhi"))
      (cond
        [(not (= (length inst) 2)) (error 'ERROR "to much/less input")]
        [(not (eq?  (token-kind (second inst)) 'register))
         (error 'ERROR "invalid input for lis/mflo/mfhi")]
        [else
         (cond
           [(string=? (list->string (token-lexeme (first inst))) "lis")
            (write-bytes (Number->Bytes (bin->dec (string-append
                "0000000000000000"
                (dec->bin (token-lexeme  (second inst)) 5)
                "00000010100")) 4))]
           [(string=? (list->string (token-lexeme (first inst))) "mflo")
            (write-bytes (Number->Bytes (bin->dec (string-append 
                "0000000000000000"
                (dec->bin (token-lexeme  (second inst)) 5)
                "00000010010")) 4))]
           [(string=? (list->string (token-lexeme (first inst))) "mfhi")
            (write-bytes (Number->Bytes (bin->dec (string-append
                "0000000000000000"
                (dec->bin (token-lexeme  (second inst)) 5)
                "00000010000")) 4))]
        )]
     )]
     ;instruction sw lw
     [(member (list->string (token-lexeme (first inst))) '("sw" "lw"))
      (cond
        [(not (= (length inst) 7)) (error 'ERROR "to much/less input")]
        [(not (and (eq?  (token-kind (second inst)) 'register)
                   (eq?  (token-kind (third inst)) 'comma)
                   (or
                    (and (eq?  (token-kind (forth inst)) 'int)
                         (check-beq-int-range (token-lexeme (forth inst))))
                    (and (eq?  (token-kind (forth inst)) 'hexint)
                         (check-beq-hexint-range (token-lexeme (forth inst)))))
                   (eq?  (token-kind (fifth inst)) 'lparen)
                   (eq?  (token-kind (sixth inst)) 'register)
                   (eq?  (token-kind (seventh inst)) 'rparen)))
         (error 'ERROR "invalid input for sw lw")]
        [else
         (cond
           [(string=? (list->string (token-lexeme (first inst))) "lw")
            (write-bytes (Number->Bytes (bin->dec (string-append
                "100011"
                (dec->bin (token-lexeme  (sixth inst)) 5)
                (dec->bin (token-lexeme  (second inst)) 5)
                (dec->bin (token-lexeme  (forth inst)) 16)
                )) 4))]
           [(string=? (list->string (token-lexeme (first inst))) "sw")
            (write-bytes (Number->Bytes (bin->dec (string-append 
                "101011"
                (dec->bin (token-lexeme  (sixth inst)) 5)
                (dec->bin (token-lexeme  (second inst)) 5)
                (dec->bin (token-lexeme  (forth inst)) 16)
                )) 4))]
        )]
     )]
     ;instruction mult multu div divu
     [(member (list->string (token-lexeme (first inst))) '("mult" "multu" "div" "divu"))
      (cond
        [(not (= (length inst) 4)) (error 'ERROR "to much/less input")]
        [(not (and (eq?  (token-kind (second inst)) 'register)
                   (eq?  (token-kind (third inst)) 'comma)
                   (eq?  (token-kind (forth inst)) 'register)))
         (error 'ERROR "invalid input for mult/multu/div/divu")]
        [else
         (cond
           ;0000 00ss ssst tttt 0000 0000 0001 1000 
           [(string=? (list->string (token-lexeme (first inst))) "mult")
            (write-bytes (Number->Bytes (bin->dec (string-append "000000"
                (dec->bin (token-lexeme  (second inst)) 5)
                (dec->bin (token-lexeme  (forth inst)) 5)
                "0000000000011000"
                )) 4))]
           [(string=? (list->string (token-lexeme (first inst))) "multu")
            (write-bytes (Number->Bytes (bin->dec (string-append "000000"
                (dec->bin (token-lexeme  (second inst)) 5)
                (dec->bin (token-lexeme  (forth inst)) 5)
                "0000000000011001"
                )) 4))]
           [(string=? (list->string (token-lexeme (first inst))) "div")
            (write-bytes (Number->Bytes (bin->dec (string-append "000000"
                (dec->bin (token-lexeme  (second inst)) 5)
                (dec->bin (token-lexeme  (forth inst)) 5)
                "0000000000011010")) 4))]
           [(string=? (list->string (token-lexeme (first inst))) "divu")
            (write-bytes (Number->Bytes (bin->dec (string-append "000000"
                (dec->bin (token-lexeme  (second inst)) 5)
                (dec->bin (token-lexeme  (forth inst)) 5)
                "0000000000011011")) 4))]
        )]
     )]
  [else (error 'ERROR "other instructions not allowed")])])
   (PASS2-rec (rest lst) (+ 4 pc))])
))


(PASS2 (PASS1 (scan-input)))
;(print-labels labels)
