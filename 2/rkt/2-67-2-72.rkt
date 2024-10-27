#lang sicp

(define (test text value)
  (display text)
  (display " test: ")
  (if value 
      (display "Success!")
      (display "Failure"))
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2-67

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) 
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

#| (decode sample-message sample-tree) |#
#|  A D     A B   B   C     A |#
#| (0 1 1 0 0 1 0 1 0 1 1 1 0) |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2-68

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (inner bits tree)
    (cond ((leaf? tree) (reverse bits))
          ((memq symbol (symbols (left-branch tree))) (inner (cons 0 bits) (left-branch tree)))
          (else (inner (cons 1 bits) (right-branch tree)))))
  (if (memq symbol (symbols tree))
    (inner `() tree)
    (error "bad symbol: ENCODE-SYMBOL" symbol)))

#| (test "decode" (equal? (encode (decode sample-message sample-tree) sample-tree) sample-message)) |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2-69

(define sample-pairs '((A 4) (B 2) (C 1) (D 1)))
(define extended-pairs-1 '((A 8) (B 7) (C 6) (D 5) (E 4) (F 3) (G 2) (H 1)))
(define extended-pairs-2 '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge
        (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set))
                    (cddr leaf-set)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

#| sample-tree |#
#| (generate-huffman-tree sample-pairs) |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2-70

(define rock-pairs '((a 2) (get 2) (sha 3) (wah 1) (boom 1) (job 2) (na 16) (yip 9))) 
(define rock-tree (generate-huffman-tree rock-pairs))
(define rock-message 
  '(get a job
    sha na na na na na na na na
    get a job
    sha na na na na na na na na
    wah yip yip yip yip 
    yip yip yip yip yip
    sha boom))
(define rock-encoding (encode rock-message rock-tree))

#| (length rock-message) ; 36 words |#
#| (length rock-encoding) ; 84 bits  |#
#| (test "encode" (equal? (decode rock-encoding rock-tree) rock-message)) |#
; fix length encoding uses 108 bits at 3 bits per word
; ascii uses 288 bits at 1 byte per word

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2-71

(define (generate-squared-pairs n)
  (define letters '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
  (define (iter pairs count weight letters)
    (if (= n count)
        pairs
        (iter (cons (list (car letters) weight) pairs) (+ count 1) (* weight 2) (cdr letters))))
  (iter '() 0 1 letters))

(generate-squared-pairs 5)
(right-branch (generate-huffman-tree (generate-squared-pairs 5)))
; for general n, it takes 1 bit to encode the most frequent symbol 
; for the least frequent symbol, it takes n-1 bits to encode it

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2-72

; O(nlog(n))

