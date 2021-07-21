#lang racket

(require racket/serialize)
(require sha)
(require (only-in file/sha1 hex-string->bytes))

(define difficulty 2)
(define target (bytes->hex-string (make-bytes difficulty 32)))

(struct block
  (current-hash previous-hash transaction timestamp nonce)
  #:prefab)

(struct transaction
  (signature from to value)
  #:prefab)

(define (calculate-block-hash previous-hash timestamp transaction nonce)
  (bytes->hex-string (sha256 (bytes-append
                              (string->bytes/utf-8 previous-hash)
                              (string->bytes/utf-8 (number->string timestamp))
                              (string->bytes/utf-8 (~a (serialize transaction)))
                              (string->bytes/utf-8 (number->string nonce))))))

(define (valid-block? bl)
  (equal? (block-current-hash bl)
          (calculate-block-hash (block-previous-hash bl)
                                (block-timestamp bl)
                                (block-transaction bl)
                                (block-nonce bl))))

;; original code from book has a bug, using 1 instead of 0 to index the number
;; of bytes to match
(define (mined-block? block-hash)
  (equal? (subbytes (hex-string->bytes block-hash) 0 difficulty)
          (subbytes (hex-string->bytes target) 0 difficulty)))

(define (make-and-mine-block previous-hash timestamp transaction nonce)
  (let ([current-hash (calculate-block-hash previous-hash timestamp transaction nonce)])
    (if (mined-block? current-hash)
        (block current-hash previous-hash transaction timestamp nonce)
        (make-and-mine-block previous-hash timestamp transaction (+ nonce 1)))))

(define (mine-block transaction previous-hash)
  (make-and-mine-block
   previous-hash (current-milliseconds) transaction 1))

(provide (struct-out block) mine-block valid-block? mined-block?)
