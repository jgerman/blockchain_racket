#lang racket

(require crypto)
(require crypto/all)

;; the original code didn't have name but I thought it'd be easier to debug
;; things by attaching an easily identifiable name to a wallet
(struct wallet
  (private-key public-key name)
  #:prefab)

(define (make-wallet name)
  (letrec ([rsa-impl (get-pk 'rsa libcrypto-factory)]
           [privkey (generate-private-key rsa-impl '((nbits 512)))]
           [pubkey (pk-key->public-only-key privkey)])
    (wallet (bytes->hex-string
             (pk-key->datum privkey 'PrivateKeyInfo))
            (bytes->hex-string
             (pk-key->datum pubkey 'SubjectPublicKeyInfo))
            name)))

(provide (struct-out wallet) make-wallet)
