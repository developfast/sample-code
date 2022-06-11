;; Non Fungible Token
;; Token authentication tool for DonSafe, a blockchain and AI based organ donation interface
;; Token is authenticated for use by donor, recipient and the hospital
(define-non-fungible-token non-fungible-token int)

;; Storage
(define-map token-gen
  ((token-id int))
  ((spender principal)))
(define-map tokens-count
  ((owner principal))
  ((count int)))
(define-map hospital
  ((operator principal) (account principal))
  ((is-approved bool)))

;; Backend

;; Gets the amount of tokens i.e. organs to transplant at a particular hospital.
(define-private (balance-of (account principal))
  (default-to 0
    (get count
         (map-get? tokens-count ((owner account))))))

;; Gets the donor of the specified organ ID.
(define-public (owner-of? (token-id int))
  (ok (nft-get-owner? non-fungible-token token-id))
)

;; Gets the approved recipient for a organ ID, or zero if no recipient found yet
(define-private (is-spender-approved (spender principal) (token-id int))
  (let ((approved-spender
         (unwrap! (get spender
                        (map-get? token-gen ((token-id token-id))))
                   false))) ;; return false if no specified recipient
    (is-eq spender approved-spender)))

;; Tells whether an organ is approved by a transplantation intermediary
(define-private (is-operator-approved (account principal) (operator principal))
  (unwrap!
    (get is-approved
      (map-get? hospital ((operator operator) (account account)))
    )
    false
  )
  ;; (default-to false
  ;;   (get is-approved
  ;;   )
  ;; )
)

(define-private (is-owner (actor principal) (token-id int))
  (is-eq actor
    ;; if no donor, return false
    (unwrap! (nft-get-owner? non-fungible-token token-id) false)
  )
)

;; Returns whether the donor can transfer a given organ.
(define-private (can-transfer (actor principal) (token-id int))
  (or
   (is-owner actor token-id)
   (is-spender-approved actor token-id)
   (is-operator-approved (unwrap! (nft-get-owner? non-fungible-token token-id) false) actor)))

;; Internal - Register organ
(define-private (register-token (new-owner principal) (token-id int))
  (let ((current-balance (balance-of new-owner)))
    (begin
      (nft-mint? non-fungible-token token-id new-owner)
      (map-set tokens-count
        ((owner new-owner))
        ((count (+ 1 current-balance))))
      true)))

;; Internal - Release token
(define-private (release-token (owner principal) (token-id int))
  (let ((current-balance (balance-of owner)))
    (begin
      (map-delete token-gen
        ((token-id token-id)))
      (map-set tokens-count
        ((owner owner))
        ((count (- current-balance 1))))
      true)))

;; Public functions

(define-constant same-spender-err (err 1))
(define-constant not-approved-spender-err (err 2))
(define-constant failed-to-move-token-err (err 3))
(define-constant unauthorized-transfer-err (err 4))
(define-constant failed-to-mint-err (err 5))

;; Approves another address to transfer the given organ ID 
(define-public (set-spender-approval (spender principal) (token-id int))
  (if (is-eq spender tx-sender)
      same-spender-err
      (if (or (is-owner tx-sender token-id)
              (is-operator-approved
               (unwrap! (nft-get-owner? non-fungible-token token-id) not-approved-spender-err)
               tx-sender))
          (begin
            (map-set token-gen
                        ((token-id token-id))
                        ((spender spender)))
            (ok token-id))
          not-approved-spender-err)))

;; Sets or unsets the approval of a given intermediary
(define-public (set-operator-approval (operator principal) (is-approved bool))
  (if (is-eq operator tx-sender)
      same-spender-err
      (begin
        (map-set hospital
                    ((operator operator) (account tx-sender))
                    ((is-approved is-approved)))
        (ok true))))

;; Transfers the ownership of a given organ to another organisation.
(define-public (transfer-from (owner principal) (recipient principal) (token-id int))
  (if (and
        (can-transfer tx-sender token-id)
        (is-owner owner token-id)
        (not (is-eq recipient owner))
      )
      (if
        (and
          (unwrap-panic (nft-transfer? non-fungible-token token-id owner recipient))
          (map-set tokens-count
            ((owner recipient))
            ((count (+ 1 (balance-of recipient))))
          )
          (map-set tokens-count
            ((owner owner))
            ((count (- (balance-of owner) 1)))
          )
        )
        (ok token-id)
      failed-to-move-token-err)
    unauthorized-transfer-err
  )
)

;; Transfers organs to a specified intermediary.
(define-public (transfer (recipient principal) (token-id int))
  (transfer-from tx-sender recipient token-id))

;; Initialize the contract
(begin
  (mint! 'ST398K1WZTBVY6FE2YEHM6HP20VSNVSSPJTW0D53M 10001)
  (mint! 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 10002)
  (mint! 'S02J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKPVKG2CE 10003))
