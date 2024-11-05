;; Luxury Goods Authentication System
;; Description: Smart contract system for authenticating and tracking luxury goods on Stacks blockchain

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_INPUT (err u101))
(define-constant ERR_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_STATUS (err u104))

;; Status constants with explicit type casting
(define-constant STATUS_ACTIVE "active")
(define-constant STATUS_SUSPENDED "suspended")
(define-constant STATUS_RETIRED "retired")

;; Data Variables
(define-data-var timestamp-counter uint u0)

;; Data Maps
(define-map products 
    {serial-number: (string-ascii 50)}
    {
        brand: (string-ascii 50),
        model: (string-ascii 50),
        manufacturer: principal,
        timestamp: uint,
        status: (string-ascii 20),
        current-owner: principal
    }
)

(define-map manufacturers principal bool)
(define-map retailers principal bool)

;; Product history tracking
(define-map product-history
    {serial-number: (string-ascii 50), index: uint}
    {
        owner: principal,
        timestamp: uint,
        action: (string-ascii 20)
    }
)

;; Track history indices
(define-map history-indices
    (string-ascii 50) ;; serial-number
    uint             ;; current index
)

;; Read-only functions
(define-read-only (get-product (serial-number (string-ascii 50)))
    (map-get? products {serial-number: serial-number})
)

(define-read-only (is-manufacturer (address principal))
    (default-to false (map-get? manufacturers address))
)

(define-read-only (is-retailer (address principal))
    (default-to false (map-get? retailers address))
)

(define-read-only (get-current-timestamp)
    (var-get timestamp-counter)
)

;; Administrative functions
(define-public (register-manufacturer (manufacturer principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (asserts! (is-none (map-get? manufacturers manufacturer)) ERR_ALREADY_EXISTS)
        (ok (map-set manufacturers manufacturer true))
    )
)

(define-public (register-retailer (retailer principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (asserts! (is-none (map-get? retailers retailer)) ERR_ALREADY_EXISTS)
        (ok (map-set retailers retailer true))
    )
)

;; Product registration
(define-public (register-product 
    (serial-number (string-ascii 50))
    (brand (string-ascii 50))
    (model (string-ascii 50)))
    
    (let
        (
            (manufacturer tx-sender)
            (timestamp (var-get timestamp-counter))
        )
        
        ;; Verify manufacturer authorization
        (asserts! (is-manufacturer manufacturer) ERR_NOT_AUTHORIZED)
        
        ;; Check product doesn't already exist
        (asserts! (is-none (get-product serial-number)) ERR_ALREADY_EXISTS)
        
        ;; Validate input
        (asserts! (> (len brand) u0) ERR_INVALID_INPUT)
        (asserts! (> (len model) u0) ERR_INVALID_INPUT)
        
        ;; Register product
        (map-set products
            {serial-number: serial-number}
            {
                brand: brand,
                model: model,
                manufacturer: manufacturer,
                timestamp: timestamp,
                status: STATUS_ACTIVE,
                current-owner: manufacturer
            }
        )
        
        ;; Initialize history
        (map-set product-history
            {serial-number: serial-number, index: u0}
            {
                owner: manufacturer,
                timestamp: timestamp,
                action: "registered"
            }
        )
        (map-set history-indices serial-number u1)
        
        ;; Increment timestamp counter
        (var-set timestamp-counter (+ timestamp u1))
        
        (ok true)
    )
)

;; Transfer ownership
(define-public (transfer-ownership
    (serial-number (string-ascii 50))
    (new-owner principal))
    
    (let
        (
            (product (unwrap! (get-product serial-number) ERR_NOT_FOUND))
            (timestamp (var-get timestamp-counter))
            (current-index (default-to u0 (map-get? history-indices serial-number)))
        )
        
        ;; Verify sender owns the product
        (asserts! (is-eq (get current-owner product) tx-sender) ERR_NOT_AUTHORIZED)
        
        ;; Validate new owner
        (asserts! (or (is-manufacturer new-owner) (is-retailer new-owner)) ERR_INVALID_INPUT)
        
        ;; Update product ownership
        (map-set products
            {serial-number: serial-number}
            (merge product {current-owner: new-owner})
        )
        
        ;; Record transfer in history
        (map-set product-history
            {serial-number: serial-number, index: current-index}
            {
                owner: new-owner,
                timestamp: timestamp,
                action: "transferred"
            }
        )
        (map-set history-indices serial-number (+ current-index u1))
        
        ;; Increment timestamp counter
        (var-set timestamp-counter (+ timestamp u1))
        
        (ok true)
    )
)

;; Verify product authenticity
(define-public (verify-product
    (serial-number (string-ascii 50)))
    
    (let
        (
            (product (unwrap! (get-product serial-number) ERR_NOT_FOUND))
        )
        
        ;; Verify manufacturer is still authorized
        (asserts! (is-manufacturer (get manufacturer product)) ERR_NOT_AUTHORIZED)
        
        ;; Return verification result
        (ok {
            is-authentic: true,
            manufacturer: (get manufacturer product),
            current-owner: (get current-owner product),
            status: (get status product)
        })
    )
)

;; Get product history
(define-read-only (get-product-history 
    (serial-number (string-ascii 50))
    (index uint))
    
    (map-get? product-history {serial-number: serial-number, index: index})
)

;; Update product status
(define-public (update-product-status
    (serial-number (string-ascii 50))
    (new-status (string-ascii 20)))
    
    (let
        (
            (product (unwrap! (get-product serial-number) ERR_NOT_FOUND))
        )
        
        ;; Only manufacturer can update status
        (asserts! (is-eq (get manufacturer product) tx-sender) ERR_NOT_AUTHORIZED)
        
        ;; Validate status
        (asserts! (or
            (is-eq new-status STATUS_ACTIVE)
            (is-eq new-status STATUS_SUSPENDED)
            (is-eq new-status STATUS_RETIRED)
        ) ERR_INVALID_STATUS)
        
        ;; Update status
        (ok (map-set products
            {serial-number: serial-number}
            (merge product {status: new-status})
        ))
    )
)
