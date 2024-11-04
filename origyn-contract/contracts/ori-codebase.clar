;; Luxury Goods Authentication System - Phase 2
;; Version: 0.2
;; Focus: Ownership and transfer functionality
;; Building on Phase 1

;; ========= Constants =========
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_INPUT (err u101))
(define-constant ERR_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_TRANSFER (err u104))
(define-constant ERR_PRODUCT_LOCKED (err u105))

;; ========= Data Maps =========
;; Enhanced products map with ownership tracking
(define-map products 
    {serial-number: (string-utf8 50)}
    {
        brand: (string-utf8 50),
        model: (string-utf8 50),
        manufacturer: principal,
        timestamp: uint,
        status: (string-utf8 20),
        current-owner: principal,
        transfer-locked: bool,
        last-transfer: uint
    }
)

;; Authorization maps (carried over from Phase 1)
(define-map manufacturers principal bool)
(define-map retailers principal bool)

;; Transfer history tracking
(define-map ownership-history
    {serial-number: (string-utf8 50), transfer-id: uint}
    {
        from: principal,
        to: principal,
        timestamp: uint,
        price: (optional uint),
        transfer-type: (string-utf8 20)
    }
)

;; Track transfer counts per product
(define-map transfer-counts
    (string-utf8 50)  ;; serial-number
    uint              ;; number of transfers
)

;; ========= Read-Only Functions =========
;; Enhanced product query
(define-read-only (get-product (serial-number (string-utf8 50)))
    (map-get? products {serial-number: serial-number})
)

;; Get current owner
(define-read-only (get-current-owner (serial-number (string-utf8 50)))
    (match (get-product serial-number)
        product (ok (get current-owner product))
        ERR_NOT_FOUND
    )
)

;; Get transfer history
(define-read-only (get-transfer
    (serial-number (string-utf8 50))
    (transfer-id uint))
    (map-get? ownership-history 
        {serial-number: serial-number, transfer-id: transfer-id}
    )
)

;; Get total transfers
(define-read-only (get-transfer-count (serial-number (string-utf8 50)))
    (default-to u0 (map-get? transfer-counts serial-number))
)

;; Check if product is locked for transfer
(define-read-only (is-transfer-locked (serial-number (string-utf8 50)))
    (match (get-product serial-number)
        product (get transfer-locked product)
        false
    )
)

;; ========= Ownership Management Functions =========
;; Register product with ownership
(define-public (register-product 
    (serial-number (string-utf8 50))
    (brand (string-utf8 50))
    (model (string-utf8 50)))
    
    (let
        (
            (manufacturer tx-sender)
            (timestamp (unwrap! (get-block-info? time (- block-height u1)) ERR_INVALID_INPUT))
        )
        
        (asserts! (is-manufacturer manufacturer) ERR_NOT_AUTHORIZED)
        (asserts! (is-none (get-product serial-number)) ERR_ALREADY_EXISTS)
        
        ;; Initialize product with ownership
        (try! (map-set products
            {serial-number: serial-number}
            {
                brand: brand,
                model: model,
                manufacturer: manufacturer,
                timestamp: timestamp,
                status: "active",
                current-owner: manufacturer,
                transfer-locked: false,
                last-transfer: timestamp
            }
        ))
        
        ;; Initialize transfer history
        (try! (map-set ownership-history
            {serial-number: serial-number, transfer-id: u0}
            {
                from: manufacturer,
                to: manufacturer,
                timestamp: timestamp,
                price: none,
                transfer-type: "registration"
            }
        ))
        
        ;; Initialize transfer count
        (map-set transfer-counts serial-number u1)
        
        (ok true)
    )
)

;; Transfer ownership
(define-public (transfer-ownership
    (serial-number (string-utf8 50))
    (new-owner principal)
    (price (optional uint)))
    
    (let
        (
            (product (unwrap! (get-product serial-number) ERR_NOT_FOUND))
            (timestamp (unwrap! (get-block-info? time (- block-height u1)) ERR_INVALID_INPUT))
            (current-count (get-transfer-count serial-number))
        )
        
        ;; Validation checks
        (asserts! (is-eq (get current-owner product) tx-sender) ERR_NOT_AUTHORIZED)
        (asserts! (not (get transfer-locked product)) ERR_PRODUCT_LOCKED)
        (asserts! (not (is-eq new-owner tx-sender)) ERR_INVALID_TRANSFER)
        
        ;; Update product ownership
        (try! (map-set products
            {serial-number: serial-number}
            (merge product {
                current-owner: new-owner,
                last-transfer: timestamp
            })
        ))
        
        ;; Record transfer in history
        (try! (map-set ownership-history
            {serial-number: serial-number, transfer-id: current-count}
            {
                from: tx-sender,
                to: new-owner,
                timestamp: timestamp,
                price: price,
                transfer-type: "standard"
            }
        ))
        
        ;; Update transfer count
        (map-set transfer-counts serial-number (+ current-count u1))
        
        (ok true)
    )
)

;; Lock/unlock transfers
(define-public (set-transfer-lock
    (serial-number (string-utf8 50))
    (locked bool))
    
    (let
        (
            (product (unwrap! (get-product serial-number) ERR_NOT_FOUND))
        )
        
        ;; Only current owner can lock/unlock
        (asserts! (is-eq (get current-owner product) tx-sender) ERR_NOT_AUTHORIZED)
        
        (ok (map-set products
            {serial-number: serial-number}
            (merge product {
                transfer-locked: locked
            })
        ))
    )
)

;; Force transfer (manufacturer only)
(define-public (force-transfer
    (serial-number (string-utf8 50))
    (new-owner principal))
    
    (let
        (
            (product (unwrap! (get-product serial-number) ERR_NOT_FOUND))
            (timestamp (unwrap! (get-block-info? time (- block-height u1)) ERR_INVALID_INPUT))
            (current-count (get-transfer-count serial-number))
        )
        
        ;; Only manufacturer can force transfer
        (asserts! (is-eq (get manufacturer product) tx-sender) ERR_NOT_AUTHORIZED)
        
        ;; Update ownership
        (try! (map-set products
            {serial-number: serial-number}
            (merge product {
                current-owner: new-owner,
                last-transfer: timestamp,
                transfer-locked: false
            })
        ))
        
        ;; Record forced transfer
        (try! (map-set ownership-history
            {serial-number: serial-number, transfer-id: current-count}
            {
                from: (get current-owner product),
                to: new-owner,
                timestamp: timestamp,
                price: none,
                transfer-type: "forced"
            }
        ))
        
        ;; Update transfer count
        (map-set transfer-counts serial-number (+ current-count u1))
        
        (ok true)
    )
)