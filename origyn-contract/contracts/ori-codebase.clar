;; Luxury Goods Authentication System - Phase 3
;; Focus: Comprehensive history tracking system

;; ========= Constants =========
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_INPUT (err u101))
(define-constant ERR_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_TRANSFER (err u104))
(define-constant ERR_INVALID_HISTORY (err u105))
(define-constant MAX_HISTORY_SEARCH u50)

;; ========= Data Types =========
;; Define action types for history tracking
(define-data-var valid-actions (list 20 (string-utf8 20)) 
    (list 
        "created"
        "transferred"
        "locked"
        "unlocked"
        "status-changed"
        "verified"
        "flagged"
        "unflagged"
        "manufacturer-action"
        "retailer-action"
    )
)

;; ========= Data Maps =========
;; Core product data (inherited from previous phases)
(define-map products 
    {serial-number: (string-utf8 50)}
    {
        brand: (string-utf8 50),
        model: (string-utf8 50),
        manufacturer: principal,
        timestamp: uint,
        status: (string-utf8 20),
        current-owner: principal
    }
)

;; Comprehensive event history tracking
(define-map product-history
    {serial-number: (string-utf8 50), index: uint}
    {
        timestamp: uint,
        action: (string-utf8 20),
        actor: principal,
        details: (optional (string-utf8 256)),
        data: (optional {
            old-value: (string-utf8 50),
            new-value: (string-utf8 50)
        })
    }
)

;; Track history indices per product
(define-map history-indices
    (string-utf8 50)  ;; serial-number
    {
        total-events: uint,
        last-timestamp: uint,
        first-timestamp: uint
    }
)

;; Track specific event types per product
(define-map event-type-indices
    {serial-number: (string-utf8 50), action: (string-utf8 20)}
    (list 50 uint)  ;; List of indices for specific event type
)

;; ========= Read-Only Functions =========
;; Get single history entry
(define-read-only (get-history-entry 
    (serial-number (string-utf8 50))
    (index uint))
    (map-get? product-history {serial-number: serial-number, index: index})
)

;; Get history summary
(define-read-only (get-history-summary (serial-number (string-utf8 50)))
    (map-get? history-indices serial-number)
)

;; Get event type history
(define-read-only (get-events-by-type 
    (serial-number (string-utf8 50))
    (action (string-utf8 20)))
    (map-get? event-type-indices {serial-number: serial-number, action: action})
)

;; Get history range
(define-read-only (get-history-range
    (serial-number (string-utf8 50))
    (start-index uint)
    (end-index uint))
    
    (let
        (
            (summary (unwrap! (get-history-summary serial-number) ERR_NOT_FOUND))
            (max-index (get total-events summary))
        )
        ;; Validate range
        (if (and 
                (<= start-index end-index)
                (< end-index max-index)
                (<= (- end-index start-index) MAX_HISTORY_SEARCH))
            (ok (filter not-none 
                (map unwrap-panic
                    (map (lambda (index)
                        (get-history-entry serial-number index))
                        (list start-index end-index)))))
            ERR_INVALID_HISTORY)
    )
)

;; ========= History Recording Functions =========
;; Record a new history event
(define-private (record-history-event
    (serial-number (string-utf8 50))
    (action (string-utf8 20))
    (details (optional (string-utf8 256)))
    (data (optional {
        old-value: (string-utf8 50),
        new-value: (string-utf8 50)
    })))
    
    (let
        (
            (current-summary (default-to 
                {
                    total-events: u0,
                    last-timestamp: u0,
                    first-timestamp: (unwrap! (get-block-info? time (- block-height u1)) ERR_INVALID_INPUT)
                } 
                (get-history-summary serial-number)))
            (new-index (get total-events current-summary))
            (timestamp (unwrap! (get-block-info? time (- block-height u1)) ERR_INVALID_INPUT))
        )
        
        ;; Record the event
        (try! (map-set product-history
            {serial-number: serial-number, index: new-index}
            {
                timestamp: timestamp,
                action: action,
                actor: tx-sender,
                details: details,
                data: data
            }))
        
        ;; Update indices
        (try! (map-set history-indices
            serial-number
            {
                total-events: (+ new-index u1),
                last-timestamp: timestamp,
                first-timestamp: (get first-timestamp current-summary)
            }))
        
        ;; Update event type indices
        (let
            (
                (current-type-indices (default-to (list) 
                    (get-events-by-type serial-number action)))
            )
            (map-set event-type-indices
                {serial-number: serial-number, action: action}
                (append current-type-indices new-index))
        )
        
        (ok new-index)
    )
)

;; ========= Enhanced Product Management =========
;; Enhanced product registration with history
(define-public (register-product 
    (serial-number (string-utf8 50))
    (brand (string-utf8 50))
    (model (string-utf8 50)))
    
    (begin
        ;; Original registration logic
        (try! (map-set products
            {serial-number: serial-number}
            {
                brand: brand,
                model: model,
                manufacturer: tx-sender,
                timestamp: (unwrap! (get-block-info? time (- block-height u1)) ERR_INVALID_INPUT),
                status: "active",
                current-owner: tx-sender
            }))
        
        ;; Record creation in history
        (try! (record-history-event
            serial-number
            "created"
            (some "Initial product registration")
            (some {
                old-value: "",
                new-value: brand
            })))
        
        (ok true)
    )
)

;; Enhanced status update with history
(define-public (update-product-status
    (serial-number (string-utf8 50))
    (new-status (string-utf8 20)))
    
    (let
        (
            (product (unwrap! (get-product serial-number) ERR_NOT_FOUND))
            (old-status (get status product))
        )
        ;; Update status
        (try! (map-set products
            {serial-number: serial-number}
            (merge product {status: new-status})))
        
        ;; Record status change in history
        (try! (record-history-event
            serial-number
            "status-changed"
            none
            (some {
                old-value: old-status,
                new-value: new-status
            })))
        
        (ok true)
    )
)

;; ========= History Query Functions =========
;; Get all events of a specific type
(define-read-only (query-events-by-type
    (serial-number (string-utf8 50))
    (action (string-utf8 20))
    (limit uint))
    
    (let
        (
            (indices (get-events-by-type serial-number action))
        )
        (match indices
            indices-list
            (ok (map unwrap-panic
                (map (lambda (index)
                    (get-history-entry serial-number index))
                    (take (default-to (list) indices-list) limit))))
            ERR_NOT_FOUND)
    )
)

;; Get recent history
(define-read-only (get-recent-history
    (serial-number (string-utf8 50))
    (limit uint))
    
    (let
        (
            (summary (unwrap! (get-history-summary serial-number) ERR_NOT_FOUND))
            (total (get total-events summary))
        )
        (if (> total u0)
            (get-history-range 
                serial-number
                (if (> total limit) (- total limit) u0)
                (- total u1))
            (ok (list)))
    )
)