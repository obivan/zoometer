#lang racket

(require net/url
         json)

(define resp/c (hash/c symbol? any/c))

(provide (contract-out
          [send-message (-> integer? string? resp/c)]
          [get-me (-> resp/c)]
          [get-updates (-> (or/c #f integer?) (list/c resp/c))]
          [on-update (->* ((-> resp/c any/c)) ((or/c #f integer?)) procedure?)])
         api-token)

(define api-token (make-parameter ""))
(define update-limit (make-parameter 50))
(define update-timeout (make-parameter 30))

(define (make-api-url method [params empty])
  (define scheme "https")
  (define user #f)
  (define host "api.telegram.org")
  (define port #f)
  (define path-absolute? #t)
  (define path (list (path/param (string-append "bot" (api-token)) empty)
                     (path/param (symbol->string method) empty)))
  (define fragment #f)
  (url scheme user host port path-absolute? path params fragment))

(define (port->jsexpr port)
  (string->jsexpr (port->string port)))

(define (get-me)
  (define resp (port->jsexpr (get-pure-port (make-api-url 'getMe))))
  (checked-response 'get-me resp))

(define (send-message chat-id text)
  (define body (jsexpr->bytes
                (hasheq 'chat_id chat-id
                        'text text
                        'parse_mode "HTML")))
  (define headers (list "Content-type: application/json"))
  (define resp (port->jsexpr (post-pure-port (make-api-url 'sendMessage)
                                             body
                                             headers)))
  (checked-response 'send-message resp))

(define (get-updates [last-update-id #f])
  (define params (list (cons 'limit (number->string (update-limit)))
                       (cons 'timeout (number->string (update-timeout)))
                       (cons 'offset (maybe-number->string last-update-id))))
  (define resp (port->jsexpr (get-pure-port (make-api-url 'getUpdates params))))
  (checked-response 'get-updates resp))

(define (maybe-number->string n)
  (if (number? n) (number->string n) #f))

(define (on-update callback [last-update-id #f])
  (define updates (get-updates last-update-id))
  (define next-update-id (calculate-next-update-id last-update-id updates))
  (for-each callback updates)
  (on-update callback next-update-id))

(define (calculate-next-update-id last-update-id updates)
  (if (not (empty? updates))
      (let* ([last-update (last updates)]
             [next-update-id (add1 (hash-ref last-update 'update_id))])
        next-update-id)
      last-update-id))

(define (checked-response error-sym response)
  (define succeed? (hash-ref response 'ok))
  (when (not succeed?)
    (raise-user-error error-sym "responded with error: ~a" response))
  (hash-ref response 'result))
