#lang racket

(require db
         (only-in "telegram.rkt" send-message))

(provide (contract-out
          [shutdown-rating-db (-> void?)]
          [init-rating-db (-> (or/c path-string? 'memory) connection?)]
          [reply-message? (-> (hash/c symbol? any/c) boolean?)]
          [handle-reply (-> (hash/c symbol? any/c) any/c)])
         rating-db)

(define (init-rating-db file)
  (when (not (sqlite3-available?))
    (raise-user-error 'db-initialize
                      (string-join '("The SQLite native client library is required;"
                                     "see https://docs.racket-lang.org/db/notes.html#%28part._sqlite3-requirements%29"))))
  (define connect (sqlite3-connect #:database file
                                   #:mode 'create
                                   #:use-place #t))
  (migrate-db connect)
  connect)

(define (migrate-db db)
  (query-exec db "create table if not exists ratings (userid bigint primary key, rating bigint not null default 1)"))

(define rating-db (make-parameter (init-rating-db 'memory)))

(define (shutdown-rating-db)
  (disconnect (rating-db)))

(define (has-message? update) (hash-has-key? update 'message))

(define (reply-message? update)
  (and (has-message? update)
       (hash-has-key? (hash-ref update 'message) 'reply_to_message)))

; nested hash-ref access syntax
(define-syntax (hash-ref* stx)
  (syntax-case stx ()
    [(_ hsh [k1]) #'(hash-ref hsh k1)]
    [(_ hsh [k1] f) #'(hash-ref hsh k1 f)]
    [(_ hsh [k1 k2 ...]) #'(hash-ref* (hash-ref hsh k1) [k2 ...])]
    [(_ hsh [k1 k2 ...] f) #'(hash-ref* (hash-ref hsh k1 #hasheq()) [k2 ...] f)]))

(define (handle-reply update)
  (define message-text (hash-ref* update ['message 'text] ""))
  (define chat-id (hash-ref* update ['message 'chat 'id]))
  (define from-user-id (hash-ref* update ['message 'from 'id]))
  (define reply-user-id (hash-ref* update ['message 'reply_to_message 'from 'id]))
  (define reply-user-firstname (hash-ref* update ['message 'reply_to_message 'from 'first_name]))
  (when (should-reaction? message-text)
    (if (equal? from-user-id reply-user-id)
        (report-no-self-scoring chat-id)
        (begin
          (cond [(like? message-text) (increase-rating! reply-user-id)]
                [(dislike? message-text) (decrease-rating! reply-user-id)])
          (report-rating chat-id reply-user-id reply-user-firstname)))))

(define (like? text) (regexp-match? #rx"^[+👍].*" text))
(define (dislike? text) (regexp-match? #rx"^[-👎].*" text))
(define (should-reaction? text) (or (like? text) (dislike? text)))

(define (increase-rating! user-id)
  (query-exec
   (rating-db)
   "insert into ratings (userid) values (?) on conflict(userid) do update set rating = rating + 1"
   user-id))

(define (decrease-rating! user-id)
  (query-exec
   (rating-db)
   "insert into ratings (userid) values (?) on conflict(userid) do update set rating = rating - 1"
   user-id))

(define (format-user-rating user-id user-name rating)
  (format "<b>User <a href=\"tg://user?id=~a\">~a</a> has rating ~a now</b>" user-id user-name rating))

(define (get-user-rating user-id)
  (query-value (rating-db) "select rating from ratings where userid = ?" user-id))

(define (report-no-self-scoring chat-id)
  (send-message chat-id (format "<b>Public masturbation is prohibited</b>")))

(define (report-rating chat-id user-id user-firstname)
  (define user-rating (get-user-rating user-id))
  (send-message chat-id (format-user-rating user-id user-firstname user-rating)))
