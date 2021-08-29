#lang racket

(require (prefix-in tg: "telegram.rkt")
         (prefix-in r: "rating.rkt"))

(define (handle-update update)
  (when (r:reply-message? update)
    (r:handle-reply update)))

(define (start-bot)
  (with-handlers ([exn:break? stop-bot])
    (tg:on-update handle-update)))

(define (stop-bot _break)
  (r:shutdown-rating-db)
  (displayln (format "~nbye bye")))

(define rating-db-name (make-parameter "rating.sqlite"))
(define token-env-name (make-parameter "TELEGRAM_TOKEN"))
(define (read-token) (getenv (token-env-name)))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "zoometer"
   #:once-each
   [("-e" "--token-env")
    envvar
    "Name of the environment variable containing Telegram token (default TELEGRAM_TOKEN)"
    (token-env-name envvar)]
   [("-r" "--rating-db")
    db-name
    "File name of the rating database (default rating.sqlite)"
    (rating-db-name db-name)]
   #:args ()
   (parameterize ([tg:api-token (or (read-token) "")]
                  [r:rating-db (r:init-rating-db (rating-db-name))])
     (start-bot))))
