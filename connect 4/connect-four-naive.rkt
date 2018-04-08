#lang racket

(require json
         racket/os)

;;; (valid-moves precept) -> (listof exact-nonnegative-integer?)
;;;   percept : jsexpr?
;;; Returns a list of the valid moves - that is columns that aren't full.
(define (valid-moves precept)
  (match precept
    ((hash-table ('player player) ('height height) ('width width) ('grid grid))
     (for/fold ((moves '()))
               ((col (in-list grid))
                (i (in-naturals)))
       (cond ((= 0 (first col))
              (append moves (list i)))
             (else
              moves))))))

;;; (main) -> void?
(define (main)
  (displayln "Connect Four" (current-error-port))
  ;; Seed the current pseudo-random number generator with a value that will be
  ;; different for different player programs
  (random-seed (modulo (abs (* (getpid) (current-milliseconds)))
                       (sub1 (expt 2 31))))
  ;; Process the precepts.
  (for ((json (in-lines)))
    (displayln json (current-error-port))
    (with-handlers ((exn:fail?
                     (lambda (e) (displayln "null"))))
      (define precept (string->jsexpr json))
      ;; Find the valid moves.
      (define moves (valid-moves precept))
      ;; Choose one at random.
      (define move (list-ref moves (random (length moves))))
      ;; Return the action.
      (define action
        (hasheq 'move move))
      (define action-json (jsexpr->string action))
      (displayln action-json (current-error-port))
      (displayln action-json)
      (flush-output))))

;;; Start the program.
(main)
