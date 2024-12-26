#lang sicp

(define (liars-puzzle)
  (let ((e1 (amb '(kitty 2) '(betty 3)))
        (e2 (amb '(ethel 1) '(joan 2)))
        (e3 (amb '(joan 3) '(ethel 5)))
        (e4 (amb '(kitty 2) '(mary 4)))
        (e5 (amb '(mary 4) '(betty 1))))))

