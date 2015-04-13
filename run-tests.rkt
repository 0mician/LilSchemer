(require rackunit/text-ui)
(require "atom-tests.rkt")
(require "arithmetic-tests.rkt")
;;; (require "list-processing-tests.rkt")

(displayln "running tests on 'atom.scm': ")
(run-tests atom-tests)
(displayln "running tests on 'arithmetic.scm': ")
(run-tests arithmetic-tests)
