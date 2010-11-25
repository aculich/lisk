{-# OPTIONS -F -pgmF lisk #-}
(module :fibs
  (import :system.environment)

  (:: main (:i-o ()))
  (= main (>>= get-args (. print fib read head)))

  (:: test (-> :string (, :int :string)))
  (= test (, 1))

  (:: fib (-> :int :int))
  (= fib 0 0)
  (= fib 1 1)
  (= fib n (+ (fib (- n 1))
              (fib (- n 2)))))