(def global 12)
(def foo (fn [a b] (+ a b)))
(def bar (fn [a] (+ a global)))

(print "hello" "world")
(print "foo:" (foo 1 2) " bar:" (bar 1))
