# walkclj

Walkclj is a complimentary library to
[parseclj](https://github.com/clojure-emacs/parseclj/) and
[parseedn](https://github.com/clojure-emacs/parseedn/). These Clojure/EDN
parsing libraries return a parse tree, this library lets you navigate that parse
tree as if it was a regular clojure/edn data structure.

For example when parsing `(foo bar)` you get

``` emacs-lisp
(setq parse-tree
      ;; (parseclj-parse-clojure "(foo bar)")
      '((:node-type . :root)
        (:position . 1)
        (:children ((:node-type . :list)
                    (:position . 1)
                    (:children ((:node-type . :symbol)
                                (:position . 2)
                                (:form . "foo")
                                (:value . foo))
                               ((:node-type . :symbol)
                                (:position . 6)
                                (:form . "bar")
                                (:value . bar)))))))

;; There's an extra wrapping because parseclj accepts multiple top-level forms

(walkclj-ffirst parse-tree)
;;=> ((:node-type . :symbol) (:position . 2) (:form . "foo") (:value . foo))

(walkclj-list-p (walkclj-first parse-tree)) ;;=> t

(walkclj-name (walkclj-ffirst parse-tree)) ;;=>"foo"
```

We currently implement the following functions, all match their Clojure equivalent:

- `(walkclj-ffirst form)`
- `(walkclj-first form)`
- `(walkclj-list-p form)`
- `(walkclj-meta form)`
- `(walkclj-name form)`
- `(walkclj-second form)`
- `(walkclj-symbol-p form)`

There's a macro `walkclj-do` which lets you omit the `walkclj-` prefix, and also
allows writing Clojure-style keyword lookups:

```emacs-lisp
(macroexpand-1 '
 (walkclj-do
  (:foo (list? (first parse-tree))))) 
;; => (a-get (walkclj-list-p (walkclj-first parse-tree)) :foo)
```

Two special helpers:

- `(walkclj-unwrap-meta form)`: Optionally unwrap a `:meta` type parse node. Any
  Clojure literal that has metadata will get an extra wrapper in the parse tree,
  this unwraps that, if it's there.

- `(walkclj-current-ns)`: Get the namespace name of the current buffer, assumes
  the first form in the buffer is an `ns` form

Patches are welcome! Any function that mimics a clojure.core function is fair game.

<!-- license -->
## License

Copyright &copy; 2018-2022 Arne Brasseur and Contributors

Licensed under the term of the GNU General Public License, version 3. 
<!-- /license -->
