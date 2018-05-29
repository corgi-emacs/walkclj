
(setq ast
      (with-temp-buffer
        (insert-file-contents "/home/arne/github/clojure/src/clj/clojure/core.clj")
        (goto-char (point-min))
        (parseclj-parse-clojure)))


(defvar walkclj-function-names '(
                                 ffirst
                                 first
                                 keyword?
                                 last
                                 list?
                                 map?
                                 meta
                                 name
                                 second
                                 symbol?
                                 unwrap-meta
                                 ))

(defmacro walkclj-do (&rest body)
  (cons 'progn
        (treepy-postwalk (lambda (x)
                           (cond
                            ((member x walkclj-function-names)
                             (intern (concat "walkclj-" (symbol-name x))))

                            ((and (listp x)
                                  (keywordp (car x)))
                             (list* 'a-get (cadr x) (car x) (cddr x)))

                            (t x)))
                         body)))

(defun walkclj-meta (form)
  (if (eq :with-meta (parseclj-ast-node-type form))
      (car (a-get form :children))
    form))

(defun walkclj-unwrap-meta (form)
  (if (eq :with-meta (parseclj-ast-node-type form))
      (cadr (a-get form :children))
    form))

(defun walkclj-first (form)
  (if-let (children (a-get form :children))
      (car children)
    nil))

(defun walkclj-second (form)
  (if-let (children (a-get form :children))
      (cadr children)
    nil))

(defun walkclj-ffirst (form)
  (walkclj-do (first (first form))))

(defun walkclj-list? (form)
  (eq (parseclj-ast-node-type form) :list))

(defun walkclj-symbol? (form)
  (eq (parseclj-ast-node-type form) :symbol))

(defun walkclj-name (form)
  (case (parseclj-ast-node-type form)
    (:string (a-get form :value))
    (:symbol (a-get form :form))
    (:keyword (substring (a-get form :form) 1))))

(equal "foo"
       (walkclj-do (name (first (parseclj-parse-clojure "foo")))))

(equal 'the-sym 
       (walkclj-do (unwrap-meta (first (parseclj-parse-clojure "^{} the-sym")))))

(walkclj-do
 (name (unwrap-meta (second (first ast)))))

(macroexpand-1
 '(walkclj-do
   (:foo (first ast))))

(walkclj-do
 (symbol?
  (ffirst ast)))

(defun value-p (x)
  (and (parseclj-ast-node-p x)
       (not (member (parseclj-ast-node-type x) '(:whitespace :comment)))))
(parseclj-parse-clojure "^  :meta1 ^  :meta2 the-sym"
                        :fail-fast nil
                        :value-p (lambda (x) (and (parseclj-ast-node-p x)
                                                  (not (member (parseclj-ast-node-type x) '(:whitespace :comment))))))
(parseclj-parse-clojure "^  :meta1 ^  :meta2 the-sym" :fail-fast nil)
((:node-type . :root)
 (:position . 1)
 (:children ((:node-type . :symbol)
             (:position . 21)
             (:form . "the-sym")
             (:value . the-sym)
             (:meta (:value . :meta1)
                    (:form . ":meta1")
                    (:position . 4)
                    (:node-type . :keyword)))))

((:node-type . :root)
 (:position . 1)
 (:children ((:token-type . :metadata)
             (:form . "^")
             (:pos . 1))
            ((:node-type . :keyword)
             (:position . 4)
             (:form . ":meta1")
             (:value . :meta1))
            ((:value . the-sym)
             (:form . "the-sym")
             (:position . 21)
             (:node-type . :symbol)
             (:meta (:value . :meta2)
                    (:form . ":meta2")
                    (:position . 14)
                    (:node-type . :keyword)))))a

((:node-type . :root)
 (:position . 1)
 (:children ((:token-type . :metadata)
             (:form . "^")
             (:pos . 1))
            ((:node-type . :keyword)
             (:position . 4)
             (:form . ":meta1")
             (:value . :meta1))
            ((:meta (:node-type . :keyword)
                    (:position . 14)
                    (:form . ":meta2")
                    (:value . :meta2))
             (:node-type . :symbol)
             (:position . 21)
             (:form . "the-sym")
             (:value . the-sym))))


((:node-type . :root)
 (:position . 1)
 (:children ((:node-type . :symbol)
             (:position . 21)
             (:form . "the-sym")
             (:value . the-sym)
             (:meta . ((:node-type . :keyword)
                       (:position . 14)
                       (:form . ":meta2")
                       (:value . :meta2)))
             (:meta (:node-type . :keyword)
                    (:position . 4)
                    (:form . ":meta1")
                    (:value . :meta1)))))
(parseclj-ast-node-p
 '((:meta (:node-type . :keyword) (:position . 14) (:form . ":meta2") (:value . :meta2))
   (:node-type . :symbol)
   (:position . 21)
   (:form . "the-sym")
   (:value . the-sym)))

((:node-type . :root)
 (:position . 1)
 (:children ((:meta (:node-type . :keyword) (:position . 4) (:form . ":meta1") (:value . :meta1)) (:token-type . :metadata) (:form . "^") (:pos . 11)) ((:node-type . :symbol) (:position . 21) (:form . "the-sym") (:value . the-sym))))

((:node-type . :root)
 (:position . 1)
 (:children ((:meta (:node-type . :keyword) (:position . 2) (:form . ":meta1") (:value . :meta1))
             (:token-type . :metadata)
             (:form . "^")
             (:pos . 9))
            ((:node-type . :symbol)
             (:position . 17)
             (:form . "the-sym")
             (:value . the-sym))))

((:node-type . :root)
 (:position . 1)
 (:children ((:meta (:node-type . :keyword) (:position . 2) (:form . ":foo") (:value . :foo))
             (:node-type . :symbol)
             (:position . 7)
             (:form . "bar")
             (:value . bar))))

((:node-type . :root)
 (:position . 1)
 (:children ((:meta (:node-type . :keyword) (:position . 2) (:form . ":xxx") (:value . :xxx))
             (:token-type . :metadata)
             (:form . "^")
             (:pos . 7))
            ((:node-type . :symbol)
             (:position . 13)
             (:form . "bar")
             (:value . bar))))

((:node-type . :root)
 (:position . 1)
 (:children ((:meta . ((:node-type . :keyword) (:position . 2) (:form . ":xxx") (:value . :xxx)))
             (:token-type . :metadata)
             (:form . "^")
             (:pos . 7))
            ((:node-type . :symbol)
             (:position . 13)
             (:form . "bar")
             (:value . bar))))

((:node-type . :root)
 (:position . 1)
 (:children ((:token-type . :metadata)
             (:form . "^")
             (:pos . 1))
            ((:node-type . :keyword)
             (:position . 2)
             (:form . ":xxx")
             (:value . :xxx))
            ((:meta . ((:node-type . :keyword)
                       (:position . 8)
                       (:form . ":foo")
                       (:value . :foo)))
             (:node-type . :symbol)
             (:position . 13)
             (:form . "bar")
             (:value . bar))))

(setq stack (reverse '( ((:token-type . :metadata)
                         (:form . "^")
                         (:pos . 1))
                        ((:node-type . :keyword)
                         (:position . 2)
                         (:form . ":xxx")
                         (:value . :xxx))
                        ((:meta . ((:node-type . :keyword)
                                   (:position . 8)
                                   (:form . ":foo")
                                   (:value . :foo)))
                         (:node-type . :symbol)
                         (:position . 13)
                         (:form . "bar")
                         (:value . bar)))))

- ^:foo bar
- :xxx
- ^

(parseclj--take-token (cddr stack) (lambda (e) (not (parseclj-lex-token-p e)))
                      parseclj-lex--prefix-2-tokens)

(parseclj-lex-token-p (car stack))
(mapcar #'parseclj-lex-token-p stack)

(parseclj--take-value stack #'value-p)
(((:node-type . :keyword) (:position . 2) (:form . ":xxx") (:value . :xxx))
 ((:meta (:node-type . :keyword) (:position . 8) (:form . ":foo") (:value . :foo)) (:node-type . :symbol) (:position . 13) (:form . "bar") (:value . bar)))

(parseclj--take-value (cddr stack) #'value-p)
nil

(top-value-2 (parseclj--take-value (nthcdr (length top-value-1) stack) value-p))
(opening-token (parseclj--take-token (nthcdr (+ (length top-value-1)
                                                (length top-value-2))
                                             stack)
                                     value-p
                                     parseclj-lex--prefix-2-tokens))
new-stack


- OPENING-TOKEN (((:token-type . :metadata) (:form . "^") (:pos . 1)))
- TOP-VALUE-2 (((:node-type . :keyword) (:position . 4) (:form . ":meta1") (:value . :meta1))
               ((:token-type . :metadata) (:form . "^") (:pos . 11)))
- TOP-VALUE-1 (((:node-type . :keyword) (:position . 14) (:form . ":meta2") (:value . :meta2)))
(parseclj-lex-token-p '((:token-type . :metadata) (:form . "^") (:pos . 11)))
