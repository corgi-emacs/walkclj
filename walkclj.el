;;; walkclj.el --- Manipulate Clojure parse trees
;;
;; Filename: walkclj.el
;; Author: Arne Brasseur
;; Maintainer: Arne Brasseur
;; Created: Mi Jul 18 09:39:10 2018 (+0200)
;; Version: 0.1.0
;; Package-Requires: ((emacs "25") (parseclj "20180602.1306") (treepy "20170722.355"))
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL: https://github.com/plexus/walkclj
;; Keywords: languages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; A complementary library to parseclj. Experimental.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'treepy)
(require 'parseclj)

(defvar walkclj-function-names '(ffirst
                                 first
                                 keyword?
                                 last
                                 list?
                                 map?
                                 meta
                                 name
                                 second
                                 symbol?
                                 unwrap-meta))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun walkclj-current-ns ()
  (save-excursion
    (goto-char 1)
    (let ((ns-form (parseclj-parse-clojure :read-one t)))
      (walkclj-do
       (when (and (list? ns-form) (eq 'ns (a-get (first ns-form) :value)))
         (name (unwrap-meta (second ns-form))))))))

;; (equal "foo"
;;        (walkclj-do (name (first (parseclj-parse-clojure "foo")))))

;; (equal 'the-sym
;;        (a-get
;;         (walkclj-do (unwrap-meta (first (parseclj-parse-clojure "^{} the-sym"))))
;;         :value))

;; (setq ast
;;       (with-temp-buffer
;;         (insert-file-contents "/home/arne/github/clojure/src/clj/clojure/core.clj")
;;         (goto-char (point-min))
;;         (parseclj-parse-clojure :read-one t)))

(provide 'walkclj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; walkclj.el ends here
