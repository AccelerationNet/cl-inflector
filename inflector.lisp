(defpackage :cl-inflector
  (:use cl cl-ppcre)
  (:import-from cl-inflector.langs
                *plurals*
                *singulars*
                *uncountables*
                *irregulars*
                available-langs
                curr-lang
                set-lang!)
  (:export pluralize
	   plural-of
	   singularize
	   singular-of
           :symbol-plural-of
           :symbol-singular-of
           irregular?
	   irregular
	   uncountable?
	   uncountable
           available-langs
           curr-lang
           set-lang!))

(in-package :cl-inflector)

(defun uncountable (word)
  "Notifies the inflector that a word is uncountable."
  (pushnew word *uncountables* :test #'string-equal))

(defun uncountable? (word)
  (member word *uncountables* :test #'string-equal))

(defun irregular (singular plural)
  "Adds a irregular single-plural set to the irregular list"
  (push (cons singular plural) *irregulars*))

(defun irregular-plural? (word)
  (rassoc word *irregulars* :test #'string-equal))

(defun irregular-singular? (word)
  (car (assoc word *irregulars* :test #'string-equal)))

(defun irregular? (word)
  (or (irregular-singular? word)
      (irregular-plural? word)))

(defun get-irregular-singular (plural)
  (if (irregular-singular? plural)
      plural
      (car (rassoc plural *irregulars* :test #'string-equal))))

(defun get-irregular-plural (singular)
  (if (irregular-plural? singular)
      singular
      (cdr (assoc singular *irregulars* :test #'string-equal))))

(defun plural (rule replacement)
  "Adds a plural rule, where RULE can be either a string or a regex, and REPLACEMENT can contain capture references defined in RULE"
  (push (list rule replacement) *plurals*))

(defun plural-of (word)
  "Returns the plural of a word if it's singular, or itself if already plural"
  (let ((word (typecase word
                (string word)
                (T (princ-to-string word)))))

    (cond ((uncountable? word) word)
          ((irregular?   word) (get-irregular-plural word))
          (t (inflector-helper word *plurals*)))))

(defun singular (rule replacement)
  "Adds a singular rule, where RULE can be either a string or a regex, and REPLACEMENT can contain capture references defined in RULE"
  (push (list rule replacement) *singulars*))

(defun singular-of (word)
  "Returns the singular of a word if it's singular, or itself if already singular"
  (let ((word (typecase word
                (string word)
                (T (princ-to-string word)))))
    (cond ((uncountable? word) word)
          ((irregular?   word) (get-irregular-singular word))
          (t (inflector-helper word *singulars*)))))

(defun symbol-singular-of (word &key (package *package*))
  (intern (string-upcase (singular-of word)) package))

(defun symbol-plural-of (word &key (package *package*))
  (intern (string-upcase (plural-of word)) package))

(defun inflector-helper (word regexes)
  (if (null regexes)
      word
      (multiple-value-bind (string match-found?)
          (cl-ppcre:regex-replace
	   (cl-ppcre:create-scanner
	    (first (first regexes))
	    :case-insensitive-mode T)
	   word (second (first regexes)))
        (if match-found?
            string
            (inflector-helper word (rest regexes))))))

(defun pluralize (count word &optional plural)
  (if (not (= count 1))
      (or plural
          (plural-of word))
      word))
