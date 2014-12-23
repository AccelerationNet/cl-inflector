(defpackage :cl-inflector
  (:use :cl :cl-ppcre)
  (:nicknames inflector)
  (:import-from cl-inflector.langs
   :available-languages
   :current-language
   :current-language-name
   :set-language!
   :irregulars
   :plurals
   :uncountables
   :singulars)
  (:export
   :pluralize
   :plural-of
   :singularize
   :singular-of
   :symbol-plural-of
   :irregular?
   :irregular
   :uncountable?
   :uncountable
   :available-languages
   :current-language
   :current-language-name
   :set-language!)
  (:documentation "Package with function to pluralize/singularize words."))
(in-package :cl-inflector)

(defun uncountable (word)
  "Notifies the inflector that a word is uncountable."
  (pushnew word (uncountables (current-language)) :test #'string-equal))

(defun uncountable? (word)
  "Checks whether a given `word' is uncountable."
  (member word (uncountables (current-language)) :test #'string-equal))

(defun irregular (singular plural)
  "Adds a irregular single-plural set to the irregular list"
  (push (cons singular plural) (irregulars (current-language))))

(defun irregular-plural? (word)
  "Tests if a given `word' is an irregular word in its plural form."
  (rassoc word (irregulars (current-language)) :test #'string-equal))

(defun irregular-singular? (word)
  "Tests if a given `word' is an irregular word in its singular form."
  (car (assoc word (irregulars (current-language)) :test #'string-equal)))

(defun irregular? (word)
  "Tests if a given `word' is irregular, plural or singular."
  (or (irregular-singular? word)
      (irregular-plural? word)))

(defun get-irregular-singular (plural)
  "Makes sure that a `plural' word is on its singular form. If it is already singular, returns it. If it is not irregular, returns nil."
  (if (irregular-singular? plural)
      plural
      (car (rassoc plural (irregulars (current-language)) :test #'string-equal))))

(defun get-irregular-plural (singular)
  "Makes sure that a `singular' word is on its plural form. If it is already plural, returns it. If it is not irregular, returns nil."
  (if (irregular-plural? singular)
      singular
      (cdr (assoc singular (irregulars (current-language)) :test #'string-equal))))

(defun plural (rule replacement)
  "Adds a plural rule, where RULE can be either a string or a regex, and REPLACEMENT can contain capture references defined in RULE"
  (push (list rule replacement) (plurals (current-language))))

(defun plural-of (word)
  "Returns the plural of a word if it's singular, or itself if already plural"
  (let ((word (typecase word
                (string word)
                (T (princ-to-string word)))))

    (cond ((uncountable? word) word)
          ((irregular?   word) (get-irregular-plural word))
          (t (inflector-helper word (plurals (current-language)))))))

(defun singular (rule replacement)
  "Adds a singular rule, where RULE can be either a string or a regex, and REPLACEMENT can contain capture references defined in RULE"
  (push (list rule replacement) (singulars (current-language))))

(defun singular-of (word)
  "Returns the singular of a word if it's singular, or itself if already singular"
  (let ((word (typecase word
                (string word)
                (T (princ-to-string word)))))
    (cond ((uncountable? word) word)
          ((irregular?   word) (get-irregular-singular word))
          (t (inflector-helper word (singulars (current-language)))))))

(defun symbol-singular-of (word &key (package *package*))
  "Creates a symbol with the singular version of the given `word'."
  (intern (string-upcase (singular-of word)) package))

(defun symbol-plural-of (word &key (package *package*))
  "Creates a symbol with the plural version of the given `word'."
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
  "If `count' is greater than 1, returns the `plural' arg if provided, otherwise pluralizes the given `word'."
  (if (not (= count 1))
      (or plural
          (plural-of word))
      word))
