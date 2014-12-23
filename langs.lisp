(defpackage :cl-inflector.langs
  (:use :cl)
  (:export
   :language
   :*language*
   :*current-language*
   :available-languages
   :current-language
   :current-language-name
   :set-language!
   :irregulars
   :plurals
   :uncountables
   :singulars)
  (:documentation "Package with the language class and its methods and a few functions to handle the language class."))
(in-package :cl-inflector.langs)

(defclass language ()
  ((name :accessor name :initarg :name :initform nil
         :documentation "Name of the language as a keyword.")
   (plurals :accessor plurals :initarg :plurals :initform nil
            :documentation "Alist with (singular-matching-regex plural-equiv).")
   (singulars :accessor singulars :initarg :singulars :initform nil
              :documentation "Alist with (plural-matching-regex singular-equiv).")
   (uncountables :accessor uncountables :initarg :uncountables :initform nil
                 :documentation "List with uncountable words.")
   (irregulars :accessor irregulars :initarg :irregulars :initform nil
               :documentation "Alist with irregular words pairs."))
  (:documentation "Language object to hold all lists with regexps, irregulars and uncountable words."))

(defparameter +en_US+
  (make-instance
   'language
   :name :en_US
   :plurals '(("(.*[aeiou])z$"             "\\1zzes")
              ("^(ox)$"                    "\\1en")
              ("([m|l])ouse$"              "\\1ice")
              ("(matr|vert|ind)(?:ix|ex)$" "\\1ices")
              ("(z|x|ch|ss|sh)$"             "\\1es")
              ("([^aeiouy]|qu)y$"          "\\1ies")
              ("(hive)$"                   "\\1s")
              ("(?:([^f])fe|([lr]|thie|loa|lea)f)$" "\\1\\2ves")
              ("sis$"                      "ses")
              ("([ti])um$"                 "\\1a")
              ("(buffal|tomat)o$"          "\\1oes")
              ("(bu)s$"                    "\\1ses")
              ("(alias|status)$"           "\\1es")
              ("(octop)us$"                "\\1uses")
              ("(vir)us$"                  "\\1i")
              ("(ax|test)is$"              "\\1es")
              ("s$"                        "s")
              ("$"                         "s"))

   :singulars '(("(database)s$"        "\\1")
                ("(.*[aeiou]z)zes$"    "\\1")
                ("(matr)ices$"         "\\1ix")
                ("(vert|ind)ices$"     "\\1ex")
                ("^(ox)en"             "\\1")
                ("(alias|status)es$"   "\\1")
                ("(octop)(odes|uses)$" "\\1us")
                ("(octop|vir)i$"       "\\1us")
                ("(cris|ax|test)es$"   "\\1is")
                ("(shoe)s$"            "\\1")
                ("(o)es$"              "\\1")
                ("(bus)es$"            "\\1")
                ("([m|l])ice$"         "\\1ouse")
                ("(z|x|ch|ss|sh)es$"     "\\1")
                ("(m)ovies$"           "\\1ovie")
                ("(s)eries$"           "\\1eries")
                ("([^aeiouy]|qu)ies$"  "\\1y")
                ;; http://en.wiktionary.org/wiki/Category:English_irregular_plurals_ending_in_%22-ves%22
                ("(thie|lea|loa)ves$"      "\\1f")
                ("([lr])ves$"          "\\1f")
                ("([^f])ves$"          "\\1fe")

                ("(tive)s$"            "\\1")
                ("(hive)s$"            "\\1")
                ("(^analy)ses$"        "\\1sis")
                ("((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)ses$" "\\1\\2sis")
                ("([ti])a$"           "\\1um")
                ("(n)ews$"            "\\1ews")
                ("s$" ""))

   :uncountables (list "equipment" "information" "rice" "money" "species"
                       "series" "fish" "sheep" "jeans" "news")

   :irregulars (alexandria:plist-alist
                (list
                 "is"     "are"
                 "person" "people"
                 "man"    "men"
                 "woman"  "women"
                 "child"  "children"
                 "move"   "moves"
                 "movie"  "movies"
                 "buzz"   "buzzes")))
  "Adapted *cough*ripped*cough* from rails inflector.rb singular->plurals regular expressions")

(defparameter +pt_BR+
  (make-instance
   'language
   :name :pt_BR
   :plurals '(("^(alem|c|p)ão" "\\1ães")
              ("^(irm|m)ão$" "\\1ãos")
              ("ão$" "ões")
              ("^(|g)ás$" "\\1ases")
              ("^(japon|escoc|ingl|dinamarqu|fregu|portug)ês$" "\\1eses")
              ("m$" "ns")
              ("([^aeou])il$" "\\1is")
              ("ul$" "uis")
              ("ol$" "óis")
              ("el$" "éis")
              ("al$" "ais")
              ("(z|r)$" "\\1es")
              ("^(paí)s$" "\\1ses")
              ("(s)$" "\\1")
              ("$" "s"))

   :singulars '(("^(g|)ases$" "\\1ás")
                ("(japon|escoc|ingl|dinamarqu|fregu|portug)eses$" "\\1ês")
                ("ões$" "ão")
                ("ãos$" "ão")
                ("ães$" "ão")
                ("^(.*[^s]s)es$" "\\1")
                ("sses$" "sse")
                ("ns$" "m")
                ("(r|t|f|v)is$" "\\1il")
                ("uis$" "ul")
                ("óis$" "ol")
                ("éis$" "el")
                ("([^p])ais$" "\\1al")
                ("(r|z)es" "\\1")
                ("^(á|gá|paí)s$" "\\1s")
                ("([^ê])s$" "\\1"))

   :uncountables (list "tórax" "tênis" "ônibus" "lápis" "fênix")

   :irregulars (alexandria:plist-alist
                (list "é" "são" "tem" "têm"))))

(defparameter +languages+
  (list +en_us+ +pt_br+)
  "List of all available languages objects.")

(defparameter *current-language* +en_US+
  "Current language used. Defaults to en_US.")

(defun available-languages ()
  "Returns the name of all available languages."
  (mapcar #'name +languages+))

(defun set-language! (lang &optional (clone? nil))
  "Sets the current language, if clone? then set to a copy of the current language
   this is useful - eg in testing or creating custom dictionaries"
  (setf lang
        (etypecase lang
          (symbol (find lang +languages+ :key #'name))
          (language lang)))
  (unless (member lang +languages+)
    (error "Unsupported language"))
  (setf *current-language*
        (if clone?
            (make-instance 'language
                           :name (list (name lang) 'copy)
                           :singulars (copy-list (singulars lang))
                           :plurals (copy-list (plurals lang))
                           :uncountables (copy-list (uncountables lang))
                           :irregulars (copy-list (irregulars lang)))
            lang))
  *current-language*)

(defun current-language ()
  "Returns the current language object."
  *current-language*)

(defun current-language-name ()
  "Returns the name of the current language."
  (name (current-language)))
