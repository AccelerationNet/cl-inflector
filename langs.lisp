(defpackage :cl-inflector.langs
  (:use :cl)
  (:export
   :*plurals*
   :*singulars*
   :*uncountables*
   :*irregulars*
   :available-langs
   :curr-lang
   :set-lang!))
(in-package :cl-inflector.langs)

(defparameter *plurals-en-us*
  '(("(.*[aeiou])z$"             "\\1zzes")
    ("^(ox)$"                    "\\1en")
    ("([m|l])ouse$"              "\\1ice")
    ("(matr|vert|ind)(?:ix|ex)$" "\\1ices")
    ("(z|x|ch|ss|sh)$"             "\\1es")
    ("([^aeiouy]|qu)y$"          "\\1ies")
    ("(hive)$"                   "\\1s")
    ("(?:([^f])fe|([lraeiou])f)$" "\\1\\2ves")
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
  "Adapted *cough*ripped*cough* from rails inflector.rb
singular->plurals regular expressions")

(defparameter *singulars-en-us*
  '(("(database)s$"        "\\1")
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
    ("([lraeiou])ves$"     "\\1f")
    ("(tive)s$"            "\\1")
    ("(hive)s$"            "\\1")
    ("([^f])ves$"          "\\1fe")
    ("(^analy)ses$"        "\\1sis")
    ("((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)ses$" "\\1\\2sis")
    ("([ti])a$"           "\\1um")
    ("(n)ews$"            "\\1ews")
    ("s$" "")))

(defparameter *uncountables-en-us*
  (list "equipment" "information" "rice" "money" "species" "series" "fish"
        "sheep" "jeans" "news" ))

(defparameter *irregulars-en-us*
  (alexandria:plist-alist
   (list
    "is"     "are"
    "person" "people"
    "man"    "men"
    "woman"  "women"
    "child"  "children"
    "move"   "moves"
    "movie"  "movies"
    "buzz"   "buzzes")))

(defparameter *plurals-pt-br*
  '(("^(alem|c|p)ão" "\\1ães")
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
    ("$" "s")))

(defparameter *singulars-pt-br*
  '(("^(g|)ases$" "\\1ás")
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
    ("([^ê])s$" "\\1")))

(defparameter *uncountables-pt-br*
  (list "tórax" "tênis" "ônibus" "lápis" "fênix"))

(defparameter *irregulars-pt-br*
  (alexandria:plist-alist
   (list "é" "são"
         "tem" "têm")))

(defparameter *plurals* *plurals-en-us*
  "The language to be used in #'PLURAL and #'PLURAL-OF functions.
It's an alist with regexes in the pattern singular->plural.
Defaults to en_US.")

(defparameter *singulars* *singulars-en-us*
  "The language to be used in #'SINGULAR and #'SINGULAR-OF functions.
It's an alist with regexes in the pattern plural->singular.
Defaults to en_US.")

(defparameter *uncountables* *uncountables-en-us*
  "The list of uncountable words.
Defaults to en_US.")

(defparameter *irregulars* *irregulars-en-us*
  "The alist of irregular words.
Defaults to en_US.")

(defparameter *curr-lang* :en_US
  "Current language used.
Defaults to en_US.")

(defparameter *langs-plist*
  (list :en_US (list *plurals-en-us*
                     *singulars-en-us*
                     *uncountables-en-us*
                     *irregulars-en-us*)
        :pt_BR (list *plurals-pt-br*
                     *singulars-pt-br*
                     *uncountables-pt-br*
                     *irregulars-pt-br*)))

(defun available-langs ()
  (remove-if-not #'keywordp *langs-plist*))

(defun set-lang! (lang)
  (unless (member lang *langs-plist*)
    (error "Unsupported language"))
  (destructuring-bind (plurals singulars uncountables irregulars)
      (getf *langs-plist* lang)
    (setf *curr-lang* lang
          *plurals* plurals
          *singulars* singulars
          *uncountables* uncountables
          *irregulars* irregulars))
  lang)

(defun curr-lang ()
  *curr-lang*)
