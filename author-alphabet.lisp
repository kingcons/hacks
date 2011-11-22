;; Honestly, this doesn't even close to count as a hack.
;; This is just for giggles.

(defpackage :author-alphabet
  (:use :cl))

(in-package :author-alphabet)

(defvar *authors*
  '((#\a '("Abelson" "Appel"))
    (#\b '("Bentley" "Bird" "Brodie" "Bryant"))
    (#\c '("Cooper" "Cormen" "Crockford"))
    (#\d '("Dasgupta" "Date" "Dijkstra"))
    (#\e '("Emerick" "Evans"))
    (#\f '("Felleisen" "Findler" "Flatt" "Fogus" "Friedman"))
    (#\g '("Goerzen" "Goldman" "Graham"))
    (#\h '("Haridi" "Haynes" "Hennessy" "Houser" "Hoyte"))
    (#\i '("Ingo")) ;; shite molnar
    (#\j '("Jackson" "Jones"))
    (#\k '("Keene" "Kernighan" "Kiczales" "Kleinberg" "Knuth" "Krishnamurthi"))
    (#\l '("Leiserson" "Levine" "Levitin" "Lions" "Lipovaca"))
    (#\m '("Meyer"))
    (#\n '("Norvig"))
    (#\o '("O'Hallaron" "O'Sullivan" "Okasaki"))
    (#\p '("Papadimitriou" "Patterson" "Pierce" "Pike"))
    (#\q '("Quiennec"))
    (#\r '("Raskin" "Resig" "Ritchie" "Rivest" "Russell"))
    (#\s '("Seibel" "Skiena" "Stein" "Stewart" "Sussman"))
    (#\t '("Tanenbaum" "Tardos" "Torczon"))
    (#\u '("Urs")) ;; shite hoetzle
    (#\v '("Valhalia" "Van Der Linden" "Van Roy" "Vazirani"))
    (#\w '("Wand" "Warren"))
    (#\x '("xerxes")) ;; SHIT
    (#\y '("yarislav")) ;; SHIT
    (#\z '("Zeller"))))

(defun author-matches (string)
  (loop for char across string
     collecting (let ((matches (rest (assoc (char-downcase char) *authors*))))
                  (nth (random (length matches)) matches))))
