(cl:in-package :gauche-compat.text.tr.internal)

(def-suite gauche-compat.text.tr)

(in-suite gauche-compat.text.tr)

(defmacro test* (name expect expr)
  `(test ,(gensym name)
     (is (equal ,expect ,expr))))

;;
;; testing text.tr
;;

(test* "basic" "hELLO, wORLD!"
       (string-tr "Hello, World!" "A-Za-z" "a-zA-Z"))

(test* "repeat" "h????, w????!"
       (string-tr "Hello, World!" "A-Za-z" "a-z?*"))
(test* "repeat" "h????, w????!"
       (string-tr "Hello, World!" "A-Za-z" "a-z?*0"))
(test* "repeat" "h???!, w!!??!"
       (string-tr "Hello, World!" "A-Za-z" "a-z?*13!*13"))
(test #.(gensym "repeat - error")
      (signals (cl:error)
        (string-tr "Hello, World!" "A*10" "a-z?*13!*13")))
(test* "delete" ", !"
       (string-tr "Hello, World!" "A-Za-z" "" :delete 'T))
(test* "delete" "H, W!"
       (string-tr "Hello, World!" "a-z" "" :delete 'T))
(test* "delete" "h, w!"
       (string-tr "Hello, World!" "A-Za-z" "a-z" :delete 'T))
(test* "complement" "Hello??World?"
       (string-tr "Hello, World!" "A-Za-z" "?*" :complement 'T))


(test* "complement" "H??????W?????"
       (string-tr "Hello, World!" "A-Z" "?*" :complement 'T))
(test* "complement & delete" "HelloWorld"
       (string-tr "Hello, World!" "A-Za-z" ""
                  :complement 'T :delete 'T))
(test* "squeeze" "helo,   world!!!!"
       (string-tr "Hello,   World!!!!" "A-Za-z" "a-z" :squeeze 'T))
(test* "squeeze & complement" "Hello, World!"
       (string-tr "Hello,   World!!!!" "A-Za-z" ""
                  :squeeze 'T :complement 'T))

(test* "spec edge case (-)" "-bacdaef"
       (string-tr "Ab-cd-ef" "A-" "-a"))
(test* "spec edge case (\\)" "a/b"
       (string-tr "a\\b" "\\\\" "/"))
(test #.(gensym "spec edge case (\\)") 
  (signals (cl:error)
    (string-tr "a\\b" "\\" "/")))


;; whole test over smaller table size
(test* "basic, table-size" "hELLO, wORLD!"
       (string-tr "Hello, World!" "A-Za-z" "a-zA-Z" :table-size 65))
(test* "repeat, table-size" "h????, w????!"
       (string-tr "Hello, World!" "A-Za-z" "a-z?*" :table-size 66))
(test* "repeat, table-size" "h????, w????!"
       (string-tr "Hello, World!" "A-Za-z" "a-z?*0" :table-size 98))
(test* "repeat, table-size" "h???!, w!!??!"
       (string-tr "Hello, World!" "A-Za-z" "a-z?*13!*13" :table-size 99))
(test* "delete, table-size" ", !"
       (string-tr "Hello, World!" "A-Za-z" ""
                  :delete 'T :table-size 32))
(test* "delete, table-size" "H, W!"
       (string-tr "Hello, World!" "a-z" ""
                  :delete 'T :table-size 64))
(test* "delete, table-size" "h, w!"
       (string-tr "Hello, World!" "A-Za-z" "a-z"
                  :delete 'T :table-size 68))
(test* "complement, table-size" "Hello??World?"
       (string-tr "Hello, World!" "A-Za-z" "?*"
                  :complement 'T :table-size 87))
(test* "complement, table-size" "H??????W?????"
       (string-tr "Hello, World!" "A-Z" "?*"
                  :complement 'T :table-size 2))
(test* "complement & delete, table-size" "HelloWorld"
       (string-tr "Hello, World!" "A-Za-z" ""
                  :complement 'T :delete 'T :table-size 70))
(test* "squeze, table-size" "helo,   world!!!!"
       (string-tr "Hello,   World!!!!" "A-Za-z" "a-z"
                  :squeeze 'T :table-size 65))
(test* "squeeze & complement, table-size" "Hello, World!"
       (string-tr "Hello,   World!!!!" "A-Za-z" ""
                  :squeeze 'T :complement 'T :table-size 103))

(test* "escape in spec" "*ello, World!"
       (string-tr "Hello,-World!" "A\\-H" "_ \\*"))

;;; eof
