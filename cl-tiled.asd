(defsystem #:cl-tiled
  :name "cl-tiled"
  :version "0.2.2"
  :description "Tiled (http://www.mapeditor.org/) Loader"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "zlib/libpng License <http://opensource.org/licenses/zlib-license.php>"
  :pathname "src"
  :components
  ((:file "data-types")
   (:file "impl")
   (:file "tiled-xml" :depends-on ("data-types" "impl"))
   (:file "tiled-json" :depends-on ("data-types" "impl"))
   (:file "tiled" :depends-on ("data-types" "impl" "tiled-xml" "tiled-json")))
  :depends-on
  (#:alexandria
   #:chipz
   #:cl-base64
   #:cl-json
   #:nibbles
   #:parse-float
   #:split-sequence
   #:uiop
   #:xmls))
