;;;Copyright (c) 2017 Wilfredo Velázquez-Rodríguez
;;;
;;;This software is provided 'as-is', without any express or implied
;;;warranty. In no event will the authors be held liable for any damages
;;;arising from the use of this software.
;;;
;;;Permission is granted to anyone to use this software for any purpose,
;;;including commercial applications, and to alter it and redistribute
;;;it freely, subject to the following restrictions:
;;;
;;;1. The origin of this software must not be misrepresented; you must not
;;;   claim that you wrote the original software. If you use this software
;;;   in a product, an acknowledgment in the product documentation would
;;;   be appreciated but is not required.
;;;
;;;2. Altered source versions must be plainly marked as such, and must not
;;;   be misrepresented as being the original software.
;;;
;;;3. This notice may not be removed or altered from any source distribution.

(defpackage #:cl-tiled.impl
  (:use #:alexandria #:cl))

(cl:in-package #:cl-tiled.impl)

(defstruct tproperty
  (name "" :type string)
  (type "" :type string)
  (value "" :type string))

(defstruct tiled-color
  (r #x00 :type (unsigned-byte 8))
  (g #x00 :type (unsigned-byte 8))
  (b #x00 :type (unsigned-byte 8))
  (a #xFF :type (unsigned-byte 8)))

(deftype timage-encoding ()
  '(member :base64))

(deftype tcompression ()
  '(member nil :gzip :zlib))

(defstruct timage-data
  (encoding :base64 :type timage-encoding)
  (compression nil :type tcompression)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) *)))

(defstruct timage
  (format nil :type (or null string))
  (source nil :type (or null string))
  (transparent-color nil :type (or null tiled-color))
  (width nil :type (or null integer))
  (height nil :type (or null integer))
  (image-data nil :type (or null timage-data)))

(defstruct tterrain
  (name "" :type string)
  (tile 0 :type integer)
  (properties () :type list))

(defstruct tellipse)

(defstruct tpoint
  (x 0 :type real)
  (y 0 :type real))

(defstruct tpolygon
  (points () :type list))

(defstruct tpolyline
  (points () :type list))

(deftype halign ()
  '(member :left :center :right))

(deftype valign ()
  '(member :top :center :bottom))

(defstruct ttext
  (text "" :type string)
  (font-family nil :type (or null string))
  (pixel-size nil :type (or null integer))
  (wrap nil :type boolean)
  (color nil :type (or null tiled-color))
  (bold nil :type boolean)
  (italic nil :type boolean)
  (underline nil :type boolean)
  (strikeout nil :type boolean)
  (kerning t :type boolean)
  (halign nil :type (or null halign))
  (valign nil :type (or null valign)))

(defstruct tobject
  (id 0 :type integer)
  (name "" :type string)
  (type "" :type string)
  (x 0 :type integer)
  (y 0 :type integer)
  (width nil :type (or null integer))
  (height nil :type (or null integer))
  (rotation nil :type (or null real))
  (gid nil :type (or null integer))
  (visible t :type boolean)
  (properties () :type list)
  (ellipse nil :type (or null tellipse))
  (polygon nil :type (or null tpolygon))
  (polyline nil :type (or null tpolyline))
  (text nil :type (or null ttext))
  (image nil :type (or null timage)))

(deftype tdraw-order ()
  '(member :index :topdown))

(defstruct tobject-group
  (name "" :type string)
  (color nil :type (or null tiled-color))
  (x nil :type (or null integer))
  (y nil :type (or null integer))
  (width nil :type (or null integer))
  (height nil :type (or null integer))
  (opacity 1.0 :type real)
  (visible t :type boolean)
  (offset-x 0 :type integer)
  (offset-y 0 :type integer)
  (draw-order nil :type (or null tdraw-order))
  (properties () :type list)
  (objects () :type list))

(defstruct tframe
  (tile-id 0 :type integer)
  (duration 0 :type integer))

(defstruct ttileset-tile
  (id 0 :type integer)
  (type "" :type string)
  (terrain #(nil nil nil nil) :type (simple-vector 4))
  (probability nil :type (or null real))
  (properties () :type list)
  (image nil :type (or null timage))
  (object-group nil :type (or null tobject-group))
  (frames () :type list))

(defstruct ttileset
  (first-gid nil :type (or null integer))
  (source nil :type (or null string))
  (name nil :type (or null string))
  (tile-width nil :type (or null integer))
  (tile-height nil :type (or null integer))
  (spacing nil :type (or null integer))
  (margin nil :type (or null integer))
  (tile-count nil :type (or null integer))
  (columns nil :type (or null integer))
  (tile-offset-x nil :type (or null integer))
  (tile-offset-y nil :type (or null integer))
  (properties () :type list)
  (image nil :type (or null timage))
  (terrains () :type list)
  (tiles () :type list))

(deftype ttile-encoding ()
  '(member nil :csv :base64))

(defstruct ttile-data
  (encoding :csv :type ttile-encoding)
  (compression nil :type tcompression)
  (tiles () :type list))

(defstruct ttile-layer
  (name "" :type string)
  (x nil :type (or null integer))
  (y nil :type (or null integer))
  (width 0 :type integer)
  (height 0 :type integer)
  (opacity 1.0 :type real)
  (visible t :type boolean)
  (offset-x 0 :type integer)
  (offset-y 0 :type integer)
  (properties () :type list)
  (tile-data nil :type ttile-data))

(defstruct timage-layer
  (name "" :type string)
  (offset-x 0 :type integer)
  (offset-y 0 :type integer)
  (x nil :type (or null integer))
  (y nil :type (or null integer))
  (opacity 1.0 :type real)
  (visible t :type boolean)
  (properties () :type list)
  (image nil :type (or null timage)))

(defstruct tlayer-group
  (name "" :type string)
  (offset-x 0 :type integer)
  (offset-y 0 :type integer)
  (x nil :type (or null integer))
  (y nil :type (or null integer))
  (opacity 1.0 :type real)
  (visible t :type boolean)
  (properties () :type list)
  (layers () :type list))

(deftype torientation ()
  '(member :orthogonal :isometric :staggered :hexagonal))

(deftype trender-order ()
  '(member :right-down :right-up :left-down :left-up))

(deftype tstagger-axis ()
  '(member :x :y))

(deftype tstagger-index ()
  '(member :even :odd))

(defstruct tmap
  (version "" :type (or null string))
  (tiled-version "" :type (or null string))
  (orientation :orthogonal :type (or null torientation))
  (render-order nil :type (or null trender-order))
  (width 0 :type integer)
  (height 0 :type integer)
  (tile-width 0 :type integer)
  (tile-height 0 :type integer)
  (hex-side-length nil :type (or null integer))
  (stagger-axis nil :type (or null tstagger-axis))
  (stagger-index nil :type (or null tstagger-index))
  (background-color nil :type (or null tiled-color))
  (next-object-id 0 :type integer)
  (properties () :type list)
  (tilesets () :type list)
  (layers () :type list))

(defun parse-color-string (color-str &optional (default nil))
  (cond
    ((or (not (typep color-str 'string))
         (< (length color-str) 6))
     default)
    ((= (length color-str) 6)
     (make-tiled-color
      :r (parse-integer color-str :start 0 :end 2 :radix 16)
      :g (parse-integer color-str :start 2 :end 4 :radix 16)
      :b (parse-integer color-str :start 4 :end 6 :radix 16)))
    ((= (length color-str) 7)
     (make-tiled-color
      :r (parse-integer color-str :start 1 :end 3 :radix 16)
      :g (parse-integer color-str :start 3 :end 5 :radix 16)
      :b (parse-integer color-str :start 5 :end 7 :radix 16)))
    ((= (length color-str) 8)
     (make-tiled-color
      :a (parse-integer color-str :start 0 :end 2 :radix 16)
      :r (parse-integer color-str :start 2 :end 4 :radix 16)
      :g (parse-integer color-str :start 4 :end 6 :radix 16)
      :b (parse-integer color-str :start 6 :end 8 :radix 16)))
    ((= (length color-str) 9)
     (make-tiled-color
      :a (parse-integer color-str :start 1 :end 3 :radix 16)
      :r (parse-integer color-str :start 3 :end 5 :radix 16)
      :g (parse-integer color-str :start 5 :end 7 :radix 16)
      :b (parse-integer color-str :start 7 :end 9 :radix 16)))
    (t
     default)))

(defun parse-image-encoding-string (encoding)
  (eswitch (encoding :test 'equalp)
    (nil nil)
    ("base64" :base64)))

(defun parse-compression-string (compression)
  (eswitch (compression :test 'equalp)
    (nil nil)
    ("zlib" :zlib)
    ("gzip" :gzip)))

(defun parse-tile-encoding-string (encoding)
  (eswitch (encoding :test 'equalp)
    (nil nil)
    ("csv" :csv)
    ("base64" :base64)))

(defun parse-halign (halign)
  (eswitch (halign :test 'equalp)
    (nil nil)
    ("left" :left)
    ("center" :center)
    ("right" :right)))

(defun parse-valign (valign)
  (eswitch (valign :test 'equalp)
    (nil nil)
    ("top" :top)
    ("center" :center)
    ("bottom" :bottom)))

(defun parse-draw-order (draw-order)
  (eswitch (draw-order :test 'equalp)
    (nil nil)
    ("index" :index)
    ("topdown" :topdown)))

(defun parse-orientation (orientation)
  (eswitch (orientation :test 'equalp)
    (nil nil)
    ("orthogonal" :orthogonal)
    ("isometric" :isometric)
    ("staggered" :staggered)
    ("hexagonal" :hexagonal)))

(defun parse-render-order (render-order)
  (eswitch (render-order :test 'equalp)
    (nil nil)
    ("right-down" :right-down)
    ("right-up" :right-up)
    ("left-down" :left-down)
    ("left-up" :left-up)))

(defun parse-stagger-axis (stagger-axis)
  (eswitch (stagger-axis :test 'equalp)
    (nil nil)
    ("x" :x)
    ("y" :y)))

(defun parse-stagger-index (stagger-index)
  (eswitch (stagger-index :test 'equalp)
    (nil nil)
    ("even" :even)
    ("odd" :odd)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((exported-symbols ()))
    (do-symbols (s)
      (when (eq (symbol-package s) *package*)
        (push s exported-symbols)))
    (export exported-symbols)))