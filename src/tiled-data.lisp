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
  (:use
   #:alexandria
   #:cl)
  (:export
   #:tiled-color
   #:tiled-color-a
   #:tiled-color-r
   #:tiled-color-g
   #:tiled-color-b
   #:make-tiled-color

   #:property-type
   #:property-name
   #:property-value
   #:property-value-string

   #:string-property
   #:int-property
   #:float-property
   #:bool-property
   #:color-property
   #:file-property

   #:timage-encoding
   #:tcompression

   #:timage-data
   #:encoding
   #:compression
   #:data
   #:make-timage-data

   #:timage
   #:timage-format
   #:timage-source
   #:timage-transparent-color
   #:timage-width
   #:timage-height
   #:timage-image-data
   #:make-timage

   #:tterrain
   #:tterrain-name
   #:tterrain-tile
   #:tterrain-properties
   #:make-tterrain

   #:tpolygon
   #:tpolygon-points
   #:make-tpolygon

   #:tpolyline
   #:tpolyline-points
   #:make-tpolyline

   #:horizontal-alignment
   #:vertical-alignment

   #:ttext
   #:ttext-text
   #:ttext-font-family
   #:ttext-pixel-size
   #:ttext-wrap
   #:ttext-color
   #:ttext-bold
   #:ttext-italic
   #:ttext-underline
   #:ttext-strikeout
   #:ttext-kerning
   #:ttext-halign
   #:ttext-valign
   #:make-ttext

   #:tobject
   #:tobject-id
   #:tobject-name
   #:tobject-type
   #:tobject-x
   #:tobject-y
   #:tobject-width
   #:tobject-height
   #:tobject-rotation
   #:tobject-gid
   #:tobject-tobject-gid
   #:tobject-visible
   #:tobject-properties
   #:tobject-ellipse
   #:tobject-polygon
   #:tobject-polyline
   #:tobject-text
   #:tobject-image
   #:make-tobject

   #:draw-order

   #:tobject-group
   #:tobject-group-name
   #:tobject-group-color
   #:tobject-group-x
   #:tobject-group-y
   #:tobject-group-width
   #:tobject-group-height
   #:tobject-group-opacity
   #:tobject-group-visible
   #:tobject-group-offset-x
   #:tobject-group-offset-y
   #:tobject-group-draw-order
   #:tobject-group-properties
   #:tobject-group-objects
   #:make-tobject-group

   #:tframe
   #:tframe-tile-id
   #:tframe-duration
   #:make-tframe

   #:ttileset-tile
   #:ttileset-tile-id
   #:ttileset-tile-type
   #:ttileset-tile-terrain
   #:ttileset-tile-probability
   #:ttileset-tile-properties
   #:ttileset-tile-image
   #:ttileset-tile-object-group
   #:ttileset-tile-frames
   #:make-ttileset-tile

   #:ttileset
   #:ttileset-first-gid
   #:ttileset-source
   #:ttileset-name
   #:ttileset-tile-width
   #:ttileset-tile-height
   #:ttileset-spacing
   #:ttileset-margin
   #:ttileset-tile-count
   #:ttileset-columns
   #:ttileset-tile-offset-x
   #:ttileset-tile-offset-y
   #:ttileset-properties
   #:ttileset-image
   #:ttileset-terrains
   #:ttileset-tiles
   #:make-ttileset

   #:ttile-encoding

   #:ttile-data
   #:encoding
   #:compression
   #:tiles
   #:ttile-data-tiles
   #:make-ttile-data

   #:ttile-layer
   #:ttile-layer-name
   #:ttile-layer-x
   #:ttile-layer-y
   #:ttile-layer-width
   #:ttile-layer-height
   #:ttile-layer-opacity
   #:ttile-layer-visible
   #:ttile-layer-offset-x
   #:ttile-layer-offset-y
   #:ttile-layer-properties
   #:ttile-layer-tile-data
   #:make-ttile-layer

   #:timage-layer
   #:timage-layer-name
   #:timage-layer-offset-x
   #:timage-layer-offset-y
   #:timage-layer-x
   #:timage-layer-y
   #:timage-layer-opacity
   #:timage-layer-visible
   #:timage-layer-properties
   #:timage-layer-image
   #:make-timage-layer

   #:tlayer-group
   #:tlayer-group-name
   #:tlayer-group-offset-x
   #:tlayer-group-offset-y
   #:tlayer-group-x
   #:tlayer-group-y
   #:tlayer-group-opacity
   #:tlayer-group-visible
   #:tlayer-group-properties
   #:tlayer-group-layers
   #:make-tlayer-group

   #:orientation
   #:render-order
   #:stagger-axis
   #:stagger-index

   #:tmap
   #:tmap-version
   #:tmap-tiled-version
   #:tmap-orientation
   #:tmap-render-order
   #:tmap-width
   #:tmap-height
   #:tmap-tile-width
   #:tmap-tile-height
   #:tmap-hex-side-length
   #:tmap-stagger-axis
   #:tmap-stagger-index
   #:tmap-background-color
   #:tmap-next-object-id
   #:tmap-properties
   #:tmap-tilesets
   #:tmap-layers
   #:make-tmap

   #:parse-color-string
   #:make-property
   #:parse-image-encoding-string
   #:parse-compression-string
   #:parse-tile-encoding-string
   #:parse-halign
   #:parse-valign
   #:parse-draw-order
   #:parse-orientation
   #:parse-render-order
   #:parse-stagger-axis
   #:parse-stagger-index))

(in-package #:cl-tiled.impl)

(defstruct tiled-color
  (r #x00 :type (unsigned-byte 8))
  (g #x00 :type (unsigned-byte 8))
  (b #x00 :type (unsigned-byte 8))
  (a #xFF :type (unsigned-byte 8)))

(deftype property-type ()
  '(member :string :int :float :bool :color :file))

(defgeneric property-name (property)
  (:documentation "The name of the given property."))

(defgeneric property-value (property)
  (:documentation "The value of the given property."))

(defgeneric property-value-string (property)
  (:documentation "The string representation of this property's value."))

(defclass property ()
  ((name
    :type string
    :documentation "The name of this property"
    :initarg :name
    :initform (required-argument)
    :reader property-name)
   (string
    :type string
    :documentation "The verbatim string representation of this property."
    :initarg :string
    :initform (required-argument)
    :reader property-value-string)))

(defclass string-property (property)
  ())

(defmethod property-value ((property string-property))
  (property-value-string property))

(defclass int-property (property)
  ((value
    :type integer
    :reader property-value
    :initarg :value)))

(defclass float-property (property)
  ((value
    :type float
    :reader property-value
    :initarg :value)))

(defclass bool-property (property)
  ((value
    :type boolean
    :reader property-value
    :initarg :value)))

(defclass color-property (property)
  ((value
    :type color
    :reader property-value
    :initarg :value)))

(defclass file-property (property)
  ((value
    :type pathname
    :reader property-value
    :initarg :value)))

(defgeneric property-type (property)
  (:documentation "The `property-type' of the given `property'.")
  (:method ((property string-property))
    :string)
  (:method ((property int-property))
    :int)
  (:method ((property float-property))
    :float)
  (:method ((property color-property))
    :color)
  (:method ((property file-property))
    :file))

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

(defstruct tpolygon
  (points () :type list))

(defstruct tpolyline
  (points () :type list))

(deftype horizontal-alignment ()
  "Horizontal alignment of text"
  '(member :left :center :right))

(deftype vertical-alignment ()
  "Vertical alignment of text"
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
  (halign nil :type (or null horizontal-alignment))
  (valign nil :type (or null vertical-alignment)))

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
  (ellipse nil :type boolean)
  (polygon nil :type (or null tpolygon))
  (polyline nil :type (or null tpolyline))
  (text nil :type (or null ttext))
  (image nil :type (or null timage)))

(deftype draw-order ()
  "Draw order for objects in an `object-group'.
  top-down - sorted by y coordinate
  index - manual stacking, meaning drawn in defined order"
  '(member :top-down :index))

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
  (draw-order nil :type (or null draw-order))
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

(deftype orientation ()
  "Orientation of the map"
  '(member :orthogonal :isometric :staggered :hexagonal))

(deftype render-order ()
  '(member :right-down :right-up :left-down :left-up))

(deftype stagger-axis ()
  "Which axis is staggered.
Only used by the staggered, and hexagonal maps"
  '(member :x :y))

(deftype stagger-index ()
  "Whether the odd or even rows/columns are shifted.
Only used by the staggered and hexagonal maps."
  '(member :odd :even))

(defstruct tmap
  (version "" :type (or null string))
  (tiled-version "" :type (or null string))
  (orientation :orthogonal :type (or null orientation))
  (render-order nil :type (or null render-order))
  (width 0 :type integer)
  (height 0 :type integer)
  (tile-width 0 :type integer)
  (tile-height 0 :type integer)
  (hex-side-length nil :type (or null integer))
  (stagger-axis nil :type (or null stagger-axis))
  (stagger-index nil :type (or null stagger-index))
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

(defun make-property (name type value)
  (eswitch (type :test 'string-equal)
    (""
     (make-instance 'string-property :name name :string value))
    ("string"
     (make-instance 'string-property :name name :string value))
    ("int"
     (make-instance 'int-property :name name :string value :value (parse-integer value)))
    ("float"
     (make-instance 'float-property :name name :string value :value (parse-float:parse-float value)))
    ("bool"
     (make-instance 'bool-property :name name :string value :value (string-equal value "true")))
    ("color"
     (make-instance 'color-property :name name :string value :value (parse-color-string value)))
    ("file"
     ;; Note, we assume `*default-pathname-defaults*' is set to a reasonable value
     ;; This is because reconstructing a file path from the string later on would be
     ;; quite difficult.
     ;; This is generally The Right Thing
     (make-instance 'file-property :name name :string value
                                   :value (uiop:merge-pathnames* value *default-pathname-defaults*)))))

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
