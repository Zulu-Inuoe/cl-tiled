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

(defpackage #:cl-tiled.impl.xml
  (:use #:alexandria #:cl #:cl-tiled.impl)
  (:export
   #:parse-xml-map-file
   #:parse-xml-tileset-file))

(cl:in-package #:cl-tiled.impl.xml)

(defun xml-attr (node attr &optional (default nil))
  (or (xmls:xmlrep-attrib-value attr node nil)
      default))

(defun xml-attr-int (node attr &optional (default nil))
  (if-let ((val (xml-attr node attr)))
    (or (parse-integer val :junk-allowed t) default)
    default))

(defun xml-attr-bool (node attr &optional (default nil))
  (if-let ((int-val (xml-attr-int node attr)))
    (/= int-val 0)
    default))

(defun xml-attr-color (node attr &optional (default nil))
  (if-let ((str-val (xml-attr node attr)))
    (parse-color-string str-val default)
    default))

(defun xml-attr-float (node attr &optional (default nil))
  (if-let ((val (xml-attr node attr)))
    (or (parse-float:parse-float val :junk-allowed t) default)
    default))

(defun xml-child (node tag)
  (xmls:xmlrep-find-child-tag tag node nil))

(defun xml-children (node tag)
  (xmls:xmlrep-find-child-tags tag node))

(defun xml-text (node &optional (default nil))
  (or (xmls:xmlrep-string-child node)
      default))

(defun %parse-xml-property (property)
  (make-tproperty
   :name (xml-attr property "name" "")
   :type (xml-attr property "type" "")
   :value (xml-attr property "value" "")))

(defun %parse-xml-properties (properties)
  (if properties
      (mapcar #'%parse-xml-property
              (xml-children properties "property"))
      (list)))

(defun %parse-image-data (encoding compression data)
  (switch ((cons encoding compression) :test 'equalp)
    ('(:base64 . nil)
      (cl-base64:base64-string-to-usb8-array data))
    ('(:base64 . :zlib)
      (chipz:decompress nil 'chipz:zlib (cl-base64:base64-string-to-usb8-array data)))
    ('(:base64 . :gzip)
      (chipz:decompress nil 'chipz:gzip (cl-base64:base64-string-to-usb8-array data)))
    (t
     (error "invalid encoding/compression: '~A'/'~A'" encoding compression))))


(defun %parse-xml-image-data (image-data)
  (if image-data
      (let ((encoding (parse-image-encoding-string (xml-attr image-data "encoding")))
            (compression (parse-compression-string (xml-attr image-data "compression"))))
        (make-timage-data
         :encoding encoding
         :compression compression
         :data (%parse-image-data encoding compression (xml-text image-data ""))))
      nil))

(defun %parse-xml-image (image)
  (if image
      (make-timage
       :format (xml-attr image "format")
       :source (xml-attr image "source")
       :transparent-color (xml-attr-color image "trans")
       :width (xml-attr-int image "width")
       :height (xml-attr-int image "height")
       :image-data (%parse-xml-image-data (xml-child image "data")))
      nil))

(defun %parse-xml-terrain (terrain)
  (make-tterrain
   :name (xml-attr terrain "name")
   :tile (xml-attr-int terrain "tile")
   :properties (%parse-xml-properties (xml-child terrain "properties"))))

(defun %parse-xml-terrain-types (terrain-types)
  (if terrain-types
      (mapcar #'%parse-xml-terrain
              (xml-children terrain-types "terrain"))
      (list)))

(defun %parse-xml-ellipse (ellipse)
  (if ellipse
      (make-tellipse)
      nil))

(defun %parse-xml-point (point)
  (make-tpoint
   :x (xml-attr-int point "x" 0)
   :y (xml-attr-int point "y" 0)))

(defun %parse-xml-polygon (polygon)
  (if polygon
      (make-tpolygon
       :points (mapcar
                (lambda (pair)
                  (destructuring-bind (x y)
                      (split-sequence:split-sequence #\, pair)
                    (make-tpoint
                     :x (parse-float:parse-float x :junk-allowed t)
                     :y (parse-float:parse-float y :junk-allowed t))))
                (split-sequence:split-sequence #\SPACE (xml-attr polygon "points"))))
      nil))

(defun %parse-xml-polyline (polyline)
  (if polyline
      (make-tpolyline
       :points (mapcar
                (lambda (pair)
                  (destructuring-bind (x y)
                      (split-sequence:split-sequence #\, pair)
                    (make-tpoint
                     :x (parse-float:parse-float x :junk-allowed t)
                     :y (parse-float:parse-float y :junk-allowed t))))
                (split-sequence:split-sequence #\SPACE (xml-attr polyline "points"))))
      nil))

(defun %parse-xml-text (text)
  (if text
      (make-ttext
       :text (xml-text text "")
       :font-family (xml-attr text "fontfamily")
       :pixel-size (xml-attr-int text "pixelsize")
       :wrap (xml-attr-bool text "wrap")
       :color (xml-attr-color text "color")
       :bold (xml-attr-bool text "bold")
       :italic (xml-attr-bool text "italic")
       :underline (xml-attr-bool text "underline")
       :strikeout (xml-attr-bool text "strikeout")
       :kerning (xml-attr-bool text "kerning" t)
       :halign (parse-halign (xml-attr text "halign"))
       :valign (parse-valign (xml-attr text "valign")))
      nil))

(defun %parse-xml-object (object)
  (make-tobject
   :id (xml-attr-int object "id" 0)
   :name (xml-attr object "name" "")
   :type (xml-attr object "type" "")
   :x (xml-attr-int object "x" 0)
   :y (xml-attr-int object "y" 0)
   :width (xml-attr-int object "width")
   :height (xml-attr-int object "height")
   :rotation (xml-attr-float object "rotation")
   :gid (xml-attr-int object "gid")
   :visible (xml-attr-bool object "visible" t)
   :properties (%parse-xml-properties (xml-child object "properties"))
   :ellipse (%parse-xml-ellipse (xml-child object "ellipse"))
   :polygon (%parse-xml-polygon (xml-child object "polygon"))
   :polyline (%parse-xml-polyline (xml-child object "polyline"))
   :text (%parse-xml-text (xml-child object "text"))
   :image (%parse-xml-image (xml-child object "image"))))

(defun %parse-xml-object-group (object-group)
  (if object-group
      (make-tobject-group
       :name (xml-attr object-group "name" "")
       :color (xml-attr-color object-group "color")
       :x (xml-attr-int object-group "x")
       :y (xml-attr-int object-group "y")
       :width (xml-attr-int object-group "width")
       :height (xml-attr-int object-group "height")
       :opacity (xml-attr-float object-group "opacity" 1.0)
       :visible (xml-attr-bool object-group "visible" t)
       :offset-x (xml-attr-int object-group "offsetx" 0)
       :offset-y (xml-attr-int object-group "offsety" 0)
       :draw-order (parse-draw-order (xml-attr object-group "draworder"))
       :properties (%parse-xml-properties (xml-child object-group "properties"))
       :objects (mapcar #'%parse-xml-object (xml-children object-group "object")))
      nil))

(defun %parse-xml-frame (frame)
  (make-tframe
   :tile-id (xml-attr-int frame "tileid")
   :duration (xml-attr-int frame "duration")))

(defun %parse-xml-tileset-tile (tile)
  (make-ttileset-tile
   :id (xml-attr-int tile "id" 0)
   :type (xml-attr tile "type" "")
   :terrain (destructuring-bind (&optional t1 t2 t3 t4)
                (mapcar (lambda (v) (and v (parse-integer v :junk-allowed t)))
                        (split-sequence:split-sequence #\, (xml-attr tile "terrain")))
              (vector t1 t2 t3 t4))
   :probability (xml-attr-float tile "probability")
   :properties (%parse-xml-properties (xml-child tile "properties"))
   :image (%parse-xml-image (xml-child tile "image"))
   :object-group (%parse-xml-object-group (xml-child tile "objectgroup"))
   :frames (if-let ((animation (xml-child tile "animation")))
             (mapcar #'%parse-xml-frame (xml-children animation "frame")))))

(defun %parse-xml-tileset (tileset)
  (make-ttileset
   :first-gid (xml-attr-int tileset "firstgid")
   :source (xml-attr tileset "source")
   :name (xml-attr tileset "name")
   :tile-width (xml-attr-int tileset "tilewidth")
   :tile-height (xml-attr-int tileset "tileheight")
   :spacing (xml-attr-int tileset "spacing" 0)
   :margin (xml-attr-int tileset "margin" 0)
   :tile-count (xml-attr-int tileset "tilecount")
   :columns (xml-attr-int tileset "columns")
   :tile-offset-x (if-let ((offset (xml-child tileset "tileoffset")))
                    (xml-attr-int offset "x" 0)
                    0)
   :tile-offset-y (if-let ((offset (xml-child tileset "tileoffset")))
                    (xml-attr-int offset "y" 0)
                    0)
   :properties (%parse-xml-properties (xml-child tileset "properties"))
   :image (%parse-xml-image (xml-child tileset "image"))
   :terrains (%parse-xml-terrain-types (xml-child tileset "terraintypes"))
   :tiles (mapcar #'%parse-xml-tileset-tile (xml-children tileset "tile"))))

(defun %collect-tiles-from-data (ub-data-seq)
  (assert (zerop (mod (length ub-data-seq) 4))
          (ub-data-seq) "data sequence not divisible by 4")
  (loop
     :for i := 0 :then (+ i 4)
     :while (< i (length ub-data-seq))
     :collect (nibbles:ub32ref/le ub-data-seq i)))

(defun %parse-tiles-data (encoding compression data)
  (switch ((cons encoding compression) :test 'equalp)
    ('(nil . nil)
      (error "not implemented"))
    ('(:csv . nil)
      (mapcar #'parse-integer (split-sequence:split-sequence #\, data)))
    ('(:base64 . nil)
      (%collect-tiles-from-data (cl-base64:base64-string-to-usb8-array data)))
    ('(:base64 . :zlib)
      (%collect-tiles-from-data  (chipz:decompress nil 'chipz:zlib (cl-base64:base64-string-to-usb8-array data))))
    ('(:base64 . :gzip)
      (%collect-tiles-from-data (chipz:decompress nil 'chipz:gzip (cl-base64:base64-string-to-usb8-array data))))
    (t
     (error "invalid encoding/compression: '~a'/'~a'" encoding compression))))

(defun %parse-xml-tile-layer-data (tile-layer-data)
  (if tile-layer-data
      (let ((encoding (parse-tile-encoding-string (xml-attr tile-layer-data "encoding")))
            (compression (parse-compression-string (xml-attr tile-layer-data "compression"))))
        (make-ttile-data
         :encoding encoding
         :compression compression
         :tiles (%parse-tiles-data encoding compression (xml-text tile-layer-data ""))))
      nil))

(defun %parse-xml-tile-layer (tile-layer)
  (make-ttile-layer
   :name (xml-attr tile-layer "name" "")
   :x (xml-attr-int tile-layer "x")
   :y (xml-attr-int tile-layer "y")
   :width (xml-attr-int tile-layer "width")
   :height (xml-attr-int tile-layer "height")
   :opacity (xml-attr-float tile-layer "opacity" 1.0)
   :visible (xml-attr-bool tile-layer "visible" t)
   :offset-x (xml-attr-int tile-layer "offsetx" 0)
   :offset-y (xml-attr-int tile-layer "offsety" 0)
   :properties (%parse-xml-properties (xml-child tile-layer "properties"))
   :tile-data (%parse-xml-tile-layer-data (xml-child tile-layer "data"))))

(defun %parse-xml-image-layer (image-layer)
  (make-timage-layer
   :name (xml-attr image-layer "name" "")
   :offset-x (xml-attr-int image-layer "offsetx" 0)
   :offset-y (xml-attr-int image-layer "offsety" 0)
   :x (xml-attr-int image-layer "x")
   :y (xml-attr-int image-layer "y")
   :opacity (xml-attr-float image-layer "opacity" 1.0)
   :visible (xml-attr-bool image-layer "visible" t)
   :properties (%parse-xml-properties (xml-child image-layer "properties"))
   :image (%parse-xml-image (xml-child image-layer "image"))))

(defun %parse-xml-layer-group (layer-group)
  (make-tlayer-group
   :name (xml-attr layer-group "name" "")
   :offset-x (xml-attr-int layer-group "offsetx" 0)
   :offset-y (xml-attr-int layer-group "offsety" 0)
   :x (xml-attr-int layer-group "x")
   :y (xml-attr-int layer-group "y")
   :opacity (xml-attr-float layer-group "opacity" 1.0)
   :visible (xml-attr-bool layer-group "visible" t)
   :properties (%parse-xml-properties (xml-child layer-group "properties"))
   :tile-layers (mapcar #'%parse-xml-tile-layer (xml-children layer-group "layer"))
   :object-groups (mapcar #'%parse-xml-object-group (xml-children layer-group "objectgroup"))
   :image-layers (mapcar #'%parse-xml-image-layer (xml-children layer-group "imagelayer"))
   :layer-groups (mapcar #'%parse-xml-layer-group (xml-children layer-group "group"))))

(defun %parse-xml-map (map)
  (make-tmap
   :version (xml-attr map "version")
   :tiled-version (xml-attr map "tiledversion")
   :orientation (parse-orientation (xml-attr map "orientation"))
   :render-order (parse-render-order (xml-attr map "renderorder"))
   :width (xml-attr-int map "width")
   :height (xml-attr-int map "height")
   :tile-width (xml-attr-int map "tilewidth")
   :tile-height (xml-attr-int map "tileheight")
   :hex-side-length (xml-attr-int map "hexsidelength")
   :stagger-axis (parse-stagger-axis (xml-attr map "staggeraxis"))
   :stagger-index (parse-stagger-index (xml-attr map "staggerindex"))
   :background-color (xml-attr-color map "backgroundcolor")
   :next-object-id (xml-attr-int map "nextobjectid")
   :properties (%parse-xml-properties (xml-child map "properties"))
   :tilesets (mapcar #'%parse-xml-tileset (xml-children map "tileset"))
   :tile-layers (mapcar #'%parse-xml-tile-layer (xml-children map "layer"))
   :object-groups (mapcar #'%parse-xml-object-group (xml-children map "objectgroup"))
   :image-layers (mapcar #'%parse-xml-image-layer (xml-children map "imagelayer"))
   :layer-groups (mapcar #'%parse-xml-layer-group (xml-children map "group"))))

(defun %slurp-file (path)
  (with-output-to-string (str)
    (with-open-file (stream path)
      (loop
         :for line := (read-line stream nil nil)
         :while line
         :doing
         (write-line (string-right-trim '(#\return) line) str)))))

(defun parse-xml-map-file (path)
  (%parse-xml-map
   (xmls:parse (%slurp-file path))))

(defun parse-xml-tileset-file (path)
  (%parse-xml-tileset
   (xmls:parse (%slurp-file path))))