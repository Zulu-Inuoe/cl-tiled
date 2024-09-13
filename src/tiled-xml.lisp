(defpackage #:cl-tiled.impl.xml
  (:use
   #:cl
   #:cl-tiled.data-types
   #:cl-tiled.impl)
  (:import-from
   #:alexandria
   #:if-let
   #:switch)
  (:import-from
   #:split-sequence
   #:split-sequence)
  (:import-from
   #:xmls)
  (:export
   #:parse-xml-map-stream
   #:parse-xml-tileset-stream
   #:parse-xml-template-stream))

(in-package #:cl-tiled.impl.xml)

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
  (make-property
   (xml-attr property "name" "")
   (xml-attr property "type" "")
   ;;Value might be stored in the 'value' property or in the string content
   ;;or in the <properties> sub-tag for class properties
   (or (xmls:xmlrep-attrib-value "value" property nil)
       (xmls:xmlrep-string-child property nil)
       (xml-child property "properties"))
   #'%parse-xml-properties))

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
        (%parse-image-data encoding compression (xml-text image-data "")))
      (make-array 0 :element-type '(unsigned-byte 8))))

(defun %parse-xml-image (image)
  (when image
    (let ((transparent-color (or (xml-attr-color image "trans") +transparent+))
          (width (or (xml-attr-int image "width") 0))
          (height (or (xml-attr-int image "height") 0))
          (source (xml-attr image "source")))
      (cond
        (source
         (make-instance 'external-tiled-image
                        :source (uiop:ensure-absolute-pathname source *default-pathname-defaults*)
                        :transparent-color transparent-color
                        :width width :height height))
        (t
         (make-instance 'embedded-tiled-image
                        :format (xml-attr image "format")
                        :data (%parse-xml-image-data (xml-child image "data"))
                        :transparent-color transparent-color
                        :width width :height height))))))

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

(defun %parse-xml-point (point)
  (cons
   (xml-attr-int point "x" 0)
   (xml-attr-int point "y" 0)))

(defun %parse-xml-polygon (polygon)
  (if polygon
      (make-tpolygon
       :points (mapcar
                (lambda (pair)
                  (destructuring-bind (x y)
                      (split-sequence #\, pair)
                    (cons
                     (parse-integer x :junk-allowed t)
                     (parse-integer y :junk-allowed t))))
                (split-sequence #\SPACE (xml-attr polygon "points"))))
      nil))

(defun %parse-xml-polyline (polyline)
  (if polyline
      (make-tpolyline
       :points (mapcar
                (lambda (pair)
                  (destructuring-bind (x y)
                      (split-sequence #\, pair)
                    (cons
                     (parse-integer x :junk-allowed t)
                     (parse-integer y :junk-allowed t))))
                (split-sequence #\SPACE (xml-attr polyline "points"))))
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
   :template (xml-attr object "template")
   :properties (%parse-xml-properties (xml-child object "properties"))
   :ellipse (and (xml-child object "ellipse") t)
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
                        (split-sequence #\, (xml-attr tile "terrain")))
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
      (mapcar #'parse-integer (split-sequence #\, data)))
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
   :x (xml-attr-int image-layer "x")
   :y (xml-attr-int image-layer "y")
   :width (xml-attr-int image-layer "width")
   :height (xml-attr-int image-layer "height")
   :opacity (xml-attr-float image-layer "opacity" 1.0)
   :visible (xml-attr-bool image-layer "visible" t)
   :offset-x (xml-attr-int image-layer "offsetx" 0)
   :offset-y (xml-attr-int image-layer "offsety" 0)
   :repeat-x (xml-attr-bool image-layer "repeatx" nil)
   :repeat-y (xml-attr-bool image-layer "repeaty" nil)
   :properties (%parse-xml-properties (xml-child image-layer "properties"))
   :image (%parse-xml-image (xml-child image-layer "image"))))

(defun %parse-xml-layers (layers)
  (loop
    :for layer :in layers
    :for layer-type := (xmls:node-name layer)
    :for parsed := (switch (layer-type :test #'string=)
                     ("layer" (%parse-xml-tile-layer layer))
                     ("objectgroup" (%parse-xml-object-group layer))
                     ("imagelayer" (%parse-xml-image-layer layer))
                     ("group" (%parse-xml-layer-group layer)))
    :if parsed :collect parsed))

(defun %parse-xml-layer-group (layer-group)
  (make-tlayer-group
   :name (xml-attr layer-group "name" "")
   :x (xml-attr-int layer-group "x")
   :y (xml-attr-int layer-group "y")
   :width (xml-attr-int layer-group "width")
   :height (xml-attr-int layer-group "height")
   :opacity (xml-attr-float layer-group "opacity" 1.0)
   :visible (xml-attr-bool layer-group "visible" t)
   :properties (%parse-xml-properties (xml-child layer-group "properties"))
   :offset-x (xml-attr-int layer-group "offsetx" 0)
   :offset-y (xml-attr-int layer-group "offsety" 0)
   :layers (%parse-xml-layers (xmls:node-children layer-group))))

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
   :layers (%parse-xml-layers (xmls:node-children map))))

(defun %parse-xml-template (template)
  (make-ttemplate
   :tileset (xml-attr (xml-child template "tileset") "source")
   :object (%parse-xml-object (xml-child template "object"))))

(defun %slurp-stream (stream)
  (with-output-to-string (str)
    (loop
      :for line := (read-line stream nil nil)
      :while line
      :doing
         (write-line (string-right-trim '(#\return) line) str))))

(defun %slurp-file (path)
  (with-open-file (stream path)
    (%slurp-stream stream)))

(defun parse-xml-map-stream (stream current-directory)
  (let ((tree (xmls:parse (%slurp-stream stream))))
    (uiop:with-current-directory ((uiop:pathname-directory-pathname current-directory))
      (%parse-xml-map tree))))

(defun parse-xml-tileset-stream (stream current-directory)
  (let ((tree (xmls:parse (%slurp-stream stream))))
    (uiop:with-current-directory ((uiop:pathname-directory-pathname current-directory))
      (%parse-xml-tileset tree))))

(defun parse-xml-template-stream (stream current-directory)
  (let ((tree (xmls:parse (%slurp-stream stream))))
    (uiop:with-current-directory ((uiop:pathname-directory-pathname current-directory))
      (%parse-xml-template tree))))
