(defpackage #:cl-tiled.impl.json
  (:use
   #:cl
   #:cl-tiled.data-types
   #:cl-tiled.impl)
  (:import-from
   #:alexandria
   #:if-let
   #:switch
   #:eswitch)
  (:export
   #:parse-json-map-stream
   #:parse-json-tileset-stream
   #:parse-json-template-stream))

(in-package #:cl-tiled.impl.json)

(defun json-attr (node attr &optional (default nil)
                  &aux (val (cdr (assoc attr node :test (lambda (a b) (string-equal (symbol-name a) (symbol-name b)))))))
  (etypecase val
    (null default)
    (keyword (ecase val (:true "true") (:false "false")))
    (integer (format nil "~D" val))
    (float (format nil "~F" val))
    (string val)
    (list default)
    (vector default)))

(defun json-attr-color (node attr &optional (default nil)
                        &aux (val (cdr (assoc attr node :test (lambda (a b) (string-equal (symbol-name a) (symbol-name b)))))))
  (or (parse-color-string val) default))

(defun json-attr-bool (node attr &optional (default nil)
                       &aux (val (cdr (assoc attr node :test (lambda (a b) (string-equal (symbol-name a) (symbol-name b)))))))
  (typecase val
    (keyword (eq val :true))
    (t default)))

(defun json-attr-int (node attr &optional (default nil))
  (if-let ((val (json-attr node attr)))
    (or (parse-integer val :junk-allowed t) default)
    default))

(defun json-attr-float (node attr &optional (default nil))
  (if-let ((val (json-attr node attr)))
    (or (parse-float:parse-float val :junk-allowed t) default)
    default))

(defun json-child (node tag
                   &aux (val (cdr (assoc tag node :test (lambda (a b) (string-equal (symbol-name a) (symbol-name b)))))))
  (typecase val
    (list val)
    (t (list))))

(defun json-children (node tag
                      &aux (val (cdr (assoc tag node :test (lambda (a b) (string-equal (symbol-name a) (symbol-name b)))))))
  (typecase val
    (null (list))
    (vector (map 'list 'identity val))
    (t (list))))

(defun %parse-json-properties (properties property-types)
  ;; TODO : class properties wouldn't be loaded since there is no
  ;; propertytypes in Tiled >= 1.2
  (if (and properties property-types)
      (loop
        :for (prop-name . prop-value) :in properties
        :for prop-type := (cdr (assoc prop-name property-types))
        :collect
        (make-property
         (or (and prop-name (symbol-name prop-name)) "")
         (or prop-type "")
         (or prop-value "")
         #'%parse-json-properties))
      (list)))

(defun %parse-json-image (image)
  (when image
    (make-instance 'external-tiled-image
                   :source (uiop:merge-pathnames* image)
                   :transparent-color +transparent+
                   :width 0 :height 0)))

(defun %parse-json-terrain (terrain)
  (make-tterrain
   :name (json-attr terrain :name "")
   :tile (json-attr-int terrain :tile)
   :properties (%parse-json-properties (json-child terrain :properties) (json-child terrain :propertytypes))))

(defun %parse-json-point (point)
  (cons
   (json-attr-int point :x 0)
   (json-attr-int point :y 0)))

(defun %parse-json-polygon (polygon-points)
  (if polygon-points
      (make-tpolygon
       :points (mapcar #'%parse-json-point
                       polygon-points))
      nil))

(defun %parse-json-polyline (polyline-points)
  (if polyline-points
      (make-tpolyline
       :points (mapcar #'%parse-json-point
                       polyline-points))
      nil))

(defun %parse-json-text (text)
  (if text
      (make-ttext
       :text (json-attr text :text "")
       :font-family (json-attr text :fontfamily)
       :pixel-size (json-attr-int text :pixelsize)
       :wrap (json-attr-bool text :wrap)
       :color (json-attr-color text :color)
       :bold (json-attr-bool text :bold)
       :italic (json-attr-bool text :italic)
       :underline (json-attr-bool text :underline)
       :strikeout (json-attr-bool text :strikeout)
       :kerning (json-attr-bool text :kerning t)
       :halign (parse-halign (json-attr text :halign))
       :valign (parse-valign (json-attr text :valign)))
      nil))

(defun %parse-json-object (object)
  (make-tobject
   :id (json-attr-int object :id 0)
   :name (json-attr object :name "")
   :type (json-attr object :type "")
   :x (json-attr-int object :x 0)
   :y (json-attr-int object :y 0)
   :width (json-attr-int object :width)
   :height (json-attr-int object :height)
   :rotation (json-attr-float object :rotation)
   :gid (json-attr-int object :gid)
   :visible (json-attr-bool object :visible t)
   :template (json-attr object :template)
   :properties (%parse-json-properties (json-child object :properties) (json-child object :propertytypes))
   :ellipse (json-attr-bool object :ellipse)
   :polygon (%parse-json-polygon (json-children object :polygon))
   :polyline (%parse-json-polyline (json-children object :polyline))
   :text (%parse-json-text (json-child object :text))
   :image (%parse-json-image (json-attr object :image))))

(defun %parse-json-object-group (object-group)
  (when object-group
    (make-tobject-group
     :name (json-attr object-group :name "")
     :color (json-attr-color object-group :color)
     :x (json-attr-int object-group :x)
     :y (json-attr-int object-group :y)
     :width (json-attr-int object-group :width)
     :height (json-attr-int object-group :height)
     :opacity (json-attr-float object-group :opacity 1.0)
     :visible (json-attr-bool object-group :visible t)
     :offset-x (json-attr-int object-group :offsetx 0)
     :offset-y (json-attr-int object-group :offsety 0)
     :properties (%parse-json-properties (json-child object-group :properties) (json-child object-group :propertytypes))
     :draw-order (parse-draw-order (json-attr object-group :draworder))
     :objects (mapcar #'%parse-json-object (json-children object-group :objects)))))

(defun %parse-json-frame (frame)
  (make-tframe
   :tile-id (json-attr-int frame :tileid)
   :duration (json-attr-int frame :duration)))

(defun %parse-json-tileset-tile (id tile)
  (make-ttileset-tile
   :id (or id 0)
   :type (json-attr tile :type "")
   :terrain (destructuring-bind (&optional t1 t2 t3 t4)
                (json-children tile :terrain)
              (vector (and t1 (/= t1 -1) t1)
                      (and t2 (/= t2 -1) t2)
                      (and t3 (/= t3 -1) t3)
                      (and t4 (/= t4 -1) t4)))
   :probability (json-attr-float tile :probability)
   :properties (%parse-json-properties (json-children tile :properties) (json-children tile :propertytypes))
   :image (%parse-json-image (json-attr tile :image))
   :object-group (%parse-json-object-group (json-child tile :objectgroup))
   :frames (mapcar #'%parse-json-frame (json-children tile :animation))))

(defun %parse-json-tileset (tileset)
  (make-ttileset
   :first-gid (json-attr-int tileset :firstgid)
   :source (json-attr tileset :source)
   :name (json-attr tileset :name)
   :tile-width (json-attr-int tileset :tilewidth)
   :tile-height (json-attr-int tileset :tileheight)
   :spacing (json-attr-int tileset :spacing 0)
   :margin (json-attr-int tileset :margin 0)
   :tile-count (json-attr-int tileset :tilecount)
   :columns (json-attr-int tileset :columns)
   :tile-offset-x (if-let ((offset (json-child tileset :tileoffset)))
                    (json-attr-int offset :x 0)
                    0)
   :tile-offset-y (if-let ((offset (json-child tileset :tileoffset)))
                    (json-attr-int offset :y 0)
                    0)
   :properties (%parse-json-properties (json-child tileset :properties) (json-child tileset :propertytypes))
   :image (%parse-json-image (json-attr tileset :image))
   :terrains (map 'list #'%parse-json-terrain (json-children tileset :terrains))
   :tiles (mapcar (lambda (tile) (%parse-json-tileset-tile (parse-integer (symbol-name (car tile)) :junk-allowed t) (cdr tile)))
                  (json-child tileset :tiles))))

(defun %parse-json-tile-layer-data (tile-layer-data)
  (if tile-layer-data
      (make-ttile-data
       :encoding nil
       :compression nil
       :tiles tile-layer-data)
      nil))

(defun %parse-json-tile-layer (tile-layer)
  (make-ttile-layer
   :name (json-attr tile-layer :name "")
   :x (json-attr-int tile-layer :x)
   :y (json-attr-int tile-layer :y)
   :width (json-attr-int tile-layer :width)
   :height (json-attr-int tile-layer :height)
   :opacity (json-attr-float tile-layer :opacity 1.0)
   :visible (json-attr-bool tile-layer :visible t)
   :offset-x (json-attr-int tile-layer :offsetx 0)
   :offset-y (json-attr-int tile-layer :offsety 0)
   :properties (%parse-json-properties (json-child tile-layer :properties) (json-child tile-layer :propertytypes))
   :tile-data (%parse-json-tile-layer-data (json-children tile-layer :data))))

(defun %parse-json-image-layer (image-layer)
  (make-timage-layer
   :name (json-attr image-layer :name "")
   :x (json-attr-int image-layer :x)
   :y (json-attr-int image-layer :y)
   :width (json-attr-int image-layer :width)
   :height (json-attr-int image-layer :height)
   :opacity (json-attr-float image-layer :opacity 1.0)
   :visible (json-attr-bool image-layer :visible t)
   :offset-x (json-attr-int image-layer :offsetx 0)
   :offset-y (json-attr-int image-layer :offsety 0)
   :repeat-x (json-attr-bool image-layer :repeatx nil)
   :repeat-y (json-attr-bool image-layer :repeaty nil)
   :properties (%parse-json-properties (json-child image-layer :properties) (json-child image-layer :propertytypes))
   :image (%parse-json-image (json-attr image-layer :image))))

(defun %parse-json-layers (layers)
  (loop
    :for layer :in layers
    :for layer-type := (json-attr layer :type)
    :for parsed := (switch (layer-type :test #'string=)
                     ("tilelayer" (%parse-json-tile-layer layer))
                     ("objectgroup" (%parse-json-object-group layer))
                     ("imagelayer" (%parse-json-image-layer layer))
                     ("group" (%parse-json-layer-group layer)))
    :if parsed :collect parsed))

(defun %parse-json-layer-group (layer-group)
  (make-tlayer-group
   :name (json-attr layer-group :name "")
   :x (json-attr-int layer-group :x)
   :y (json-attr-int layer-group :y)
   :width (json-attr-int layer-group :width)
   :height (json-attr-int layer-group :height)
   :opacity (json-attr-float layer-group :opacity 1.0)
   :visible (json-attr-bool layer-group :visible t)
   :offset-x (json-attr-int layer-group :offsetx 0)
   :offset-y (json-attr-int layer-group :offsety 0)
   :properties (%parse-json-properties (json-child layer-group :properties) (json-child layer-group :propertytypes))
   :layers (%parse-json-layers (json-children layer-group :layers))))

(defun %parse-json-map (map)
  (make-tmap
   :version (json-attr map :version)
   :tiled-version (json-attr map :tiledversion)
   :orientation (parse-orientation (json-attr map :orientation))
   :render-order (parse-render-order (json-attr map :renderorder))
   :width (json-attr-int map :width)
   :height (json-attr-int map :height)
   :tile-width (json-attr-int map :tilewidth)
   :tile-height (json-attr-int map :tileheight)
   :hex-side-length (json-attr-int map :hexsidelength)
   :stagger-axis (parse-stagger-axis (json-attr map :staggetaxis))
   :stagger-index (parse-stagger-index (json-attr map :staggerindex))
   :background-color (json-attr-color map :backgroundcolor)
   :next-object-id (json-attr-int map :nextobjectid)
   :properties (%parse-json-properties (json-child map :properties) (json-child map :propertytypes))
   :tilesets (mapcar #'%parse-json-tileset (json-children map :tilesets))
   :layers (%parse-json-layers (json-children map :layers))))

(defun %parse-json-template (template)
  (make-ttemplate
   :tileset (json-attr (json-child template :tileset) :source)
   :object (%parse-json-object (json-child template :object))))

(defun bool-handler (str)
  (eswitch (str :test 'string=)
    ("false" :false)
    ("true" :true)))

(defun ident-handler (str)
  str)

(defun for-json-tree-from-stream (stream current-directory processor)
  (let* ((tree (let ((cl-json:*json-identifier-name-to-lisp* #'ident-handler)
                     (cl-json:*boolean-handler* #'bool-handler)
                     (cl-json:*json-array-type* 'cl:vector))
                 (cl-json:decode-json stream)))
         (thunk (lambda () (funcall processor tree))))
    (if current-directory
        (uiop:call-with-current-directory
         (uiop:pathname-directory-pathname current-directory)
         thunk)
        (funcall thunk))))

(defun parse-json-map-stream (stream &optional current-directory)
  (for-json-tree-from-stream stream current-directory #'%parse-json-map))

(defun parse-json-tileset-stream (stream &optional current-directory)
  (for-json-tree-from-stream stream current-directory #'%parse-json-tileset))

(defun parse-json-template-stream (stream &optional current-directory)
  (for-json-tree-from-stream stream current-directory #'%parse-json-template))
