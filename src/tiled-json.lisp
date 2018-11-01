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

(defpackage #:cl-tiled.impl.json
  (:use #:alexandria #:cl #:cl-tiled.impl)
  (:export
   #:parse-json-map-file
   #:parse-json-tileset-file))

(cl:in-package #:cl-tiled.impl.json)

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
  (if (and properties property-types)
      (loop
         :for (prop-name . prop-value) :in properties
         :for prop-type := (cdr (assoc prop-name property-types))
         :collect
         (make-tproperty
          :name (or (and prop-name (symbol-name prop-name)) "")
          :type (or prop-type "")
          :value (or prop-value "")))
      (list)))

(defun %parse-json-image (image)
  (if image
      (make-timage
       :format nil
       :source image
       :transparent-color nil
       :width nil
       :height nil
       :image-data nil)
      nil))

(defun %parse-json-terrain (terrain)
  (make-tterrain
   :name (json-attr terrain :name "")
   :tile (json-attr-int terrain :tile)
   :properties (%parse-json-properties (json-child terrain :properties) (json-child terrain :propertytypes))))

(defun %parse-json-ellipse (ellipse)
  (if ellipse
      (make-tellipse)
      nil))

(defun %parse-json-point (point)
  (make-tpoint
   :x (json-attr-int point :x 0)
   :y (json-attr-int point :y 0)))

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
   :properties (%parse-json-properties (json-child object :properties) (json-child object :propertytypes))
   :ellipse (%parse-json-ellipse (json-attr-bool object :ellipse))
   :polygon (%parse-json-polygon (json-children object :polygon))
   :polyline (%parse-json-polyline (json-children object :polyline))
   :text (%parse-json-text (json-child object :text))
   :image (%parse-json-image (json-attr object :image))))

(defun %parse-json-object-group (object-group)
  (if object-group
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
       :draw-order (parse-draw-order (json-attr object-group :draworder))
       :properties (%parse-json-properties (json-child object-group :properties) (json-child object-group :propertytypes))
       :objects (mapcar #'%parse-json-object (json-children object-group :objects)))
      nil))

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
   :offset-x (json-attr-int image-layer :offsetx 0)
   :offset-y (json-attr-int image-layer :offsety 0)
   :x (json-attr-int image-layer :x)
   :y (json-attr-int image-layer :y)
   :opacity (json-attr-float image-layer :opacity 1.0)
   :visible (json-attr-bool image-layer :visible t)
   :properties (%parse-json-properties (json-child image-layer :properties) (json-child image-layer :propertytypes))
   :image (%parse-json-image (json-attr image-layer :image))))

(defun %parse-json-layers (layers)
  (loop
     :for layer :in layers
     :for layer-type := (json-attr layer :type)
     :if (string= layer-type "tilelayer")
     :collect (%parse-json-tile-layer layer)
     :else :if (string= layer-type "objectgroup")
     :collect (%parse-json-object-group layer)
     :else :if (string= layer-type "imagelayer")
     :collect (%parse-json-image-layer layer)
     :else :if (string= layer-type "group")
     :collect (%parse-json-layer-group layer)))

(defun %parse-json-layer-group (layer-group)
  (make-tlayer-group
   :name (json-attr layer-group :name "")
   :offset-x (json-attr-int layer-group :offsetx 0)
   :offset-y (json-attr-int layer-group :offsety 0)
   :x (json-attr-int layer-group :x)
   :y (json-attr-int layer-group :y)
   :opacity (json-attr-float layer-group :opacity 1.0)
   :visible (json-attr-bool layer-group :visible t)
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

(defun bool-handler (str)
  (eswitch (str :test 'string=)
    ("false" :false)
    ("true" :true)))

(defun ident-handler (str)
  str)

(defun parse-json-map-file (path)
  (%parse-json-map
   (let ((cl-json:*json-identifier-name-to-lisp* #'ident-handler)
         (cl-json:*boolean-handler* #'bool-handler)
         (cl-json:*json-array-type* 'cl:vector))
     (with-open-file (stream path)
       (cl-json:decode-json stream)))))

(defun parse-json-tileset-file (path)
  (%parse-json-tileset
   (let ((cl-json:*json-identifier-name-to-lisp* #'ident-handler)
         (cl-json:*boolean-handler* #'bool-handler)
         (cl-json:*json-array-type* 'cl:vector))
     (with-open-file (stream path)
       (cl-json:decode-json stream)))))