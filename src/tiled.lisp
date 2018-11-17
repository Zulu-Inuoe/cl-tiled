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

(cl:in-package #:cl-tiled)

(defclass properties-mixin ()
  ((properties
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :reader properties)))

(defparameter +transparent+ (make-tiled-color :a #x00))
(defparameter +black+ (make-tiled-color))

(defmethod initialize-instance :after ((obj properties-mixin)
                                       &key (properties nil)
                                       &aux (hash (properties obj)))
  (dolist (tprop properties)
    (with-slots (name type value) tprop
      (setf (gethash name hash)
            (eswitch (type :test 'string-equal)
              ("" value)
              ("string" value)
              ("int" (parse-integer value))
              ("float" (parse-float:parse-float value))
              ("bool" (string-equal value "true"))
              ("color" (cl-tiled.impl:parse-color-string value))
              ("file" (uiop:ensure-absolute-pathname value *default-pathname-defaults*)))))))

(defun property-val (obj prop-name)
  (values (gethash prop-name (properties obj))))

(defclass tiled-image ()
  ((transparent-color
    :documentation "Color to use as 'transparent' (optional)"
    :type tiled-color
    :initarg :transparent-color
    :reader image-transparent-color)
   (width
    :documentation "Width of the image in pixels (optional)"
    :type integer
    :initarg :width
    :reader image-width)
   (height
    :documentation "Height of the image in pixels (optional)"
    :type integer
    :initarg :height
    :reader image-height)))

(defclass embedded-tiled-image (tiled-image)
  ((format
    :documentation "Format of the embedded image data as a string in the form \"png\", \"gif\", \"jpg\", \"bmp\", etc."
    :type string
    :initarg :format
    :reader image-format)
   (data
    :documentation "Embedded data in the given format"
    :type (simple-array (unsigned-byte 8))
    :initarg :data
    :reader image-data))
  (:documentation
   "An image that is embedded in the defining tileset/layer etc.
`data' refers to the embedded image data"))

(defclass external-tiled-image (tiled-image)
  ((source
    :documentation "Path to the image file"
    :type pathname
    :initarg :source
    :reader image-source))
  (:documentation
   "An image that is stored externally. Source is a path referring to it."))

(defclass object-group ()
  ((objects
    :documentation "The objects in this group."
    :initarg :objects
    :reader object-group-objects)
   (draw-order
    :documentation "The draw order for the objects on this group."
    :type draw-order
    :initarg :draw-order
    :reader object-group-draw-order)))

(defclass tiled-tile ()
  ((tileset
    :documentation "The tileset this tile belongs to."
    :type tileset
    :initarg :tileset
    :reader tile-tileset)
   (id
    :documentation "The local ID of this tile within its tileset"
    :type integer
    :initarg :id
    :reader tile-id)))

(defclass tiled-tileset-tile (properties-mixin tiled-tile)
  ((type
    :documentation "'The type of the tile. Refers to an object type and is used by tile objects. (optional)'"
    :type string
    :initarg :type
    :reader tile-type)
   (terrains
    :documentation "Defines the terrain types of each corner of the tile.
0 - top left
1 - top right
2 - bottom left
3 - bottom right
If nil, indicates no terrain at that corner."
    :type (simple-vector 4)
    :initarg :terrains
    :reader tile-terrains)
   (probability
    :documentation "'A percentage indicating the probability that this tile is chosen when it competes with others while editing with the terrain tool.'"
    :type (or null real)
    :initarg :probability
    :reader tile-probability)
   (object-group
    :documentation "The collision objects in this tile"
    :type (or null object-group)
    :initform nil
    :reader tile-object-group))
  (:documentation
   "A tile."))

(defclass tiled-frame ()
  ((tile
    :documentation "The tile to use for this frame"
    :type tiled-tile
    :initarg :tile
    :reader frame-tile)
   (duration
    :documentation "The length of this frame, in milliseconds"
    :type integer
    :initarg :duration
    :reader frame-duration))
  (:documentation
   "One frame of animation for an animated tile.
The `tile' here refers to the image to be displayed on this particular frame."))

(defun tile-column (tile)
  (mod (tile-id tile) (tileset-columns (tile-tileset tile))))

(defun tile-row (tile)
  (values (truncate (tile-id tile) (tileset-columns (tile-tileset tile)))))

(defun tile-pixel-x (tile
                          &aux
                            (column (tile-column tile))
                            (tileset (tile-tileset tile)))
  (+ (* column (tileset-tile-width tileset))
     (* column (tileset-spacing tileset))
     (tileset-offset-x tileset)
     (tileset-margin tileset)))

(defun tile-pixel-y (tile
                       &aux
                         (row (tile-row tile))
                         (tileset (tile-tileset tile)))
  (+ (* row (tileset-tile-height tileset))
     (* row (tileset-spacing tileset))
     (tileset-offset-y tileset)
     (tileset-margin tileset)))

(defun tile-width (tile)
  (tileset-tile-width (tile-tileset tile)))

(defun tile-height (tile)
  (tileset-tile-height (tile-tileset tile)))

(defun tile-image (tile)
  (tileset-image (tile-tileset tile)))

(defclass animated-tile (tiled-tileset-tile)
  ((frames
    :documentation "A lit of animation frames"
    :type list
    :initarg :frames
    :reader tile-frames))
  (:documentation
   "An animated tile. The frames are played in successive order, looping indefinitely"))

(defclass tiled-terrain (properties-mixin)
  ((name
    :documentation "The name of the terrain type"
    :type string
    :initarg :name
    :reader terrain-name)
   (tile
    :documentation "The tile this terrain type refers to"
    :type tiled-tile
    :initarg :tile
    :reader terrain-tile)))

(defclass tileset (properties-mixin)
  ((name
    :documentation "Name of this tileset"
    :type string
    :initarg :name
    :reader tileset-name)
   (first-gid
    :documentation "First global tile ID of this tileset"
    :type integer
    :initarg :first-gid
    :reader tileset-first-gid)
   (tile-width
    :documentation "The 'maximum' width of the tiles in this tileset in pixels"
    :type integer
    :initarg :tile-width
    :reader tileset-tile-width)
   (tile-height
    :documentation "The 'maximum' height of the tiles in this tileset in pixels"
    :type integer
    :initarg :tile-height
    :reader tileset-tile-height)
   (spacing
    :documentation "The spacing in pixels between the tiles in this tileset"
    :type integer
    :initarg :spacing
    :reader tileset-spacing)
   (margin
    :documentation "The margin around the tiles in this tileset"
    :type integer
    :initarg :margin
    :reader tileset-margin)
   (tile-count
    :documentation "The number of tiles in this tileset"
    :type integer
    :initarg :tile-count
    :reader tileset-tile-count)
   (columns
    :documentation "The number of columns of tiles in this tileset"
    :type integer
    :initarg :columns
    :reader tileset-columns)
   (offset-x
    :documentation "The offset that is applied when drawing the tiles in this tileset"
    :type integer
    :initarg :offset-x
    :reader tileset-offset-x)
   (offset-y
    :documentation "The offset that is applied when drawing the tiles in this tileset"
    :type integer
    :initarg :offset-y
    :reader tileset-offset-y)
   (image
    :documentation "The image to be used in this tileset"
    :type tiled-image
    :initarg :image
    :reader tileset-image)
   (tiles
    :documentation "The tiles in this terrain"
    :type list
    :initarg :tiles
    :reader tileset-tiles)
   (terrains
    :documentation "The terrain types in this tileset"
    :type list
    :initarg :terrains
    :reader tileset-terrains)))

(defclass embedded-tileset (tileset)
  ()
  (:documentation
   "A `tileset' that is embedded in the same file defining the map."))

(defclass external-tileset (tileset)
  ((source
    :documentation "The path to this tileset"
    :type pathname
    :initarg :source
    :reader tileset-source))
  (:documentation
   "A `tileset' that is stored in a separate file from the map."))

(defun tileset-last-gid (tileset)
  (with-slots (first-gid image tile-width tile-height)
      tileset
    (+ (* (truncate (image-width image) tile-width)
          (truncate (image-height image) tile-height))
       first-gid -1)))

(defclass layer (properties-mixin)
  ((map
    :documentation "The map containing this layer"
    :type tiled-map
    :initarg :map
    :reader layer-map)
   (parent
    :documentation "The parent group layer containing this layer, if any"
    :type (or null group-layer)
    :initarg :parent
    :reader layer-parent)
   (name
    :documentation "The name of this layer"
    :type string
    :initarg :name
    :reader layer-name)
   (opacity
    :documentation "The opacity of this layer, from 0 to 1"
    :type (real 0 1)
    :initarg :opacity
    :reader layer-opacity)
   (visible
    :documentation "Whether or not the layer is visible"
    :type boolean
    :initarg :visible
    :reader layer-visible)
   (offset-x
    :documentation "The horizontal rendering offset for this layer, in pixels"
    :type integer
    :initarg :offset-x
    :reader layer-offset-x)
   (offset-y
    :documentation "The vertical rendering offset for this layer, in piyels"
    :type integer
    :initarg :offset-y
    :reader layer-offset-y)))

(defun layer-tile-width (layer)
  (map-tile-width (layer-map layer)))

(defun layer-tile-height (layer)
  (map-tile-height (layer-map layer)))

(defun layer-full-offsets (layer)
  (if (layer-parent layer)
      (multiple-value-bind (x y)
          (layer-full-offsets (layer-parent layer))
        (values (+ x (layer-offset-x layer))
                (+ y (layer-offset-y layer))))
      (values (layer-offset-x layer)
              (layer-offset-y layer))))

(defclass cell ()
  ((layer
    :documentation "The tile-layer containing this cell"
    :type tile-layer
    :initarg :layer
    :reader cell-layer)
   (tile
    :documentation "The tile at this cell"
    :type tiled-tile
    :initarg :tile
    :reader cell-tile)
   (column
    :documentation "The column to draw this cell to, relative to its containing layer"
    :type integer
    :initarg :column
    :reader cell-column)
   (row
    :documentation "The row to draw this cell to, relative to its containing layer"
    :type integer
    :initarg :row
    :reader cell-row)))

(defun cell-x (cell)
  (* (cell-column cell)
     (layer-tile-width (cell-layer cell))))

(defun cell-y (cell)
  (* (cell-row cell)
     (layer-tile-height (cell-layer cell))))

(defun cell-full-offsets (cell)
  (multiple-value-bind (x y)
      (layer-full-offsets (cell-layer cell))
    (values (+ x (cell-x cell))
            (+ y (cell-y cell)))))

(defclass tile-layer (layer)
  ((cells
    :documentation "The cells on this layer"
    :type list
    :initarg :cells
    :reader layer-cells)))

(defclass object (properties-mixin)
  ((id
    :documentation "Unique ID of the object."
    :type integer
    :initarg :id
    :reader object-id)
   (name
    :documentation "The name of the object. An arbitrary string."
    :type string
    :initarg :name
    :reader object-name)
   (type
    :documentation "The type of the object. An arbitrary string."
    :type string
    :initarg :type
    :reader object-type)
   (x
    :documentation "The x coordinate of the object, in pixels."
    :type integer
    :initarg :x
    :reader object-x)
   (y
    :documentation "The y coordinate of the object, in pixels."
    :type integer
    :initarg :y
    :reader object-y)
   (rotation
    :documentation "The clockwise rotation of the object, in degrees."
    :type float
    :initarg :rotation
    :reader object-rotation)
   (visible
    :documentation "Whether or not the object is visible."
    :type boolean
    :initarg :visible
    :reader object-visible)))

(defclass rect-object (object)
  ((width
    :documentation "The width of the rectangle, in pixels."
    :type integer
    :initarg :width
    :reader rect-width)
   (height
    :documentation "The height of the rectangle, in pixels."
    :type integer
    :initarg :height
    :reader rect-height)))

(defclass ellipse-object (object)
  ((rx
    :documentation "The horizontal radius of this ellipse, in pixels"
    :type integer
    :initarg :rx
    :reader ellipse-rx)
   (ry
    :documentation "The vertical radius of this ellipse, in pixels"
    :type integer
    :initarg :ry
    :reader ellipse-ry)))

(defclass polygon-object (object)
  ((vertices
    :documentation "A list of the form (x . y) of vertices, in pixels.
These coordinates are relative to the x and y of the object"
    :type list
    :initarg :vertices
    :reader polygon-vertices)))

(defclass polyline-object (object)
  ((points
    :documentation "A list of the form (x . y) of points, in pixels.
These coordinates are relative to the x and y of the object"
    :type list
    :initarg :points
    :reader polyline-points)))

(defclass tile-object (object)
  ((tile
    :documentation "The tile used by this object."
    :type tiled-tile
    :initarg :tile
    :reader object-tile)))

(deftype horizontal-alignment ()
  "Horizontal alignment of text"
  '(member :left :center :right))

(deftype vertical-alignment ()
  "Vertical alignment of text"
  '(member :top :center :bottom))

(defclass text-object (object)
  ((string
    :documentation "The string of text"
    :type string
    :initarg :string
    :reader text-string)
   (font-family
    :documentation "The font family used"
    :type string
    :initarg :font-family
    :reader text-font-family)
   (pixel-size
    :documentation "The size of the font, in pixels (not points)"
    :type integer
    :initarg :pixel-size
    :reader text-pixel-size)
   (wrap
    :documentation "Whether word wrapping is enabled or not"
    :type boolean
    :initarg :wrap
    :reader text-wrap)
   (color
    :documentation "The color of the text"
    :type tiled-color
    :initarg :color
    :reader text-color)
   (bold
    :documentation "Whether bold is enabled or not"
    :type boolean
    :initarg :bold
    :reader text-bold)
   (italic
    :documentation "Whether italic is enabled or not"
    :type boolean
    :initarg :italic
    :reader text-italic)
   (underline
    :documentation "Whether underline is enabled or not"
    :type boolean
    :initarg :underline
    :reader text-underline)
   (strikeout
    :documentation "Whether strikeout is enabled or not"
    :type boolean
    :initarg :strikeout
    :reader text-strikeout)
   (kerning
    :documentation "Whether kerning is enabled or not"
    :type boolean
    :initarg :kerning
    :reader text-kerning)
   (halign
    :documentation "Horizontal alignment of the text"
    :type horizontal-alignment
    :initarg :halign
    :reader text-halign)
   (valign
    :documentation "Horizontal alignment of the text"
    :type vertical-alignment
    :initarg :valign
    :reader text-valign)))

(defclass image-object (object)
  ((image
    :documentation "The image displayed by this object"
    :type tiled-image
    :initarg :image
    :reader object-image)))

(deftype draw-order ()
  "Draw order for objects in an `object-group'.
  top-down - sorted by y coordinate
  index - manual stacking, meaning drawn in defined order"
  '(member :top-down :index))

(defclass object-layer (layer object-group)
  ())

(defclass image-layer (layer)
  ((image
    :documentation "The image displayed by this layer."
    :type tiled-image
    :initarg :image
    :reader layer-image)))

(defclass group-layer (layer)
  ((layers
    :documentation "The layers nested in this group"
    :type list
    :initarg :layers
    :reader group-layers)))

(deftype orientation ()
  "Orientation of the map"
  '(member :orthogonal :isonometric :staggered :hexagonal))

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

(defclass tiled-map (properties-mixin)
  ((version
    :documentation "The TMX format version"
    :type string
    :initarg :version
    :reader map-version)
   (tiled-version
    :documentation "The Tiled version used to save the file"
    :type string
    :initarg :tiled-version
    :reader map-tiled-version)
   (orientation
    :documentation "The orientation of the map"
    :type orientation
    :initarg :orientation
    :reader map-orientation)
   (render-order
    :documentation "The order in which tiles on each layer are rendered."
    :type render-order
    :initarg :render-order
    :reader map-render-order)
   (width
    :documentation "Width of this map, in tiles."
    :type integer
    :initarg :width
    :reader map-width)
   (height
    :documentation "Height of this map, in tiles."
    :type integer
    :initarg :height
    :reader map-height)
   (tile-width
    :documentation "The width of each tile in this map, in pixels."
    :type integer
    :initarg :tile-width
    :reader map-tile-width)
   (tile-height
    :documentation "The height of each tile in this map, in pixels."
    :type integer
    :initarg :tile-height
    :reader map-tile-height)
   (background-color
    :documentation "The background color of this map."
    :type tiled-color
    :initarg :background-color
    :reader map-background-color)
   (tilesets
    :documentation "All the tilesets of this map."
    :type list
    :initarg :tilesets
    :reader map-tilesets)
   (layers
    :documentation "All the layers of this map."
    :type list
    :initarg :layers
    :reader map-layers)))

(defun map-tile-layers (map)
  (loop :for layer :in (map-layers map)
     :if (typep layer 'tile-layer)
     :collect layer))

(defun map-object-layers (map)
  (loop :for layer :in (map-layers map)
     :if (typep layer 'object-layer)
     :collect layer))

(defun map-image-layers (map)
  (loop :for layer :in (map-layers map)
     :if (typep layer 'image-layer)
     :collect layer))

(defun map-width-pixels (map)
  (* (map-tile-width map)
     (map-width map)))

(defun map-height-pixels (map)
  (* (map-tile-height map)
     (map-height map)))

(defun load-map (path
                 &aux
                   (tmap
                    (eswitch ((pathname-type path) :test 'string-equal)
                      ("tmx"
                       (parse-xml-map-file path))
                      ("json"
                       (parse-json-map-file path)))))
  (uiop:with-current-directory ((uiop:pathname-directory-pathname path))
    (with-slots (version tiled-version orientation render-order
                         width height tile-width tile-height background-color
                         tilesets layers properties)
        tmap
      (let* ((loaded-tilesets
              (mapcar #'%load-tileset tilesets))
             (ret
              (make-instance
               'tiled-map
               :version (or version "0.0")
               :tiled-version (or tiled-version "0.0.0")
               :orientation (or orientation :orthogonal)
               :render-order (or render-order :right-down)
               :width width
               :height height
               :tile-width tile-width
               :tile-height tile-height
               :background-color (or background-color +transparent+)
               :tilesets loaded-tilesets
               :properties properties))
             (loaded-layers
              (mapcar
               (lambda (l)
                 (etypecase l
                   (ttile-layer (%load-tile-layer l ret nil))
                   (tobject-group (%load-object-layer l ret nil))
                   (timage-layer (%load-image-layer l ret nil))
                   (tlayer-group (%load-layer-group l ret nil))))
               layers)))
        (setf (slot-value ret 'layers) loaded-layers)
        ret))))

(defun load-tileset (path)
  (%load-external-tileset path 0))

(defun %find-tile (tgid tilesets)
  (loop
     :for tileset :in tilesets
     :for tlid := (- tgid (tileset-first-gid tileset))
     :if (<= 0
             tlid
             (1- (tileset-tile-count tileset)))
     :return
     (let ((tile (find tlid
                       (tileset-tiles tileset)
                       :key #'tile-id)))
       (unless tile
         (setf tile (make-instance
                     'tiled-tile
                     :id tlid
                     :tileset tileset))
         (push tile (slot-value tileset 'tiles))
         (setf (slot-value tileset 'tiles) (sort (slot-value tileset 'tiles) #'< :key #'tile-id)))
       tile)))

(defun %load-tile-layer (tlayer map parent
                         &aux (tilesets (map-tilesets map)))
  (with-slots (name opacity visible width height offset-x
                    offset-y tile-data properties)
      tlayer
    (let ((ret (make-instance
                'tile-layer
                :map map
                :parent parent
                :name name
                :opacity opacity
                :visible visible
                :offset-x offset-x
                :offset-y offset-y
                :properties properties)))
      (setf (slot-value ret 'cells)
            (loop
               :for i :from 0
               :for tgid :in (ttile-data-tiles tile-data)
               :for tile := (%find-tile tgid tilesets)
               :when tile
               :collect
               (multiple-value-bind (row col)
                   (truncate i width)
                 (make-instance
                  'cell
                  :row row
                  :column col
                  :tile tile
                  :layer ret))))
      ret)))

(defun %load-object-layer (tgroup map parent
                           &aux (tilesets (map-tilesets map)))
  (with-slots (name opacity visible width height offset-x
                    offset-y draw-order (tobjects objects) properties)
      tgroup
    (let ((ret
           (make-instance
            'object-layer
            :map map
            :parent parent
            :name name
            :opacity opacity
            :visible visible
            :offset-x offset-x
            :offset-y offset-y
            :draw-order draw-order
            :objects (%load-objects tobjects)
            :properties properties)))
      (%finalize-object-layer ret tobjects tilesets)
      ret)))

(defun %finalize-object-layer (object-layer tobjects tilesets)
  (%finalize-objects (object-group-objects object-layer) tobjects tilesets))

(defun %load-object (tobject)
  (with-slots (id name type x y width height rotation gid visible properties
                  ellipse polygon polyline text image)
      tobject
    (cond
      (ellipse
       (make-instance
        'ellipse-object
        :id id
        :name name
        :type type
        :x x
        :y y
        :rotation (or rotation 0)
        :visible visible
        :rx width
        :ry height
        :properties properties))
      (polygon
       (with-slots (points)
           polygon
         (make-instance
          'polygon-object
          :id id
          :name name
          :type type
          :x x
          :y y
          :rotation (or rotation 0)
          :visible visible
          :vertices
          (mapcar
           (lambda (tpoint)
             (cons (tpoint-x tpoint)
                   (tpoint-y tpoint)))
           points))))
      (polyline
       (with-slots (points)
           polyline
         (make-instance
          'polyline-object
          :id id
          :name name
          :type type
          :x x
          :y y
          :rotation (or rotation 0)
          :visible visible
          :points (mapcar
                   (lambda (tpoint)
                     (cons (tpoint-x tpoint)
                           (tpoint-y tpoint)))
                   points))))
      (text
       (with-slots (text font-family pixel-size wrap color
                         bold italic underline strikeout kerning
                         halign valign)
           text
         (make-instance
          'text-object
          :id id
          :name name
          :type type
          :x x
          :y y
          :rotation (or rotation 0)
          :visible visible
          :string (or text "")
          :font-family (or font-family "sans-serif")
          :pixel-size (or pixel-size 16)
          :wrap wrap
          :color (or color +black+)
          :bold bold
          :italic italic
          :underline underline
          :strikeout strikeout
          :kerning kerning
          :halign (or halign :left)
          :valign (or valign :top)
          :properties properties)))
      (gid
       (make-instance
        'tile-object
        :id id
        :name name
        :type type
        :x x
        :y y
        :rotation (or rotation 0)
        :properties properties
        :visible visible))
      (image
       (make-instance
        'image-object
        :id id
        :name name
        :type type
        :x x
        :y y
        :rotation (or rotation 0)
        :visible visible
        :image (%load-image image)
        :properties properties))
      (t
       (make-instance
        'rect-object
        :id id
        :name name
        :type type
        :x x
        :y y
        :rotation (or rotation 0)
        :visible visible
        :width width
        :height height
        :properties properties)))))

(defun %finalize-object (object tobject tilesets)
  (when (typep object 'tile-object)
    (setf (slot-value object 'tile)
          (%find-tile (tobject-gid tobject) tilesets))))

(defun %load-objects (tobjects)
  (mapcar #'%load-object tobjects))

(defun %finalize-objects (objects tobjects tilesets)
  (mapc
   (lambda (obj)
     (%finalize-object obj (find (object-id obj) tobjects :key #'tobject-id)  tilesets))
   objects))

(defun %load-image-layer (tlayer map parent)
  (with-slots (name opacity visible width height offset-x
                    offset-y image properties)
      tlayer
    (make-instance
     'image-layer
     :map map
     :parent parent
     :name name
     :opacity opacity
     :visible visible
     :offset-x offset-x
     :offset-y offset-y
     :image (%load-image image)
     :properties properties)))

(defun %load-layer-group (tlayer map parent)
  (with-slots (name offset-x offset-y opacity visible
                    layers properties)
      tlayer
    (let ((ret
           (make-instance
            'group-layer
            :map map
            :parent parent
            :name name
            :opacity opacity
            :visible visible
            :offset-x offset-x
            :offset-y offset-y
            :properties properties)))
      (setf (slot-value ret 'layers)
            (mapcar
             (lambda (l)
               (etypecase l
                 (ttile-layer (%load-tile-layer l map ret))
                 (tobject-group (%load-object-layer l map ret))
                 (timage-layer (%load-image-layer l map ret))
                 (tlayer-group (%load-layer-group l map ret))))
             layers))
      ret)))

(defun %load-tileset (ttileset)
  (with-slots (source first-gid)
      ttileset
    (if source
        (%load-external-tileset source first-gid)
        (%load-embedded-tileset ttileset))))

(defun %load-external-tileset (path first-gid
                     &aux
                       (full-path (uiop:ensure-absolute-pathname path *default-pathname-defaults*))
                       (ttileset
                        (eswitch ((pathname-type full-path) :test 'string-equal)
                          ("tsx"
                           (parse-xml-tileset-file full-path))
                          ("json"
                           (parse-json-tileset-file full-path)))))
  (with-slots (name tile-width tile-height columns
                    spacing margin tile-count tile-offset-x tile-offset-y
                    (timage image) (ttiles tiles) (tterrains terrains))
      ttileset
    (uiop:with-current-directory ((uiop:pathname-directory-pathname full-path))
      (let* ((image (%load-image timage))
             (tiles (%load-tiles ttiles))
             (terrains (%load-terrains tterrains tiles))
             (ret
              (make-instance
               'external-tileset
               :source full-path
               :name name
               :first-gid first-gid
               :tile-width tile-width
               :tile-height tile-height
               :spacing spacing
               :margin margin
               :tile-count tile-count
               :columns columns
               :offset-x tile-offset-x
               :offset-y tile-offset-y
               :image image
               :terrains terrains)))
        (%finalize-tiles tiles ttiles ret)
        (setf (slot-value ret 'tiles) tiles)
        ret))))

(defun %load-embedded-tileset (ttileset)
  (with-slots (name first-gid tile-width tile-height columns
                    spacing margin tile-count tile-offset-x tile-offset-y
                    (timage image) (ttiles tiles) (tterrains terrains))
      ttileset
      (let* ((image (%load-image timage))
             (tiles (%load-tiles ttiles))
             (terrains (%load-terrains tterrains tiles))
             (tileset
              (make-instance
               'embedded-tileset
               :name name
               :first-gid first-gid
               :tile-width tile-width
               :tile-height tile-height
               :spacing spacing
               :margin margin
               :tile-count tile-count
               :columns columns
               :offset-x tile-offset-x
               :offset-y tile-offset-y
               :image image
               :tiles tiles
               :terrains terrains)))
        (%finalize-tiles tiles ttiles tileset)
        tileset)))

(defun %load-image (timage)
  (if timage
      (with-slots (source width height transparent-color format image-data)
          timage
        (if source
            (make-instance
             'external-tiled-image
             :source (uiop:merge-pathnames* source)
             :transparent-color (or transparent-color +transparent+)
             :width (or width 0)
             :height (or height 0))
            (make-instance
             'embedded-tiled-image
             :format format
             :data image-data
             :transparent-color (or transparent-color +transparent+)
             :width (or width 0)
             :height (or height 0))))
      nil))

(defun %load-tiles (ttiles)
  (mapcar
   (lambda (tile)
     (with-slots (id type probability frames properties)
         tile
       (if frames
           (make-instance
            'animated-tile
            :id id
            :type type
            :probability probability
            :properties properties)
           (make-instance
            'tiled-tileset-tile
            :id id
            :type type
            :probability probability
            :properties properties))))
   ttiles))

(defun %load-terrains (tterrains tiles)
  (mapcar
   (lambda (terrain)
     (with-slots (name tile properties)
         terrain
       (make-instance
        'tiled-terrain
        :name name
        :tile (find tile tiles :key #'tile-id)
        :properties properties)))
   tterrains))

(defun %finalize-tiles (tiles ttiles tileset)
  (loop
     :for tile :in tiles
     :for ttile :in ttiles
     :for terrains := (tileset-terrains tileset)
     :for terrains-v := (ttileset-tile-terrain ttile)
     :for tgroup := (ttileset-tile-object-group ttile)
     :do
     (setf (slot-value tile 'tileset) tileset)
     (setf (slot-value tile 'terrains)
           (vector (and (svref terrains-v 0) (elt terrains (svref terrains-v 0)))
                   (and (svref terrains-v 1) (elt terrains (svref terrains-v 1)))
                   (and (svref terrains-v 2) (elt terrains (svref terrains-v 2)))
                   (and (svref terrains-v 3) (elt terrains (svref terrains-v 3)))))
     (when tgroup
       (setf (slot-value tile 'object-group)
             (make-instance
              'object-group
              :draw-order (tobject-group-draw-order tgroup)
              :objects (%load-objects (tobject-group-objects tgroup)))))

     (when (typep tile 'animated-tile)
       (setf (slot-value tile 'frames)
             (mapcar
              (lambda (tframe)
                (with-slots (duration tile-id)
                    tframe
                  (make-instance
                   'tiled-frame
                   :duration duration
                   :tile (find tile-id tiles :key #'tile-id))))
              (ttileset-tile-frames ttile))))))