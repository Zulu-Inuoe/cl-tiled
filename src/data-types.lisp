(defpackage #:cl-tiled.data-types
  (:use #:cl)
  (:import-from
   #:alexandria
   #:required-argument)
  (:export
   #:tiled-color
   #:tiled-color-a
   #:tiled-color-r
   #:tiled-color-g
   #:tiled-color-b
   #:make-tiled-color

   #:properties-mixin
   #:properties

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
   #:class-property

   #:tiled-image
   #:image-transparent-color
   #:image-width
   #:image-height

   #:embedded-tiled-image
   #:image-format
   #:image-data

   #:external-tiled-image
   #:image-source

   #:object-group
   #:object-group-objects
   #:object-group-draw-order

   #:tiled-tile
   #:tile-tileset
   #:tile-id
   #:tile-gid
   #:tile-column
   #:tile-row
   #:tile-pixel-x
   #:tile-pixel-y
   #:tile-width
   #:tile-height

   #:tile-image

   #:tiled-tileset-tile
   #:tile-type
   #:tile-terrains
   #:tile-probability
   #:tile-object-group

   #:tiled-tileset-image-tile

   #:tiled-frame
   #:frame-tile
   #:frame-duration

   #:animated-tile
   #:tile-frames

   #:tiled-terrain
   #:terrain-name
   #:terrain-tile

   #:tileset
   #:tileset-name
   #:tileset-first-gid
   #:tileset-tile-width
   #:tileset-tile-height
   #:tileset-spacing
   #:tileset-margin
   #:tileset-tile-count
   #:tileset-columns
   #:tileset-offset-x
   #:tileset-offset-y
   #:tileset-image
   #:tileset-tiles
   #:tileset-terrains

   #:embedded-tileset

   #:external-tileset
   #:tileset-source

   #:layer
   #:layer-map
   #:layer-parent
   #:layer-name
   #:layer-opacity
   #:layer-visible
   #:layer-offset-x
   #:layer-offset-y
   #:layer-repeat-x
   #:layer-repeat-y
   #:layer-tile-width
   #:layer-tile-height

   #:layer-full-offsets

   #:cell
   #:cell-layer
   #:cell-tile
   #:cell-flipped-anti-diagonal
   #:cell-flipped-horizontal
   #:cell-flipped-vertical
   #:cell-column
   #:cell-row
   #:cell-x
   #:cell-y

   #:cell-full-offsets

   #:tile-layer
   #:layer-cells

   #:object
   #:object-id
   #:object-name
   #:object-type
   #:object-x
   #:object-y
   #:object-rotation
   #:object-visible
   #:object-template

   #:rect-object
   #:rect-width
   #:rect-height

   #:ellipse-object
   #:ellipse-rx
   #:ellipse-ry

   #:polygon-object
   #:polygon-vertices

   #:polyline-object
   #:polyline-points

   #:tile-object
   #:object-tile
   #:object-height
   #:object-width
   #:object-flipped-anti-diagonal
   #:object-flipped-horizontal
   #:object-flipped-vertical

   #:horizontal-alignment
   #:vertical-alignment

   #:text-object
   #:text-string
   #:text-font-family
   #:text-pixel-size
   #:text-wrap
   #:text-color
   #:text-bold
   #:text-italic
   #:text-underline
   #:text-strikeout
   #:text-kerning
   #:text-halign
   #:text-valign

   #:image-object
   #:object-image

   #:draw-order
   #:object-layer

   #:image-layer
   #:layer-image

   #:group-layer
   #:group-layers

   #:orientation

   #:render-order

   #:stagger-axis

   #:stagger-index

   #:tiled-map
   #:map-version
   #:map-tiled-version
   #:map-orientation
   #:map-render-order
   #:map-width
   #:map-height
   #:map-tile-width
   #:map-tile-height
   #:map-background-color
   #:map-tilesets
   #:map-layers

   #:map-width-pixels
   #:map-height-pixels

   #:map-tile-layers
   #:map-object-layers
   #:map-image-layers))

(in-package #:cl-tiled.data-types)

(defstruct tiled-color
  (r #x00 :type (unsigned-byte 8))
  (g #x00 :type (unsigned-byte 8))
  (b #x00 :type (unsigned-byte 8))
  (a #xFF :type (unsigned-byte 8)))

(deftype property-type ()
  '(member :string :int :float :bool :color :file :class))

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

(defclass class-property (property properties-mixin)
  ())

(defmethod property-value ((property class-property))
  (properties property))

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
    :file)
  (:method ((property class-property))
    :class))

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

(defclass properties-mixin ()
  ((properties
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :reader properties)))

(defmethod initialize-instance :after ((obj properties-mixin)
                                       &key (properties nil)
                                       &aux (hash (properties obj)))
  (dolist (property properties)
    (setf (gethash (property-name property) hash) (property-value property))))

(defun property-val (obj prop-name)
  (values (gethash prop-name (properties obj))))

(deftype draw-order ()
  "Draw order for objects in an `object-group'.
  top-down - sorted by y coordinate
  index - manual stacking, meaning drawn in defined order"
  '(member :top-down :index))

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
    :reader tile-id))
  (:documentation
   "A simple tile belonging to a tileset, with no individual properties."))


(defun tile-gid (tile)
  "Get global tile ID, unique within the map."
  (+ (tile-id tile)
     (tileset-first-gid (tile-tileset tile))))

(defgeneric tile-image (tile)
  (:method ((tile tiled-tile))
    (tileset-image (tile-tileset tile))))

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
    :type (simple-array (or null tiled-terrain) (4))
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
   "A tile specified in a tilesheet, with additional properties."))

(defclass tiled-tileset-image-tile (tiled-tileset-tile)
  ((image
    :documentation "The individualized image for this tile."
    :type tiled-image
    :initarg :image
    :reader tile-image))
  (:documentation
   "A specified tilesheet with a dedicated image."))

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
  "Get the `column' of this tile in its tilset."
  (let ((tileset-columns (tileset-columns (tile-tileset tile))))
    (if (zerop tileset-columns)
        0
        (mod (tile-id tile) tileset-columns))))

(defun tile-row (tile)
  "Get the `row' of this tile in its tilset."
  (let ((tileset-columns (tileset-columns (tile-tileset tile))))
    (if (zerop tileset-columns)
        0
        (values (truncate (tile-id tile) tileset-columns)))))

(defun tile-pixel-x (tile
                     &aux
                       (column (tile-column tile))
                       (tileset (tile-tileset tile)))
  "Get the pixel x coordinate of this `tile' in its tileset.
 This indicates the left pixel in the tileset image."
  (+ (* column (tileset-tile-width tileset))
     (* column (tileset-spacing tileset))
     (tileset-offset-x tileset)
     (tileset-margin tileset)))

(defun tile-pixel-y (tile
                     &aux
                       (row (tile-row tile))
                       (tileset (tile-tileset tile)))
  "Get the pixel y coordinate of this `tile' in its tileset.
 This indicates the top pixel in the tileset image."
  (+ (* row (tileset-tile-height tileset))
     (* row (tileset-spacing tileset))
     (tileset-offset-y tileset)
     (tileset-margin tileset)))

(defun tile-width (tile)
  "Get the width of `tile', in pixels."
  (tileset-tile-width (tile-tileset tile)))

(defun tile-height (tile)
  "Get the height of `tile', in pixels."
  (tileset-tile-height (tile-tileset tile)))

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
    :type (or null tiled-tile)
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
   (flipped-anti-diagonal
    :documentation "The tile is flipped anti-diagonally"
    :type boolean
    :initarg :flipped-anti-diagonal
    :reader cell-flipped-anti-diagonal)
   (flipped-horizontal
    :documentation "The tile is flipped horizontally"
    :type boolean
    :initarg :flipped-horizontal
    :reader cell-flipped-horizontal)
   (flipped-vertical
    :documentation "The tile is flipped vertically"
    :type boolean
    :initarg :flipped-vertical
    :reader cell-flipped-vertical)
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
    :reader object-visible)
   (template
    :documentation "An object serving as a template of this object."
    :type (or object null)
    :initarg :template
    :initform nil
    :reader object-template)))

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
    :type (or null tiled-tile)
    :initarg :tile
    :reader object-tile)
   (width
    :documentation "The width of the object, in pixels."
    :type integer
    :initarg :width
    :reader object-width)
   (height
    :documentation "The height of the object, in pixels"
    :type integer
    :initarg :height
    :reader object-height)
   (flipped-anti-diagonal
    :documentation "The tile is flipped anti-diagonally"
    :type boolean
    :initarg :flipped-anti-diagonal
    :reader object-flipped-anti-diagonal)
   (flipped-horizontal
    :documentation "The tile is flipped horizontally"
    :type boolean
    :initarg :flipped-horizontal
    :reader object-flipped-horizontal)
   (flipped-vertical
    :documentation "The tile is flipped vertically"
    :type boolean
    :initarg :flipped-vertical
    :reader object-flipped-vertical)))

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

(defclass object-layer (layer object-group)
  ())

(defclass image-layer (layer)
  ((image
    :documentation "The image displayed by this layer."
    :type tiled-image
    :initarg :image
    :reader layer-image)
   (repeat-x
    :documentation "The image repeats horizontally."
    :type boolean
    :initarg :repeat-x
    :reader layer-repeat-x)
   (repeat-y
    :documentation "The image repeats vertically."
    :type boolean
    :initarg :repeat-y
    :reader layer-repeat-y)))

(defclass group-layer (layer)
  ((layers
    :documentation "The layers nested in this group"
    :type list
    :initarg :layers
    :reader group-layers)))

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
