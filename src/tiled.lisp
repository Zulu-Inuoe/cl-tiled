(defpackage #:cl-tiled
  (:use
   #:cl
   #:cl-tiled.data-types
   #:cl-tiled.impl
   #:cl-tiled.impl.xml
   #:cl-tiled.impl.json)
  (:import-from
   #:alexandria
   #:if-let
   #:eswitch
   #:read-file-into-string)
  (:import-from
   #:cl-tiled.data-types
   #:layers
   #:tiles
   #:cells
   #:tile
   #:tileset
   #:terrains
   #:object-group
   #:frames)
  (:export
   #:load-map
   #:load-tileset

   #:tiled-color
   #:tiled-color-r
   #:tiled-color-g
   #:tiled-color-b
   #:tiled-color-a

   #:properties-mixin
   #:properties

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
   #:object-width
   #:object-height
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

(in-package #:cl-tiled)

(defun %load-map (tmap path resource-loader)
  (let* ((loaded-tilesets
           (uiop:with-pathname-defaults ((uiop:pathname-directory-pathname path))
             (flet ((%%load-tileset (tileset)
                      (%load-tileset tileset resource-loader)))
               (mapcar #'%%load-tileset (tmap-tilesets tmap)))))
         (ret
           (make-instance
            'tiled-map
            :version (or (tmap-version tmap) "0.0")
            :tiled-version (or (tmap-tiled-version tmap) "0.0.0")
            :orientation (or (tmap-orientation tmap) :orthogonal)
            :render-order (or (tmap-render-order tmap) :right-down)
            :width (tmap-width tmap)
            :height (tmap-height tmap)
            :tile-width (tmap-tile-width tmap)
            :tile-height (tmap-tile-height tmap)
            :background-color (or (tmap-background-color tmap) +transparent+)
            :tilesets loaded-tilesets
            :properties (tmap-properties tmap)))
         (loaded-layers
           (mapcar
            (lambda (l)
              (etypecase l
                (ttile-layer (%load-tile-layer l ret nil))
                (tobject-group (%load-object-layer l ret nil))
                (timage-layer (%load-image-layer l ret nil))
                (tlayer-group (%load-layer-group l ret nil))))
            (tmap-layers tmap))))
    (setf (slot-value ret 'layers) loaded-layers)
    ret))

(defun load-map (path &optional (resource-loader #'read-file-into-string)
                 &aux
                   (tmap
                    (eswitch ((pathname-type path) :test 'string-equal)
                      ("tmx"
                       (with-input-from-string (stream (funcall resource-loader path))
                         (parse-xml-map-stream stream path)))
                      ("json"
                       (with-input-from-string (stream (funcall resource-loader path))
                         (parse-json-map-stream stream path))))))
  (%load-map tmap path resource-loader))

(defun load-tileset (path)
  (%load-external-tileset path 0 #'read-file-into-string))

(defun %find-tile (tgid tilesets)
  (loop
    :for tileset :in tilesets
    :for tlid := (- tgid (tileset-first-gid tileset))
    :if (<= 0 tlid (tileset-tile-count tileset))
      :return
      (let ((tile (find tlid (tileset-tiles tileset) :key #'tile-id)))
        (unless tile
          (setf tile (make-instance 'tiled-tile :id tlid :tileset tileset))
          (push tile (slot-value tileset 'tiles))
          (setf (slot-value tileset 'tiles) (sort (slot-value tileset 'tiles) #'< :key #'tile-id)))
        tile)))

(defun %load-tile-layer (tlayer map parent
                         &aux (tilesets (map-tilesets map)))
  (let ((ret (make-instance
              'tile-layer
              :map map
              :parent parent
              :name (tlayer-name tlayer)
              :opacity (tlayer-opacity tlayer)
              :visible (tlayer-visible tlayer)
              :offset-x (tlayer-offset-x tlayer)
              :offset-y (tlayer-offset-y tlayer)
              :properties (tlayer-properties tlayer))))
    (setf (slot-value ret 'cells)
          (loop
            :for i :from 0
            :for tgid :in (ttile-data-tiles (ttile-layer-tile-data tlayer))
            :for tile := (%find-tile (mask-field (byte 29 0) tgid) tilesets)
            :when tile
              :collect
              (multiple-value-bind (row col)
                  (truncate i (tlayer-width tlayer))
                (make-instance
                 'cell
                 :row row
                 :column col
                 :tile tile
                 :flipped-anti-diagonal (logbitp 29 tgid)
                 :flipped-vertical (logbitp 30 tgid)
                 :flipped-horizontal (logbitp 31 tgid)
                 :layer ret))))
    ret))

(defun %load-object-layer (tgroup map parent
                           &aux (tilesets (map-tilesets map)))
  (let ((ret
          (make-instance
           'object-layer
           :map map
           :parent parent
           :name (tlayer-name tgroup)
           :opacity (tlayer-opacity tgroup)
           :visible (tlayer-visible tgroup)
           :offset-x (tlayer-offset-x tgroup)
           :offset-y (tlayer-offset-y tgroup)
           :draw-order (or (tobject-group-draw-order tgroup) :top-down)
           :objects (%load-objects (tobject-group-objects tgroup))
           :properties (tlayer-properties tgroup))))
    (%finalize-object-layer ret (tobject-group-objects tgroup) tilesets)
    ret))

(defun %finalize-object-layer (object-layer tobjects tilesets)
  (%finalize-objects (object-group-objects object-layer) tobjects tilesets)

  ;; Sort by y coordinate when :top-down
  (when (eq (object-group-draw-order object-layer) :top-down)
    (with-slots (objects) object-layer
      (setf objects (sort objects #'< :key #'object-y))))
  (values))

(defun %load-object (tobject)
  (let ((id (tobject-id tobject))
        (name (tobject-name tobject))
        (type (tobject-type tobject))
        (x (tobject-x tobject))
        (y (tobject-y tobject))
        (width (or (tobject-width tobject) 0))
        (height (or (tobject-height tobject) 0))
        (rotation (tobject-rotation tobject))
        (visible (tobject-visible tobject))
        (properties (tobject-properties tobject))
        (ellipse (tobject-ellipse tobject))
        (polygon (tobject-polygon tobject))
        (polyline (tobject-polyline tobject))
        (text (tobject-text tobject))
        (gid (tobject-gid tobject))
        (image (tobject-image tobject)))
    (cond
      (ellipse
       (make-instance
        'ellipse-object
        :id id
        :name name
        :type type
        :x x
        :y y
        :rotation (or rotation 0.0)
        :visible visible
        :rx (/ width 2)
        :ry (/ height 2)
        :properties properties))
      (polygon
       (make-instance
        'polygon-object
        :id id
        :name name
        :type type
        :x x
        :y y
        :rotation (or rotation 0.0)
        :visible visible
        :vertices (tpolygon-points polygon)))
      (polyline
       (make-instance
        'polyline-object
        :id id
        :name name
        :type type
        :x x
        :y y
        :rotation (or rotation 0.0)
        :visible visible
        :points (tpolyline-points polyline)))
      (text
       (make-instance
        'text-object
        :id id
        :name name
        :type type
        :x x
        :y y
        :rotation (or rotation 0.0)
        :visible visible
        :string (ttext-text text)
        :font-family (or (ttext-font-family text) "sans-serif")
        :pixel-size (or (ttext-pixel-size text) 16)
        :wrap (ttext-wrap text)
        :color (or (ttext-color text) +black+)
        :bold (ttext-bold text)
        :italic (ttext-italic text)
        :underline (ttext-underline text)
        :strikeout (ttext-strikeout text)
        :kerning (ttext-kerning text)
        :halign (or (ttext-halign text) :left)
        :valign (or (ttext-valign text) :top)
        :properties properties))
      (gid
       (make-instance
        'tile-object
        :id id
        :name name
        :type type
        :x x
        :y y
        :width width
        :height height
        :rotation (or rotation 0.0)
        :properties properties
        :visible visible
        :flipped-anti-diagonal (logbitp 29 gid)
        :flipped-vertical (logbitp 30 gid)
        :flipped-horizontal (logbitp 31 gid)))
      (image
       (make-instance
        'image-object
        :id id
        :name name
        :type type
        :x x
        :y y
        :rotation (or rotation 0.0)
        :visible visible
        :image image
        :properties properties))
      (t
       (make-instance
        'rect-object
        :id id
        :name name
        :type type
        :x x
        :y y
        :rotation (or rotation 0.0)
        :visible visible
        :width width
        :height height
        :properties properties)))))

(defun %finalize-object (object tobject tilesets)
  (when (typep object 'tile-object)
    (setf (slot-value object 'tile)
          (or (%find-tile (mask-field (byte 29 0) (tobject-gid tobject)) tilesets)
              (break)))))

(defun %load-objects (tobjects)
  (mapcar #'%load-object tobjects))

(defun %finalize-objects (objects tobjects tilesets)
  (mapc
   (lambda (obj)
     (%finalize-object obj (find (object-id obj) tobjects :key #'tobject-id) tilesets))
   objects))

(defun %load-image-layer (tlayer map parent)
  (make-instance
   'image-layer
   :map map
   :parent parent
   :name (tlayer-name tlayer)
   :opacity (tlayer-opacity tlayer)
   :visible (tlayer-visible tlayer)
   :offset-x (tlayer-offset-x tlayer)
   :offset-y (tlayer-offset-y tlayer)
   :image (timage-layer-image tlayer)
   :properties (tlayer-properties tlayer)))

(defun %load-layer-group (tlayer map parent)
  (let ((ret
          (make-instance
           'group-layer
           :map map
           :parent parent
           :name (tlayer-name tlayer)
           :opacity (tlayer-opacity tlayer)
           :visible (tlayer-visible tlayer)
           :offset-x (tlayer-offset-x tlayer)
           :offset-y (tlayer-offset-y tlayer)
           :properties (tlayer-properties tlayer))))
    (setf (slot-value ret 'layers)
          (mapcar
           (lambda (l)
             (etypecase l
               (ttile-layer (%load-tile-layer l map ret))
               (tobject-group (%load-object-layer l map ret))
               (timage-layer (%load-image-layer l map ret))
               (tlayer-group (%load-layer-group l map ret))))
           (tlayer-group-layers tlayer)))
    ret))

(defun %load-tileset (ttileset resource-loader)
  (if-let ((source (ttileset-source ttileset)))
    (%load-external-tileset source (ttileset-first-gid ttileset) resource-loader)
    (%load-embedded-tileset ttileset)))

(defun %load-external-tileset (path first-gid resource-loader
                               &aux
                                 (full-path (uiop:ensure-absolute-pathname path *default-pathname-defaults*))
                                 (data (funcall resource-loader full-path))
                                 (ttileset
                                  (eswitch ((pathname-type full-path) :test 'string-equal)
                                    ("tsx"
                                     (with-input-from-string (stream data)
                                       (parse-xml-tileset-stream stream full-path)))
                                    ("json"
                                     (with-input-from-string (stream data)
                                       (parse-json-tileset-stream stream full-path))))))
  (let* ((tiles (%load-tiles (ttileset-tiles ttileset)))
         (terrains (%load-terrains (ttileset-terrains ttileset) tiles))
         (ret
           (make-instance
            'external-tileset
            :source full-path
            :name (ttileset-name ttileset)
            :first-gid first-gid
            :tile-width (ttileset-tile-width ttileset)
            :tile-height (ttileset-tile-height ttileset)
            :spacing (ttileset-spacing ttileset)
            :margin (ttileset-margin ttileset)
            :tile-count (ttileset-tile-count ttileset)
            :columns (ttileset-columns ttileset)
            :offset-x (ttileset-tile-offset-x ttileset)
            :offset-y (ttileset-tile-offset-y ttileset)
            :image (ttileset-image ttileset)
            :terrains terrains)))
    (%finalize-tiles tiles (ttileset-tiles ttileset) ret)
    (setf (slot-value ret 'tiles) tiles)
    ret))

(defun %load-embedded-tileset (ttileset)
  (let* ((tiles (%load-tiles (ttileset-tiles ttileset)))
         (terrains (%load-terrains (ttileset-terrains ttileset) tiles))
         (tileset
           (make-instance
            'embedded-tileset
            :name (ttileset-name ttileset)
            :first-gid (ttileset-first-gid ttileset)
            :tile-width (ttileset-tile-width ttileset)
            :tile-height (ttileset-tile-height ttileset)
            :spacing (ttileset-spacing ttileset)
            :margin (ttileset-margin ttileset)
            :tile-count (ttileset-tile-count ttileset)
            :columns (ttileset-columns ttileset)
            :offset-x (ttileset-tile-offset-x ttileset)
            :offset-y (ttileset-tile-offset-y ttileset)
            :image (ttileset-image ttileset)
            :tiles tiles
            :terrains terrains)))
    (%finalize-tiles tiles (ttileset-tiles ttileset) tileset)
    tileset))

(defun %load-tiles (ttiles)
  ;; It can happen that the tiles other than the first frame for an animated
  ;; tile may not be in ttiles. In this case, default blank tiles will be
  ;; collected and added at the end for them. The same must be done with ttiles.
  (let ((other-frame-tiles nil)
        (other-frame-tids nil)
        (other-frame-ttiles nil))
    (nconc
     (mapcar
      (lambda (tile)
        (cond
          ((ttileset-tile-frames tile)
           ;; Go through the other tiles and add them if they are not
           ;; found.
           (dolist (tframe (ttileset-tile-frames tile))
             (let* ((tile-id (tframe-tile-id tframe))
                    (tl1 (find tile-id ttiles :key #'ttileset-tile-id))
                    (tl2 (find tile-id other-frame-tids)))
               (unless (or tl1 tl2)
                 (push (make-instance 'tiled-tileset-tile
                                      :id tile-id
                                      :type ""
                                      :probability nil)
                       other-frame-tiles)
                 (push tile-id other-frame-tids)
                 (push (make-ttileset-tile :id tile-id)
                       other-frame-ttiles))))
           (make-instance
            'animated-tile
            :id (ttileset-tile-id tile)
            :type (ttileset-tile-type tile)
            :probability (ttileset-tile-probability tile)
            :properties (ttileset-tile-properties tile)))
          ((ttileset-tile-image tile)
           (make-instance
            'tiled-tileset-image-tile
            :id (ttileset-tile-id tile)
            :type (ttileset-tile-type tile)
            :probability (ttileset-tile-probability tile)
            :properties (ttileset-tile-properties tile)
            :image (ttileset-tile-image tile)))
          (t
           (make-instance
            'tiled-tileset-tile
            :id (ttileset-tile-id tile)
            :type (ttileset-tile-type tile)
            :probability (ttileset-tile-probability tile)
            :properties (ttileset-tile-properties tile)))))
      ttiles)
     (progn (nconc ttiles other-frame-ttiles)
            other-frame-tiles))))

(defun %load-terrains (tterrains tiles)
  (mapcar
   (lambda (tterrain)
     (make-instance
      'tiled-terrain
      :name (tterrain-name tterrain)
      :tile (find (tterrain-tile tterrain) tiles :key #'tile-id)
      :properties (tterrain-properties tterrain)))
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
             (make-array 4 :element-type '(or null tiled-terrain)
                           :initial-contents
                           (flet ((get-terrain (idx)
                                    (and idx (elt terrains idx))))
                             (vector (get-terrain (aref terrains-v 0))
                                     (get-terrain (aref terrains-v 1))
                                     (get-terrain (aref terrains-v 2))
                                     (get-terrain (aref terrains-v 3))))))
       (when tgroup
         (setf (slot-value tile 'object-group)
               (make-instance
                'object-group
                :draw-order (or (tobject-group-draw-order tgroup) :top-down)
                :objects (%load-objects (tobject-group-objects tgroup)))))

       (when (typep tile 'animated-tile)
         (setf (slot-value tile 'frames)
               (mapcar
                (lambda (tframe)
                  (make-instance
                   'tiled-frame
                   :duration (tframe-duration tframe)
                   :tile (find (tframe-tile-id tframe) tiles :key #'tile-id)))
                (ttileset-tile-frames ttile))))))
