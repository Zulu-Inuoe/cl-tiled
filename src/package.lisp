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

(cl:in-package #:cl-user)

(defpackage #:cl-tiled
  (:use
   #:alexandria
   #:cl
   #:cl-tiled.impl
   #:cl-tiled.impl.xml
   #:cl-tiled.impl.json)
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