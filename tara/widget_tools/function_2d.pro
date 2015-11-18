;+
;========================================================================
;;;
;;; Function_2D Widget: $Id: function_2d.pro 1453 2001-10-09 17:07:38Z patb $
;;;
;;; Copyright (C) 1998, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION:  
;;; This tool may be used to analyze a function of two variables, z=f(x,y)
;;; expressed as samples of z taken over a regular grid of (x,y) points.
;;; These samples must be stored as a 2-D array, i.e. an "image".
;;;
;;; This program is a simple wrapper which transforms a 2-D image array
;;; into a weighted 2-D dataset and passes that to the tool dataset_2d.pro.
;;;
;========================================================================
;;; EVENTS GENERATED
;;; An event is forwarded to the client whenever the user specifies a
;;; region of interest.
;;;
;========================================================================
;;; INTERFACE TO CLIENT
;;;
;;; function_2d, top_base, z_data, EXPAND_PIXELS=expand_pixels, $
;;;		 X0=x0, DELTA_X=delta_x, Y0=y0, DELTA_Y=delta_y
;;;
;;; Unrecognized keywords (such as XWCS, YWCS, XTITLE, YTITLE) are passed  
;;; on to dataset_2d via the _EXTRA mechanism.
;;;
;;; If EXPAND_PIXELS is omitted the 2-D image "z_data" is converted into a 
;;; list of weighted bivariate data points, one for each pixel.
;;; If EXPAND_PIXELS is set and the 2-D image contains positive integers then
;;; it is converted into a list of unweighted bivariate data points whose 
;;; length equals the sum of the image pixels.

;;; The optional keywords X0, DELTA_X, Y0, DELTA_Y can be used to change
;;; the way the X&Y axes are labelled.  
;;; If X0, etc. are not supplied then the (x,y) values assigned to each 
;;; pixel are simply the 0-based integer-valued array indexes, and the
;;; bin sizes passed to dataset_2d are 1.0.
;;;
;;; If however you want to label pixels according to some linear coordinate
;;; system that's aligned with the image axes, then you can describe the
;;; coordinate system by X0, DELTA_X, Y0, DELTA_Y.
;;; The (x,y) values assigned to each pixel are then:
;;;   x = x0 + i * delta_x
;;;   y = y0 + j * delta_y
;;; where i & j are the 0-based pixel indexes, and the bin sizes passed to
;;; dataset_2d are delta_x & delta_y.
;;;
;;; Any wcs structures (see wcs_object.pro) supplied in keywords XWCS & YWCS
;;; are simply passed onto dataset_2d.  NORMALLY FOR AN ASTRONOMICAL IMAGE
;;; READ FROM A FITS FILE you would omit X0, DELTA_X, Y0, DELTA_Y and supply
;;; wcs structures (made with wcs_object.pro) in XWCS & YWCS.  
;;; The image would be displayed with axes marking the pixel indexes and
;;; the RA/DEC would be shown as the mouse is moved across the image.
;;;
;;; The state of this widget is stored as an anonymous structure in the 
;;; user value slot of the first child (the normal convention).


;==========================================================================
;;;                     CALLING OPTIONS

;;; From a non-widget program or the command line, display two images (one 
;;; at a time).
;;; Widget events will be processed when the command line returns.
;;; ** function_2d, id, image1, DATASET_NAME='First'
;;; ** function_2d, id, image2, DATASET_NAME='Second'

;;; From a non-widget program display an image but block further
;;; program execution until the function_2d widget is destroyed.
;;; ** function_2d, id, image, /BLOCK, 
;;; ** xmanager

;;; From a widget program, create a top-level function_2d.
;;; ** function_2d, id, image, GROUP=creator_widget_id

;;; Create a function_2d as a child of another widget.
;;; ** function_2d, id, PARENT_WIDGET=base

;;; Delete a dataset from an existing function_2d.
;;; ** function_2d, id, /DELETE, DATASET_NAME=name

;;; Obtain a pointer to a byte array that shows which data points in the 
;;; specified dataset are retained by the user's filter.  If no filter
;;; is defined, then the pointer points to an undefined variable.
;;; ** function_2d, id, DATASET_NAME=name, ROI_MASK=mask
;==========================================================================
;-

PRO function_2d, top_base, z_data, WIDGET_TITLE=widget_title, _EXTRA=extra, $
		 EXPAND_PIXELS=expand_pixels, $
		 X0=x0, DELTA_X=delta_x, Y0=y0, DELTA_Y=delta_y
		 
if (0 EQ n_elements(widget_title))   then widget_title = 'Function_2D'

if (n_elements(z_data) EQ 0) then begin
  dataset_2d, top_base, WIDGET_TITLE=widget_title, _EXTRA=extra
  return
endif

if (NOT keyword_set(x0))      then x0 = 0
if (NOT keyword_set(delta_x)) then delta_x = 1
if (NOT keyword_set(y0))      then y0 = 0
if (NOT keyword_set(delta_y)) then delta_y = 1

;; Compute the world coordinates of the function samples (image pixels).
xdim = (size( z_data, /DIMEN ))[0]
ydim = (size( z_data, /DIMEN ))[1]

expand_pixels_flag = 0
if keyword_set(expand_pixels) then begin
  expand_pixels_flag = 1
  if (size( z_data, /TYPE) GT 3) then begin
    print, 'function_2d: Cannot expand pixels for a non-integer image.'
    expand_pixels_flag = 0
  endif

  if (min(z_data) LT 0) then begin
    print, 'function_2d: Cannot expand pixels for an image with negative values.'
    expand_pixels_flag = 0
  endif
endif

make_2d, (x0 + delta_x * lindgen(xdim)), (y0 + delta_y * lindgen(ydim)), $
	 x_position, y_position
	 
if expand_pixels_flag then begin
  num_pix = long(total(z_data, /DOUBLE))
  print, num_pix, F='("Expanding image into ",I0," bivariate data points.")'
  x_data = fltarr(num_pix)
  y_data = fltarr(num_pix)

  evt_index = 0L
  for pix_index = 0L, xdim*ydim-1 do begin
    pixel_value = z_data[pix_index]

    if (pixel_value GT 0) then begin
      x_data[evt_index] = replicate(x_position[pix_index], pixel_value)
      y_data[evt_index] = replicate(y_position[pix_index], pixel_value)
      evt_index = evt_index + pixel_value
    endif
  endfor

  dataset_2d, top_base, WIDGET_TITLE=widget_title, $
	    x_data, y_data, $
	    XBIN=delta_x, YBIN=delta_y, /UNITY_ASPECT, $
	    XEDGE=x0-delta_x/2., YEDGE=y0-delta_y/2., _EXTRA=extra
endif else begin
  dataset_2d, top_base, WIDGET_TITLE=widget_title, $
	    x_position, y_position, WEIGHT=z_data, $
	    XBIN=delta_x, YBIN=delta_y, /UNITY_ASPECT, $
	    XEDGE=x0-delta_x/2., YEDGE=y0-delta_y/2., _EXTRA=extra
endelse
return
end
