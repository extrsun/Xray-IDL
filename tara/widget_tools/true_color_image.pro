;+
;========================================================================
;;;
;;; True_Color_Image Widget: $Id: true_color_image.pro 2139 2005-01-20 21:29:03Z patb $
;;;
;;; Copyright (C) 1998, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION:  
;;; This tool may be used to construct a "true color" image from 2 or 3
;;; coaligned images which represent the axis1, axis2, and axis3.
;;;
;========================================================================
;;; EVENTS GENERATED
;;; None
;;;
;========================================================================
;;; INTERFACE TO CLIENT
;;;
;;; true_color_image, top_base, axis1_data, axis2_data, axis3_data, $
;		 PARENT_WIDGET=parent, GROUP=group, BLOCK=block, $
;		 WIDGET_TITLE=widget_title,$
;		 DESCRIPTIONS=descriptions, RGB=rgb, $
;		 XTITLE=xtitle, YTITLE=ytitle, UNITY_ASPECT=unity_aspect, $
;		 XWCS=xwcs, YWCS=ywcs, $
;		 X0=x0, DELTA_X=delta_x, Y0=y0, DELTA_Y=delta_y
;;;
;;; The (x,y) grid over which the function is sampled is described by the
;;; keywords X0, DELTA_X, Y0, & DELTA_Y.  The coordinate (x0,y0) is the
;;; position of the lower left sample (z_data[0,0]) in the world system.
;;; The "sample interval" or "pixel size" in the world system is
;;; delta_x X delta_y.  Thus the sample z_data[i,j] falls at position
;;;   x = x0 + i * delta_x
;;;   y = y0 + j * delta_y
;;;
;;; The parameters PARENT_WIDGET & GROUP are the usual ones associated 
;;; with widgets.  If true_color_image is a top-level widget, then WIDGET_TITLE
;;; controls the title in the X-window frame.
;;;
;;; The state of this widget is stoaxis1 as an anonymous structure in the 
;;; user value slot of the first child (the normal convention).


;==========================================================================
;;;                     CALLING OPTIONS
;==========================================================================
;-

;==========================================================================
;;; Create the widget.
;==========================================================================
FUNCTION CreateTrueColorImage, PARENT_WIDGET=parent, GROUP=group, BLOCK=block, $
			  WIDGET_TITLE=widget_title, RGB=rgb

;; Call color_manager to switch to DirectColor if available.
color_manager, /X_TRUE, DECOMPOSED=decomposed

;; Default values
if (0 EQ n_elements(group))          then group = 0
if (0 EQ n_elements(widget_title))   then widget_title = 'True_Color_Image'
rgb = keyword_set(rgb)  

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

this_is_top_level = (0 EQ n_elements(parent))
if this_is_top_level then begin
 center  = ScreenCenter()
 xoffset = center(0) - 200
 yoffset = center(1) - 250
 parent = plot_window_topbase(TITLE=widget_title,GROUP_LEADER=group, $
                      XOFFSET=xoffset, YOFFSET=yoffset)
endif

top_base = widget_base( parent, /BASE_ALIGN_CENTER, $
			FRAME=(this_is_top_level EQ 0), $
			EVENT_FUNC='TrueColorImageEventFn', $
			KILL_NOTIFY='TrueColorImageCleanup', $
			/COLUMN, /SPACE, XPAD=0, YPAD=0 )
			 
upper_base = widget_base( top_base, /ALIGN_LEFT, /ROW, $
			   SPACE=8, XPAD=0, YPAD=0 )
			   
 menu = [{ CW_PDMENU_S, flags:1, name:'File' }, $ 
	 { CW_PDMENU_S,     0,        'Print' }, $ 
	 { CW_PDMENU_S,     1,        'Save Entire Image' }, $
	 { CW_PDMENU_S,     0,        'PNG  format' }, $
	 { CW_PDMENU_S,     2,        'JPEG format' }, $
	 { CW_PDMENU_S,     1,        'Save Displayed Image' }, $
	 { CW_PDMENU_S,     0,        'PNG  format' }, $
	 { CW_PDMENU_S,     2,        'JPEG format' }, $
	 { CW_PDMENU_S,     2, this_is_top_level ? 'Exit' : '' }]
 
 file_menu = cw_pdmenu(upper_base, menu, /RETURN_INDEX)
     
 edit_button    = widget_button( upper_base, VALUE='Edit' )

 value_scaling_button = widget_button( upper_base, VALUE='Brightness Scaling' )
 
 scaling_mode = widget_droplist( upper_base, VALUE=['log','linear'] )


 if (rgb) then begin
   gain_scaling_button = widget_button( upper_base, VALUE='Gain Scaling' )
   hue_scaling_button  = 0L
   menu = [{ CW_PDMENU_S, flags:1, name:'Auto' }, $ 
	   { CW_PDMENU_S,     0,        'Brightness Scaling' }, $ 
	   { CW_PDMENU_S,     0,        'Background Subtractions' }, $
	   { CW_PDMENU_S,     2,        'Channel Gains' }]
 endif else begin
   gain_scaling_button = 0L
   hue_scaling_button  = widget_button( upper_base, VALUE='Hue Scaling' )
   menu = [{ CW_PDMENU_S, flags:1, name:'Auto' }, $ 
	   { CW_PDMENU_S,     0,        'Brightness Scaling' }, $ 
	   { CW_PDMENU_S,     2,        'Hue Scaling' }]
 endelse
 
 auto_menu = cw_pdmenu(upper_base, menu, IDS=ids, /RETURN_ID)
 
 if (rgb) then begin
   auto_brightness = ids[1]
   auto_bkg        = ids[2]
   auto_gain       = ids[3]
   auto_hue        = 0L
 endif else begin
   auto_brightness = ids[1]
   auto_bkg        = 0L
   auto_gain       = 0L
   auto_hue        = ids[2]
 endelse

msg_label = widget_label( top_base, /DYNAMIC_RESIZE, VALUE=' ' )
    
plot_window, pw_id, PARENT=top_base
plot_window, pw_id, SET_XMARGIN=[8,5], /FORCE_UNITY_ASPECT


; Setup state structure.
axis = { description:'', data:ptr_new(), image:ptr_new(), bkg:0., gain:1. }
	    
state = { parent:parent, $ 
	;IDs of widgets that generate events or need to be updated. 
	file_menu: file_menu, $
	edit_button:         edit_button, $
	value_scaling_button:value_scaling_button, $
	gain_scaling_button: gain_scaling_button,$
	hue_scaling_button:  hue_scaling_button,$
	
	auto_menu:auto_menu, $
	auto_brightness:auto_brightness, auto_hue:auto_hue, $
	auto_bkg:auto_bkg, auto_gain:auto_gain, $
	
	scaling_mode:scaling_mode, $
	
	msg_label:msg_label, pw_id:pw_id, $
	
	;Image structures
	x0:0.0, y0:0.0, delta_x:1.0, delta_y:1.0, xdim:0L, ydim:0L, $
	axis1: axis, $
	axis2: axis, $
	axis3: axis, $
	
	rgb_t:rgb, blue2red_hue:0, saturation:1.0, $
	invert_color:0, color_bar:1, $
	red_channel:ptr_new(/ALLOC), $
	grn_channel:ptr_new(/ALLOC), $
	blu_channel:ptr_new(/ALLOC), $
	bgrnd_index:ptr_new(/ALLOC), $
	fgrnd_index:ptr_new(/ALLOC), $
	nan_map    :ptr_new(/ALLOC), $
	
	; Scaling info.
	low_hue:  0., high_hue:  0., $
	low_value:0., high_value:0.,  $
	low_norm: 0., high_norm: 0., $
	
	;Other state information.
	bkg_color:'black', $
	legend_style:0, note:'', note_x:0.0, note_y:0.0, msg_id:0L, $
	ps_config:ptr_new(/ALLOC), msg_widget:0L }
	

;; Allocate heap variables in axis structures.
state.axis1.data  = ptr_new(/ALLOC)
state.axis1.image = ptr_new(/ALLOC)
state.axis2.data  = ptr_new(/ALLOC)
state.axis2.image = ptr_new(/ALLOC)
state.axis3.data  = ptr_new(/ALLOC)
state.axis3.image = ptr_new(/ALLOC)


;; Save state structure.
widget_control, top_base, SET_UVALUE=ptr_new(state, /NO_COPY)


;; If this is top-level widget, realize it and register.
if this_is_top_level then begin
  widget_control, parent, /REALIZE

  ; The fancy combination of JUST_REG and NO_BLOCK is necessary to get the
  ; widget to behave when called by either widget applications or normal
  ; programs (which block on tty reads).
  xmanager, 'true_color_image', parent, GROUP_LEADER=group, $
	    JUST_REG=keyword_set(block), NO_BLOCK=(keyword_set(block) EQ 0), $
	    EVENT_HANDLER='PlotWindowTopbaseEventFn'
endif

return, top_base
END

;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO TrueColorImageCleanup, top_base

color_manager, /X_PSEUDO
widget_control, top_base, GET_UVALUE=st

;; Free the heap vars allocated locally.
ptr_free, (*st).axis1.data,  (*st).axis2.data,  (*st).axis3.data, $
          (*st).axis1.image, (*st).axis2.image, (*st).axis3.image,$
          (*st).bgrnd_index, (*st).fgrnd_index, (*st).nan_map, $
          (*st).ps_config, (*st).red_channel, (*st).grn_channel, (*st).blu_channel
ptr_free, st
return
end


;==========================================================================
;;; Routine to display the color image.  
;;; It is assumed that coordsystem_manager, /RESTORE has already been called.
;==========================================================================
PRO TvTrueColorImage, st, xaxis, yaxis, COLORBAR=colorbar, NO_DISPLAY=no_display, $
		      USE_CACHED_VALUES=use_cached_values

axis1 = (*st).axis1
axis2 = (*st).axis2
axis3 = (*st).axis3


log_scaling = (widget_info( (*st).scaling_mode, /DROPLIST_SELECT) EQ 0)

  
if ((*st).rgb_t EQ 0) then begin
  ;-------------------------------------------------------------------------
  ;; HSV MODEL (two components)
  if NOT keyword_set(use_cached_values) then begin
    ;; Scale hue to [0..1] range.
    hue_norm = (*axis1.image - (*st).low_hue) / (((*st).high_hue - (*st).low_hue)>1e-8)

    tara_hsv2rgb, 0.0 > hue_norm < 1.0, (*st).saturation, *axis2.image > 0, (*st).blue2red_hue, $
                  red_data, grn_data, blu_data
  endif

  rgb_scale, red_data, grn_data, blu_data, $
	     LOW_VALUE =(*st).low_value,  LOW_NORM =(*st).low_norm, $
	     HIGH_VALUE=(*st).high_value, HIGH_NORM=(*st).high_norm, $
	     LOG=log_scaling,                INVERT=(*st).invert_color, $
	     *(*st).red_channel, *(*st).grn_channel, *(*st).blu_channel, $
	     GET_ZERO_VALUE=zero_value,    GET_FULL_VALUE=full_value, $
	     GET_LOW_VALUE =new_low_value, GET_HIGH_VALUE=new_high_value, $
	     USE_CACHED_VALUES=keyword_set(use_cached_values)
  
endif else begin
  ;-------------------------------------------------------------------------
  min_hue   = 340
  range_hue = 360

  rgb_scale, *axis1.image, *axis2.image, *axis3.image, $
  	     BACKGROUNDS=[axis1.bkg, axis2.bkg, axis3.bkg], $
  	     GAINS=      [axis1.gain,axis2.gain,axis3.gain], $
	     LOW_VALUE =(*st).low_value,  LOW_NORM =(*st).low_norm, $
	     HIGH_VALUE=(*st).high_value, HIGH_NORM=(*st).high_norm, $
	     LOG=log_scaling,                INVERT=(*st).invert_color, $
	     *(*st).red_channel, *(*st).grn_channel, *(*st).blu_channel, $
	     GET_ZERO_VALUE=zero_value,    GET_FULL_VALUE=full_value, $
	     GET_LOW_VALUE =new_low_value, GET_HIGH_VALUE=new_high_value, $
	     USE_CACHED_VALUES=keyword_set(use_cached_values)
endelse


if keyword_set(use_cached_values) then begin
  print, zero_value, full_value, F='(%"%0.3g : %0.3g")'
  (*st).low_value  = zero_value
  (*st).high_value = full_value
  (*st).low_norm   = 0
  (*st).high_norm  = 1
endif else begin
  ;; Sometimes low_value,high_value were set to NaN elsewhere to get rgb_scale to compute
  ;; full range scaling -- save that returned range.
  if (NOT finite((*st).low_value))  then (*st).low_value  = new_low_value
  if (NOT finite((*st).high_value)) then (*st).high_value = new_high_value
  
  ;; We let the user specify the color of pixels listed in bgrnd_index which includes both 
  ;; (a) pixels that are not finite() and
  ;; (b) regions outside the image arrays we have.
  case (*st).bkg_color of
    'black': background_rgb = [0,0,0]
    'white': background_rgb = [1,1,1]
    'red'  : background_rgb = [1,0,0]
    'blue' : background_rgb = [0,0,1]
    else   : background_rgb = [0,0,0]
  endcase
    
  if (size(*(*st).bgrnd_index,/N_DIM) EQ 1) then begin
      (*(*st).red_channel)[*(*st).bgrnd_index] = background_rgb[0]
      (*(*st).grn_channel)[*(*st).bgrnd_index] = background_rgb[1]
      (*(*st).blu_channel)[*(*st).bgrnd_index] = background_rgb[2]
  endif
endelse

if keyword_set(no_display) then return


;-------------------------------------------------------------------------
;; Build a color bar.
if keyword_set(colorbar) then begin
  ;; Compute position & size of color bar in data units.
  charsize_data = convert_coord(!D.X_CH_SIZE,!D.Y_CH_SIZE,/DEV,/TO_DATA) - $
    	          convert_coord(           0,           0,/DEV,/TO_DATA)
  
  x_bar_pos  = xaxis.range[1] + charsize_data[0]
  y_bar_pos  = yaxis.image_position
  
  right_edge = (convert_coord(1,0,/NORM,/TO_DATA))[0]
  bar_xsize  = (right_edge-x_bar_pos) > charsize_data[0]
  bar_ysize  = yaxis.image_size
  
  ;; And dimensions of bar in data units = pixels.
  n_cols = floor( (convert_coord(bar_xsize,0,/DATA,/TO_DEV) - $
  		   convert_coord(        0,0,/DATA,/TO_DEV))[0] ) > 2
  n_rows = yaxis.num_pixels
  

  ;; Compute right-justified positions (data units) and text of the labels. 
  x_label_pos = [right_edge,right_edge,right_edge] - 0.2*charsize_data[0]
  y_label_pos = [y_bar_pos          -1.1*charsize_data[1], $
  		 y_bar_pos+bar_ysize+0.2*charsize_data[1], $
  		 y_bar_pos          -3.0*charsize_data[1]]
      
  labels = string([zero_value,full_value], F='(%"%0.3g")')

  if ((*st).rgb_t EQ 0) then $
    labels=[labels, string((*st).low_hue, (*st).high_hue, F='(%"Hue %0.3g:%0.3g")')]
  

  ;; Make R,G,B vectors which correspond to a ramping hue.
  ;; Equations derived from http://astronomy.swin.edu.au/~pbourke/colour/hsv/
  hue_norm = findgen(n_cols)/(n_cols-1)

  tara_hsv2rgb, hue_norm, (*st).saturation, 1, (*st).blue2red_hue, $
                red_data, grn_data, blu_data

  ;; Convert to 2D and pass through rgb_scale to handle INVERT.   
  value_ramp = findgen(n_rows) & make_2d, red_data, value_ramp
  value_ramp = findgen(n_rows) & make_2d, grn_data, value_ramp
  value_ramp = findgen(n_rows) & make_2d, blu_data, value_ramp
  
  rgb_scale, red_data * value_ramp, grn_data * value_ramp, blu_data * value_ramp,$
  	     INVERT=(*st).invert_color, red_bchannel, grn_bchannel, blu_bchannel
  
endif ;keyword_set(colorbar) 

  
;-------------------------------------------------------------------------
;; Finally we scale up the channel values to the range of the device.
color_manager, NCOLORS=ncolors, DECOMPOSED=decomposed

; Usually we quantize to NCOLORS (from color_manager) levels, but if color_quan() 
; is going to be called below, need to quantize to 256 levels
num_levels = ((!D.NAME EQ 'PS') OR decomposed) ? ncolors : 256

red_img = byte( (num_levels * *(*st).red_channel) < (num_levels-1) )
grn_img = byte( (num_levels * *(*st).grn_channel) < (num_levels-1) )
blu_img = byte( (num_levels * *(*st).blu_channel) < (num_levels-1) )

if keyword_set(colorbar) then begin
  red_bimg = byte( (num_levels * red_bchannel) < (num_levels-1) )
  grn_bimg = byte( (num_levels * grn_bchannel) < (num_levels-1) )
  blu_bimg = byte( (num_levels * blu_bchannel) < (num_levels-1) )
endif

if (!D.NAME EQ 'PS') then begin
  ;; Note that the PostScript device does not have "channels" -- you must
  ;; use the TRUE keyword instead.
  tv, [[[red_img]], [[grn_img]], [[blu_img]]], TRUE=3, $
  	xaxis.image_position, yaxis.image_position, /DATA, $
	XSIZE=xaxis.image_size, YSIZE=yaxis.image_size

  if keyword_set(colorbar) then $
    tv, [[[red_bimg]], [[grn_bimg]], [[blu_bimg]]], TRUE=3, $
  	x_bar_pos, y_bar_pos, /DATA, XSIZE=bar_xsize, YSIZE=bar_ysize
  
endif else begin
  
  if (decomposed) then begin
    ; We expect the color tables to be simple ramps.
    tv, red_img, xaxis.image_position, yaxis.image_position, CHAN=1, /DATA
    tv, grn_img, xaxis.image_position, yaxis.image_position, CHAN=2, /DATA
    tv, blu_img, xaxis.image_position, yaxis.image_position, CHAN=3, /DATA

    if keyword_set(colorbar) then begin
      tv, red_bimg, x_bar_pos, y_bar_pos, CHAN=1, /DATA
      tv, grn_bimg, x_bar_pos, y_bar_pos, CHAN=2, /DATA
      tv, blu_bimg, x_bar_pos, y_bar_pos, CHAN=3, /DATA
    endif

  endif else begin
    ;; Quantize the color and display.
    print, 'WARNING, color is quantized due to 8 bit graphics system.'
    color_image = color_quan( red_img,  grn_img,  blu_img,  COLORS=ncolors, $
  			      rtable,   gtable,   btable,   GET_TRANS=trans )
    tvlct, rtable, gtable, btable
    tv, color_image, xaxis.image_position, yaxis.image_position, /DATA

    if keyword_set(colorbar) then begin
      color_bar = color_quan( red_bimg, grn_bimg, blu_bimg, COLORS=ncolors, $
  			      rtable,   gtable,   btable,       TRANS=trans )

      tv, color_bar, x_bar_pos,            y_bar_pos,            /DATA
    endif
  endelse
endelse

if keyword_set(colorbar) then begin
  xyouts, x_label_pos, y_label_pos, labels, CHARSIZE=0.75, ALIGN=1
  if log_scaling then $
    xyouts, x_bar_pos+bar_xsize-1.5*charsize_data[0], y_bar_pos+bar_ysize/2.0, $
            'log scaling', ALIGN=0.5, ORIENT=90, CHARSIZE=1.25
endif

return
end



;==========================================================================
;;; Routine to update the widget's appearance
;==========================================================================
PRO RedrawTrueColorImage, st

if (!D.NAME EQ 'X') then color_manager, /X_TRUE
widget_control, /HOURGLASS

axis1 = (*st).axis1
axis2 = (*st).axis2
axis3 = (*st).axis3

;--------------------------------------------------------------------------
;; Assign default axis ranges if necessary.
;--------------------------------------------------------------------------
plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis
			  
if (xaxis.default_flag EQ 1) then begin
  x_low  = (*st).x0 - (*st).delta_x/2.0
  x_high = x_low + (*st).xdim * (*st).delta_x
  plot_window, (*st).pw_id, SET_XRANGE=[x_low,x_high] 
endif

if (yaxis.default_flag EQ 1) then begin
  y_low  = (*st).y0 - (*st).delta_y/2.0
  y_high = y_low + (*st).ydim * (*st).delta_y
  plot_window, (*st).pw_id, SET_YRANGE=[y_low,y_high] 
endif

;-------------------------------------------------------------------------
  ; Draw the plot axes.
;-------------------------------------------------------------------------
plot_window, (*st).pw_id, /SHOW_AXES, /FORCE_LINEAR, XAXIS=xaxis, YAXIS=yaxis
  
;-------------------------------------------------------------------------
; Resample the axis data to fit the plot window chosen by the user.
;-------------------------------------------------------------------------
;  msg = 'Sampling data to match current view ...'
;  widget_control, (*st).msg_label, SET_VALUE=msg

  ; Specify the position in the X/Y coordinate system, pixel size, and 
  ; dimensions of the image we need to "tv" into the axes that have been
  ; already drawn.
  x0_p      = xaxis.image_position 
  y0_p      = yaxis.image_position 
  delta_x_p = (xaxis.image_size/xaxis.num_pixels) 
  delta_y_p = (yaxis.image_size/yaxis.num_pixels)
  xdim_p    = xaxis.num_pixels
  ydim_p    = yaxis.num_pixels
  
  ; Resample the density array to create this image.
  resample_image, (*st).x0, (*st).y0, (*st).delta_x, (*st).delta_y, *(*st).nan_map, $
		  x0_p, y0_p, delta_x_p, delta_y_p, $
		  xdim_p, ydim_p, nan_image, COL=col, ROW=row, $
		  *(*st).bgrnd_index, *(*st).fgrnd_index, /SCREEN_NAN
                    
  *axis1.image = (*axis1.data)[col,row]  
  *axis2.image = (*axis2.data)[col,row]  
  if ((*st).rgb_t) then $
  *axis3.image = (*axis3.data)[col,row]  

  if (delta_x_p GT (*st).delta_x OR delta_y_p GT (*st).delta_y) then begin
      temp=string( max([delta_x_p/(*st).delta_x, delta_y_p/(*st).delta_y]),$
                   f='(G10.4)' )
    
      msg=['Your data bins are too small to produce an accurate ', $
             'display at this magnification.',$
             'The image you see may be misleading.',$
             '', 'Increase your pixel size by a factor of '+strcompress(temp), $
             'OR','increase the size of the plotting window (Title button),',$
             'OR','zoom in (reduce the range of your axes).']
        
      id=(*st).msg_widget
      ; We have to supply XOFFSET to keep TimedMessage from calling 
      ; ScreenCenter() which will die when the device is PS!
      TimedMessage, id, msg, GROUP=(*st).pw_id, XOFFSET=500, TIT='WARNING!'
      (*st).msg_widget=id
  endif else widget_control, (*st).msg_widget, /DESTROY, BAD_ID=bad
      
   
;  msg = 'Scaling images and displaying ...'
;  widget_control, (*st).msg_label, SET_VALUE=msg

            
;-------------------------------------------------------------------------
;; Initialize scaling if necessary.
;-------------------------------------------------------------------------
if ((*st).rgb_t EQ 0) AND ((*st).low_hue GE (*st).high_hue) AND (size(*(*st).fgrnd_index, /N_DIM) EQ 1) then begin
  print, 'Autoscaling hue ...'
  
  fgrnd_pixels = congrid( (*axis1.image)[*(*st).fgrnd_index], 1000 )

  meanclip, fgrnd_pixels, mean, sigma  
  minval = min( fgrnd_pixels, MAX=maxval )
  
  if (maxval EQ minval) then maxval=minval + 1
  
  lowlimit  = (mean - 2*sigma) > minval
  highlimit = (mean + 2*sigma) < maxval
  
  if (lowlimit EQ highlimit) then begin
    lowlimit  = minval
    highlimit = maxval
  endif
  
  (*st).low_hue  = lowlimit
  (*st).high_hue = highlimit
endif

if ((*st).low_norm  GE (*st).high_norm) OR $
   ((*st).low_value GE (*st).high_value) then begin
  ; Set low_value & high_value to NaN to get rgb_scale to compute them.
  print, 'Autoscaling brightness ...'
  (*st).low_norm   = 0
  (*st).high_norm  = 1
  (*st).low_value  = !VALUES.F_NAN
  (*st).high_value = !VALUES.F_NAN
endif

;-------------------------------------------------------------------------
; Display the image
;-------------------------------------------------------------------------
TvTrueColorImage, st, xaxis, yaxis, COLORBAR=(*st).color_bar

;------------------------------------------------------------------------
;; Draw the note.
;------------------------------------------------------------------------
if ((*st).note NE '') then begin
  xyouts, (*st).note_x, (*st).note_y, (*st).note, /DATA
endif


plot_window, (*st).pw_id, /SHOW_MARKERS, XAXIS=xaxis, YAXIS=yaxis

;; Finally, show the dataset stats in the message field.
;widget_control, (*st).msg_label, SET_VALUE=selected_dataset.stats
return
end


;==========================================================================
;;; Event Handler Function
;==========================================================================
FUNCTION TrueColorImageEventFn, Event

widget_control, /HOURGLASS
new_event   = 0
DestroyFlag = 0

;; Get the state structure. 
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st

;; Process the event.
case Event.ID of

;--------------------------------------------------------------------------
; Plot window mouse events.
  (*st).pw_id: $
   begin
   if (Event.middle_button) then begin
     plot_window, (*st).pw_id, DRAW_WIDGET=draw_widget
       
     coordsystem_manager, draw_widget, /RESTORE
     click = convert_coord( event.button_coord, /DATA, /TO_DEVICE )
     rgb_vals = tvrd( click[0], click[1], 1, 1, TRUE=1)

     col = round( (event.button_coord[0] - (*st).x0) / (*st).delta_x )
     row = round( (event.button_coord[1] - (*st).y0) / (*st).delta_y )
     
     if (col GE 0 AND col LT (*st).xdim AND $
         row GE 0 AND row LT (*st).ydim) then begin
       axis1 = (*st).axis1
       axis2 = (*st).axis2
       axis3 = (*st).axis3
       
       if ((*st).rgb_t) then begin
         fmt='(%"Signal (display) at [%0.5g,%0.5g]: red=%0.3g (%d), grn=%0.3g (%d), blu=%0.3g (%d)")'
         msg = string( (*st).x0 + col * (*st).delta_x, $
       		       (*st).y0 + row * (*st).delta_y, $
		       axis1.gain * ((*axis1.data)[col,row] - axis1.bkg), $
		       rgb_vals[0], $
		       axis2.gain * ((*axis2.data)[col,row] - axis2.bkg), $
		       rgb_vals[1], $
		       axis3.gain * ((*axis3.data)[col,row] - axis3.bkg), $
		       rgb_vals[2], F=fmt )

       endif else begin
         fmt='("Hue, Brightness at [",G11.5,", ",G11.5,"] = (",G11.5,", ",G11.5,")")'
         msg = string( (*st).x0 + col * (*st).delta_x, $
       		       (*st).y0 + row * (*st).delta_y, $
		       (*axis1.data)[col,row], $
		       (*axis2.data)[col,row], F=fmt )
       endelse
     endif else msg = ''

     widget_control, (*st).msg_label, SET_VALUE=msg
   endif ;(Event.middle_button)
   
   if (event.redraw) then RedrawTrueColorImage, st
   end



;--------------------------------------------------------------------------
; File menu
  (*st).file_menu: $
   begin
   switch (Event.value) of
    1: $ ; PRINT
     begin
     plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis
     asp_r = yaxis.num_pixels / float(xaxis.num_pixels)
     PsDevice, top_base, filename, success, CONFIG=*(*st).ps_config, $
		XMARGIN=xaxis.margin, YMARGIN=yaxis.margin, ASPECT_RATIO=asp_r, /COLOR

     if (success EQ 1 ) then begin
       color_manager, /PS_TRUE
       RedrawTrueColorImage, st
       device, /CLOSE
       color_manager, /X_TRUE
  
       if (Filename EQ '') then begin
         print_file
         widget_control, (*st).msg_label, SET_VALUE='Printed plot'
       endif else begin
         widget_control, (*st).msg_label, $
         		 SET_VALUE='Wrote Postscript file ' + filename
       endelse
     endif 
     break
     end
     
    2: ; SAVE ENTIRE IMAGE
    3:
    4: $
     begin
     *(*st).axis1.image = (*(*st).axis1.data)  
     *(*st).axis2.image = (*(*st).axis2.data)  
     if ((*st).rgb_t) then $
     *(*st).axis3.image = (*(*st).axis3.data)  
     
     ; Find the background pixels (just as RedrawTrueColorImage would do).
     *(*st).bgrnd_index = where( finite(*(*st).nan_map) EQ 0 )

     TvTrueColorImage, st, /NO_DISPLAY
     ; And fall through to code below
     end
     
    5: ; SAVE DISPLAYED IMAGE
    6:
    7: $
     begin
     format = (Event.value EQ 3 OR Event.value EQ 6) ? 'PNG' : 'JPEG'
     filter = (Event.value EQ 3 OR Event.value EQ 6) ? ['*.png','*.*'] : ['*.jpg; *.jpeg','*.*']
     pathname = dialog_pickfile( GROUP=top_base, FILTER=filter, $
     				 FILE='true_color.'+strlowcase(format), $
				 TITLE='Save Image ('+format+' format)' )
     widget_control, /HOURGLASS
     
     if (pathname NE '') then begin
       ; Make sure we can write to this file.
       openw, Unit, pathname, /GET_LUN, ERROR=Error
       if (Error NE 0) then begin
         message = 'ERROR opening file '+ pathname
       endif else begin
         free_lun, Unit
         
         ; Pack the three components as write_png desires, and save as a 24-bit
         ; PNG/JPEG image.
	 xdim = (size(*(*st).red_channel, /DIM))[0]
	 ydim = (size(*(*st).red_channel, /DIM))[1]
	 img = bytarr(3,xdim,ydim)
	 
	 num_levels = 256	 
	 img[0,*,*] = floor( num_levels * *(*st).red_channel ) < (num_levels-1)
	 img[1,*,*] = floor( num_levels * *(*st).grn_channel ) < (num_levels-1)
	 img[2,*,*] = floor( num_levels * *(*st).blu_channel ) < (num_levels-1)
	 
	 if (format EQ 'PGN') $
	   then write_png,  pathname, img $
	   else write_jpeg, pathname, img, TRUE=1
	 
	 if (Event.value LT 5) then RedrawTrueColorImage, st
	 print, 'Wrote file '+pathname
       endelse ; (Error EQ 0)
     endif ; (pathname NE '')
     break
     end

    else: DestroyFlag = 1 ; EXIT button
  endswitch
  end
   

;--------------------------------------------------------------------------
; Edit button: $
  (*st).edit_button: $
   begin
     null = '2, LABEL'

     b0 = '1, BASE,, COLUMN, FRAME'
     t0 = '0, LABEL, BRIGHTNESS SCALING, LEFT'
     f0 = '0, BUTTON, use values below|autoscale, EXCLUSIVE,'+$
          'SET_VALUE=0, ROW, TAG=value_autoscale'
;     t1 = '0, LABEL, low    high'
     form = [b0,t0,f0]
     
     b1 = '1, BASE,, ROW'
     l1 = '0, LABEL, .     data range:'
     f1 = '0, FLOAT,' + string((*st).low_value) + $
	   ', TAG=low_value,  WIDTH=8'
     f2 = '2, FLOAT,' + string((*st).high_value) + $
	   ', TAG=high_value, WIDTH=8'
     l2 = '0, LABEL, .   is scaled to, LEFT'
     form = [form, b1,l1,f1,f2,l2]

     b1 = '1, BASE,, ROW'
     l1 = '0, LABEL, brightness range:'
     f1 = '0, FLOAT,' + string((*st).low_norm) + $
	   ', TAG=low_norm,  WIDTH=8'
     f2 = '0, FLOAT,' + string((*st).high_norm) + $
	   ', TAG=high_norm, WIDTH=8'
     l2 = '2, LABEL, black=0; full=1'
     f3 = '2, DROPLIST, Logarithmic | Linear, SET_VALUE=' + $
    	   string(widget_info( (*st).scaling_mode, /DROPLIST_SELECT)) + ',TAG=log_scaling'
     form = [form, b1,l1,f1,f2,l2,f3]

   if (*st).rgb_t then begin
     b0 = '1, BASE,, COLUMN, FRAME'
     t0 = '0, LABEL, COMPONENT BACKGROUND AND GAIN  :'
     t1 = '0, LABEL, background  gain'
     form = [form, b0,t0,t1]
     
     b1 = '1, BASE,, ROW'
     l1 = '0, LABEL, RED  :'
     f1 = '0, FLOAT,' + string((*st).axis1.bkg) + $
	   ', TAG=bkg1,  WIDTH=8'
     f2 = '2, FLOAT,' + string((*st).axis1.gain) + $
	   ', TAG=gain1, WIDTH=8'
     form = [form, b1,l1,f1,f2]
    
     b1 = '1, BASE,, ROW'
     l1 = '0, LABEL, GREEN:'
     f1 = '0, FLOAT,' + string((*st).axis2.bkg) + $
	   ', TAG=bkg2,  WIDTH=8'
     f2 = '2, FLOAT,' + string((*st).axis2.gain) + $
	   ', TAG=gain2, WIDTH=8'
     form = [form, b1,l1,f1,f2]
 
     b1 = '1, BASE,, ROW'
     l1 = '0, LABEL, BLUE :'
     f1 = '0, FLOAT,' + string((*st).axis3.bkg) + $
	   ', TAG=bkg3,  WIDTH=8'
     f2 = '2, FLOAT,' + string((*st).axis3.gain) + $
	   ', TAG=gain3, WIDTH=8'
     form = [form, b1,l1,f1,f2]
    endif else begin
     b0 = '1, BASE,, COLUMN, FRAME'
     t0 = '0, LABEL, HUE SCALING, LEFT'
     f0 = '0, BUTTON, use values below|autoscale, EXCLUSIVE,'+$
          'SET_VALUE=0, ROW, TAG=hue_autoscale'
     form = [form, b0,t0,f0]
     
     b1 = '1, BASE,, ROW'
     l1 = '0, LABEL, .     data range:'
     f1 = '0, FLOAT,' + string((*st).low_hue) + $
	   ', TAG=low_hue,  WIDTH=8'
     f2 = '2, FLOAT,' + string((*st).high_hue) + $
	   ', TAG=high_hue, WIDTH=8'
     l2 = '0, LABEL, .    is scaled to, LEFT'
     f3 = '0, DROPLIST, red->yellow->green->blue->purple|purple->blue->green->yellow->red, SET_VALUE='+ $
    	   string((*st).blue2red_hue) + ',TAG=blue2red_hue'
     f4 = '2, FLOAT,' + string((*st).saturation) + $
 	  ', TAG=saturation, LABEL_LEFT=Saturation [0..1],WIDTH=4'     
     form = [form, b1,l1,f1,f2,l2,f3,f4]
    
    endelse


    f0 = '0, DROPLIST, Luminous Model|Subtractive Model, SET_VALUE=' + $
    	   string((*st).invert_color) + ',TAG=invert_color'
    f1 = '0, DROPLIST, Omit|Show, SET_VALUE=' + $
   	   string((*st).color_bar) + ',LABEL_LEFT=Colorbar,TAG=color_bar'

    color_list = ['black','white','red','blue']
    f2 = '0, DROPLIST, black|white|red|blue, SET_VALUE=' + $
   	   string(where((*st).bkg_color EQ color_list)) + ',LABEL_LEFT=Background Color,TAG=bkg_color'
  
   r = cw_form( [form,f0,f1,f2,'2,BUTTON,OK,QUIT'], /COLUMN, TITLE='Scaling Parameters' )
   
   if r.value_autoscale then begin
     (*st).low_value  = 0
     (*st).high_value = 0
   endif else begin
     (*st).low_value  = r.low_value
     (*st).high_value = r.high_value
   endelse
   
   (*st).low_norm    = r.low_norm
   (*st).high_norm   = r.high_norm   
   widget_control, (*st).scaling_mode, SET_DROPLIST_SELECT=r.log_scaling
   
   if ((*st).rgb_t) then begin
     (*st).axis1.bkg    = r.bkg1
     (*st).axis1.gain   = r.gain1 
  
     (*st).axis2.bkg    = r.bkg2
     (*st).axis2.gain   = r.gain2 
  
     (*st).axis3.bkg    = r.bkg3
     (*st).axis3.gain   = r.gain3 
   endif else begin
     if r.hue_autoscale then begin
       (*st).low_hue  = 0
       (*st).high_hue = 0
     endif else begin
       (*st).low_hue  = r.low_hue
       (*st).high_hue = r.high_hue
     endelse
     (*st).blue2red_hue = r.blue2red_hue
     (*st).saturation   = r.saturation
   endelse

   
   (*st).invert_color  = r.invert_color
   (*st).color_bar     = r.color_bar
   (*st).bkg_color     = color_list[r.bkg_color]
   
   RedrawTrueColorImage, st
   end


;--------------------------------------------------------------------------
; Value Scaling button: $
  (*st).value_scaling_button: $
   begin
   ;; Make sure the correct X window is active,
   plot_window, (*st).pw_id, DRAW_WIDGET=draw_widget, XAXIS=xaxis, YAXIS=yaxis
       
   coordsystem_manager, draw_widget, /RESTORE
   device, CURSOR_STANDARD=66
   
   ;; We need to ask color_manager to put us in X_TRUE mode since the user 
   ;; may have switched back to X_PSEUDO mode.
   color_manager, /X_TRUE
   
   ;; Give some instructions
   prompt = ['Move mouse to select data range over which Brightness is modulated',$
    'Horizontal <> : slide range to lower/higher data values', $
    'Vertical   ^v : decrease/increase width of scaled range',$
    'Click when finished']
   TimedMessage, msg_id, prompt, GROUP=top_base
   
   ;; Position the pointer at the center.
   tvcrs, 0.5, 0.5, /NORM
   start_center = ((*st).high_norm + (*st).low_norm) / 2
   center2high  =  (*st).high_norm - start_center
   center2low   =  (*st).low_norm  - start_center
   
   ;; Handle the motion events.
   print, 'stretch data range:'
   use_cached_values=0
   event = widget_event( draw_widget )
   while (event.type EQ 2) do begin
     event = widget_event( draw_widget )
     pt    = convert_coord( event.x, event.y, /DEVICE, /TO_NORM )
     
     center = start_center + (center2high-center2low) * (1-2*pt(0))
     scale  = 2.^(2*pt(1) - 1)
     (*st).low_norm  = center + scale*center2low
     (*st).high_norm = center + scale*center2high 
 
     TvTrueColorImage, st, xaxis, yaxis, USE_CACHED_VALUES=use_cached_values
     use_cached_values=1
;print, (*st).low_norm, center, (*st).high_norm 
   endwhile
   
   TvTrueColorImage, st, xaxis, yaxis, COLORBAR=(*st).color_bar
;   RedrawTrueColorImage, st
   device, /CURSOR_ORIGINAL
   
   widget_control, draw_widget, /CLEAR_EVENTS
   widget_control, msg_id, /DESTROY, BAD_ID=bad_id
   end

   
;--------------------------------------------------------------------------
; Hue Scaling button: $
  (*st).hue_scaling_button: $
   begin
   ;; Make sure the correct X window is active,
   plot_window, (*st).pw_id, DRAW_WIDGET=draw_widget, XAXIS=xaxis, YAXIS=yaxis
       
   coordsystem_manager, draw_widget, /RESTORE
   device, CURSOR_STANDARD=66
   
   ;; We need to ask color_manager to put us in X_TRUE mode since the user 
   ;; may have switched back to X_PSEUDO mode.
   color_manager, /X_TRUE
   
   ;; Give some instructions
   prompt = ['Move mouse to select data range over which Hue is modulated',$
    'Horizontal <> : slide range to lower/higher data values', $
    'Vertical   ^v : decrease/increase width of scaled range',$
    'Click when finished']
   TimedMessage, msg_id, prompt, GROUP=top_base
   
   ;; Position the pointer at the center.
   tvcrs, 0.5, 0.5, /NORM
   start_center = ((*st).high_hue + (*st).low_hue) / 2
   center2high  =  (*st).high_hue - start_center
   center2low   =  (*st).low_hue  - start_center
      
   ;; Handle the motion events.
   event = widget_event( draw_widget )
   while (event.type EQ 2) do begin
     event = widget_event( draw_widget )
     pt    = convert_coord( event.x, event.y, /DEVICE, /TO_NORM )
     
     center = start_center + (center2high-center2low) * (2*pt(0)-1)
     scale  = 2.^(2*pt(1) - 1)
     (*st).low_hue  = center + scale*center2low
     (*st).high_hue = center + scale*center2high 
 
     TvTrueColorImage, st, xaxis, yaxis
print, (*st).low_hue, center, (*st).high_hue 
   endwhile
   
   TvTrueColorImage, st, xaxis, yaxis, COLORBAR=(*st).color_bar
;   RedrawTrueColorImage, st
   device, /CURSOR_ORIGINAL
   
   widget_control, draw_widget, /CLEAR_EVENTS
   widget_control, msg_id, /DESTROY, BAD_ID=bad_id
   end


;--------------------------------------------------------------------------
; Gain Scaling button: $
  (*st).gain_scaling_button: $
   begin
   ;; Make sure the correct X window is active,
   plot_window, (*st).pw_id, DRAW_WIDGET=draw_widget, XAXIS=xaxis, YAXIS=yaxis
       
   coordsystem_manager, draw_widget, /RESTORE
   device, CURSOR_STANDARD=66
   
   ;; We need to ask color_manager to put us in X_TRUE mode since the user 
   ;; may have switched back to X_PSEUDO mode.
   color_manager, /X_TRUE
   
   ;; Give some instructions
   prompt = ['Move mouse to change relative strength of color channels',$
    'Horizontal <> : Red  gain,  [1/2 : 2]', $
    'Vertical   ^v : Blue gain, [1/2 : 2]',$
    'Click when finished']
   TimedMessage, msg_id, prompt, GROUP=top_base
   
   ;; Position the pointer at the center.
   tvcrs, 0.5, 0.5, /NORM

   start_redgain = (*st).axis1.gain 
   start_blugain = (*st).axis3.gain 
      
   ;; Handle the motion events.
   event = widget_event( draw_widget )
   while (event.type EQ 2) do begin
     event = widget_event( draw_widget )
     pt    = convert_coord( event.x, event.y, /DEVICE, /TO_NORM )
     
     (*st).axis1.gain = start_redgain * 2.^(2*pt(0) - 1)
     (*st).axis3.gain = start_blugain * 2.^(2*pt(1) - 1)
 
     TvTrueColorImage, st, xaxis, yaxis
print, (*st).axis1.gain, (*st).axis3.gain 
   endwhile
   
   TvTrueColorImage, st, xaxis, yaxis, COLORBAR=(*st).color_bar
;   RedrawTrueColorImage, st
   device, /CURSOR_ORIGINAL
   
   widget_control, draw_widget, /CLEAR_EVENTS
   widget_control, msg_id, /DESTROY, BAD_ID=bad_id
   end


;--------------------------------------------------------------------------
; Auto menu: $
  (*st).auto_menu: $
  begin
  case event.value of
; Auto Brightness button: $
  (*st).auto_brightness: $
   begin
   (*st).low_value  = 0
   (*st).high_value = 0
   end

; Auto Hue button: $
  (*st).auto_hue: $
   begin
   (*st).low_hue  = 0
   (*st).high_hue = 0
   end

; Auto Background button: 
  (*st).auto_bkg: $
   begin
   sky, *(*st).axis1.data, bkg, /NAN
   (*st).axis1.bkg = bkg

   sky, *(*st).axis2.data, bkg, /NAN
   (*st).axis2.bkg = bkg

   sky, *(*st).axis3.data, bkg, /NAN
   (*st).axis3.bkg = bkg
   print, 'Backgrounds set to ', (*st).axis1.bkg, (*st).axis2.bkg, (*st).axis3.bkg
   end

; Auto Gain button: $
  (*st).auto_gain: $
   begin
   red_signal = (*(*st).axis1.data - (*st).axis1.bkg) > 0.
   grn_signal = (*(*st).axis2.data - (*st).axis2.bkg) > 0.
   blu_signal = (*(*st).axis3.data - (*st).axis3.bkg) > 0.
   
   red_quartile = median( red_signal[where(red_signal GE median(red_signal,/EVEN))], /EVEN )
   grn_quartile = median( grn_signal[where(grn_signal GE median(grn_signal,/EVEN))], /EVEN )
   blu_quartile = median( blu_signal[where(blu_signal GE median(blu_signal,/EVEN))], /EVEN )

help, red_quartile, grn_quartile, blu_quartile
   if (red_quartile LE 0) OR (grn_quartile LE 0) OR (blu_quartile LE 0) then begin
     print, 'WARNING: non-positive quartiles computed; gains not changed.'
   endif else begin
     (*st).axis1.gain = grn_quartile / red_quartile
     (*st).axis2.gain = 1
     (*st).axis3.gain = grn_quartile / blu_quartile
   
     print, 'Gains set to ', (*st).axis1.gain, (*st).axis2.gain, (*st).axis3.gain
   endelse
   end
  endcase
  RedrawTrueColorImage, st
  end   
   
;--------------------------------------------------------------------------
; Scaling mode: $
  (*st).scaling_mode: $
  begin
  RedrawTrueColorImage, st
  end
    
  
  else: print, 'unknown event in true_color_image'
endcase


if DestroyFlag then widget_control, (*st).parent, /DESTROY

return, new_event
end



;==========================================================================
;;; Event Handler Procedure
;==========================================================================
PRO TrueColorImageEvent, Event
 
event = TrueColorImageEventFn( Event )
return
end


;==========================================================================
;;; MAIN "true_color_image" ROUTINE
;==========================================================================

PRO true_color_image, top_base, axis1_data, axis2_data, axis3_data, $
		DESCRIPTIONS=descriptions,$
		XTITLE=xtitle, YTITLE=ytitle, UNITY_ASPECT=unity_aspect,$
		XWCS=xwcs, YWCS=ywcs, $
		X0=x0, DELTA_X=delta_x, Y0=y0, DELTA_Y=delta_y, _EXTRA=extra

		 
;; If the widget ID of an existing true_color_image was not passed, then create 
;; the widget.
create_flag = 1
if (n_elements(top_base) EQ 1) then begin
  if (widget_info( top_base, /VALID_ID )) then create_flag = 0
endif

if (create_flag) then top_base = CreateTrueColorImage( _EXTRA=extra )

;;---------------------------------------------------------------------------
;; Get the state structure.
widget_control, top_base, GET_UVALUE=st

;;---------------------------------------------------------------------------
;; Handle title keywords by passing on to plot_window.
if (0 NE n_elements(xwcs))     then plot_window, (*st).pw_id, XWCS=xwcs
if (0 NE n_elements(ywcs))     then plot_window, (*st).pw_id, YWCS=ywcs
if (0 NE n_elements(xtitle))   then plot_window, (*st).pw_id, XTITLE=xtitle
if (0 NE n_elements(ytitle))   then plot_window, (*st).pw_id, YTITLE=ytitle
if keyword_set(unity_aspect)   then plot_window, (*st).pw_id, /FORCE_UNITY_ASP

if (N_ELEMENTS(axis1_data) EQ 0) then return

(*st).xdim = (size( axis1_data, /DIMEN ))[0]
(*st).ydim = (size( axis1_data, /DIMEN ))[1]

*(*st).axis1.data = axis1_data
*(*st).axis2.data = axis2_data
*(*st).nan_map    = axis1_data + axis2_data

if (n_elements(descriptions) GE 2) then begin
  (*st).axis1.description = descriptions[0]
  (*st).axis2.description = descriptions[1]
endif

if ((*st).rgb_t) then begin
  *(*st).axis3.data = axis3_data
  
  *(*st).nan_map    = *(*st).nan_map + axis3_data

  if (n_elements(descriptions) EQ 3) then $
    (*st).axis3.description = descriptions[2]
endif else if (n_elements(axis3_data) NE 0) then begin
  print, 'ERROR!  Third image provided without /RGB'
  widget_control, (*st).parent, /DESTROY
  return
endif


if (0 NE n_elements(x0))      then (*st).x0      = x0
if (0 NE n_elements(y0))      then (*st).y0      = y0
if (0 NE n_elements(delta_x)) then (*st).delta_x = delta_x
if (0 NE n_elements(delta_y)) then (*st).delta_y = delta_y
;;---------------------------------------------------------------------------
if (widget_info(top_base, /REALIZED)) then RedrawTrueColorImage, st

return
END



PRO test_true_color_image

N = 301
make_2d, indgen(N), indgen(N), axis1_data, axis2_data

even_ind = 2*indgen(N/2)
axis2_data[0,  even_ind]=0
axis2_data[N-1,even_ind]=0
axis2_data[even_ind,0]  =0
axis2_data[even_ind,N-1]=0

true_color_image, id1, axis1_data, axis2_data, XTIT='Hue', YTIT='Brightness', WIDGET_TITLE='HSV test image'

print, 'Change scaling to linear!'

dim=256L
red=lindgen(dim,dim) & grn=transpose(red)
x=lindgen(dim) & y=x & make_2d, x, y
blu=sqrt(x^2 + y^2)  & blu=dim^2 * blu/max(blu)

true_color_image, id2, red, grn, blu, /RGB, XTIT='Green', YTIT='Red', WIDGET_TITLE='RGB test image'

rfn='red.fits'
gfn='green.fits'
bfn='blue.fits'
if (total(file_test([rfn,gfn,bfn])) EQ 3) then begin
  red = rebin(float(readfits(rfn)),256,256)
  grn = rebin(float(readfits(gfn)),256,256)
  blu = rebin(float(readfits(bfn)),256,256)
 true_color_image, id3, red, grn, blu, /RGB, WIDGET_TITLE='RGB test image'
endif

rfn='red.img'
gfn='green.img'
bfn='blue.img'
if (total(file_test([rfn,gfn,bfn])) EQ 3) then begin
  red = rebin(float(readfits(rfn)),256,256)
  grn = rebin(float(readfits(gfn)),256,256)
  blu = rebin(float(readfits(bfn)),256,256)
  true_color_image, id4, red, grn, blu, /RGB, WIDGET_TITLE='RGB test image'
endif

return
end
