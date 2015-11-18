;+
;========================================================================
;;;
;;; Plot Window Widget: $Id: plot_window.pro 4374 2012-10-30 20:16:30Z psb6 $
;;;
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION:  
;;; This is a draw_widget plus a set of controls for specifying the axis
;;; ranges to use in a plot.  The client (parent widget) is responsible 
;;; for actually drawing the graphics -- this widget just provides axis
;;; range services and two markers that the user can move using the left
;;; and right mouse buttons.  
;;;
;========================================================================
;;; EVENTS GENERATED
;;; An event is forwarded to the client whenever the plot needs to be
;;; redrawn or when the middle mouse button is pressed.
;;; Events include the following structure tags:
;;;   REDRAW:        0 or 1
;;;   MIDDLE_BUTTON: 0 or 1 
;;;   BUTTON_COORD:  coordinates of the button press
;;;
;========================================================================
;;; CALLING OPTIONS
;;; plot_window, id, PARENT=parent, Z_ENABLE=z_enable, $
;;;		 TITLE=title, SUBTITLE=subtitle, 
;;;		 ZTITLE=ztitle, XTITLE=xtitle, YTITLE=ytitle, 
;;;		 XWCS=xwcs, YWCS=ywcs, $
;;;   SHOW_DATE=show_date, $
;;;		 SET_XMARGIN=set_xmargin, $
;;;		 SET_YMARGIN=set_ymargin, $
;;;		 SET_XRANGE=set_xrange, SET_YRANGE=set_yrange,
;;;		 SET_ZRANGE=set_zrange,
;;;		 SET_BIG_MARKER=set_big_marker, 
;;;		 SET_SMALL_MARKER=set_small_marker
;;;		 SHOW_AXES=show_axes, SHOW_MARKERS=show_markers,
;;;		 FORCE_LINEAR=force_linear, 
;;;		 FORCE_UNITY_ASPECT=force_unity_aspect,
;;;
;;;		 DRAW_WIDGET=draw_widget, 
;;;		 BIG_MARKER=big_marker, SMALL_MARKER=small_marker,
;;;		 XAXIS=xaxis, YAXIS=yaxis, ZAXIS=zaxis,
;;;		 GET_TITLE=get_title, GET_SUBTITLE=get_subtitle, 
;;;		 UNITY_ASPECT=unity_aspect
;;;
;;;		 GET_MOUSE=get_mouse, PROMPT=prompt
;;;
;;; TITLE, SUBTITLE, XTITLE, YTITLE, ZTITLE: Titles to use on plot.
;;; SET_XRANGE, SET_YRANGE, SET_ZRANGE: Two-element vectors.
;;; FORCE_LINEAR: If set, linear axes are required.
;;; FORCE_UNITY_ASPECT: If set, the 1-1 aspect button is pushed.

;;; DRAW_WIDGET:  widget ID of the draw_widget
;;; BIG_MARKER, SMALL_MARKER: coordinates of the markers
;;; GET_TITLE:    the current title string (which the user can edit) 
;;; GET_SUBTITLE: the current subtitle string (which the user can edit)
;;; UNITY_ASPECT: flag, set if 1-1 aspect ratio is desired
;;;
;;; XAXIS, YAXIS: Structures with the following tags.  Unless noted otherwise,
;;; all positions and sizes in DATA coordinates, not device coordinates.
;;; 
;;;  default_flag:   set if axis range should be computed by client
;;;  range_desired:  the axis range requested by user
;;;  range:          actual axis endpoints, 2-vector
;;;  log_flag:       set if log axis is desired
;;;  image_position: see example below
;;;  image_size:     see example below
;;;  num_pixels:     see example below 
;;;  title:          string
;;;
;;; The following example shows how a client would display an image in the 
;;; plot window.
;;;
;;;   plot_window, id,  
;;;		 TITLE=title, SUBTITLE=subtitle, XTITLE=xtitle, YTITLE=ytitle, 
;;;		 XAXIS=x, YAXIS=y
;;;
;;;   n_columns = x.num_pixels
;;;   n_rows    = y.num_pixels
;;;   im        = fltarr( n_columns, n_rows )
;;;
;;;   indexes   = lindgen( n_columns, n_rows )
;;;   row       = indexes / n_columns
;;;   column    = indexes - (row * n_columns)
;;;   pixel_x   = x.image_position + column * (x.image_size/x.num_pixels)
;;;   pixel_y   = y.image_position +    row * (y.image_size/y.num_pixels)
;;;
;;;   im[i] = f( pixel_x[i], pixel_y[i] )
;;;
;;;   tv, im, x.image_position, y.image_position, /DATA, $
;;;	  XSIZE=x.image_size, YSIZE=y.image_size
;;;
;;;   
;========================================================================

;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO PlotWindowCleanup, top_base

widget_control, top_base, GET_UVALUE=st
ptr_free, st
return
end


;==========================================================================
;;; Mouse Click Function
;;; Returns data coordinates of click and button PRESS bitmap as 3-vector.
;==========================================================================
FUNCTION PlotWindowMouseFn, st, position, prompt 

coordsystem_manager, (*st).draw_widget, /RESTORE
widget_control, (*st).draw_widget, DRAW_MOTION_EVENTS=0
widget_control, (*st).draw_widget, /CLEAR_EVENTS

;; We expect two events -- button down and button up.
text = ''
repeat begin
  if (strlen(text) LE 1) then begin
    text = prompt
    TimedMessage, msg, text, POS=position, /NO_QUIT
    pause_cnt = 60
  endif else begin
    if (pause_cnt LT 0) then text = strmid( text, 1, 1000 )
    TimedMessage, msg, text, POS=position, /NO_QUIT
    wait, 0.05
    pause_cnt = pause_cnt - 1
  endelse

  event = widget_event( (*st).draw_widget, /NOWAIT )
endrep until (event.id EQ (*st).draw_widget)

event = widget_event( (*st).draw_widget )
widget_control, msg, /DESTROY, BAD_ID=bad_id

;; If desired, mark the window.
if (strmid(prompt, strlen(prompt)-1) NE ' ') then $
  mark_window, event.x, event.y, 2

widget_control, (*st).draw_widget, DRAW_MOTION_EVENTS=1

return, [(convert_coord( event.x, event.y, /DEVICE, /TO_DATA, /DOUBLE ))[0:1], $
	 event.release]
end


;==========================================================================
;;; Event Handler Function
;==========================================================================
FUNCTION PlotWindowEventFn, Event

;; Get the state structure. 
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st

xrange = (*st).xaxis.range
yrange = (*st).yaxis.range

height = yrange[1] - yrange[0]
width  = xrange[1] - xrange[0]
ycen   = total( yrange ) / 2
xcen   = total( xrange ) / 2

ydemag=0 & ymag=0 & xdemag=0 & xmag=0

new_event = { ID:top_base, TOP:event.top, HANDLER:0L, $
	      REDRAW:1, MIDDLE_BUTTON:0, BUTTON_COORD:[0D,0D]}
			 
;; Process the event.
case Event.ID of

;--------------------------------------------------------------------------
; The base widget merely forwards events.
  top_base: 
  
;--------------------------------------------------------------------------
; Plot window mouse events.
  (*st).draw_widget: $
   begin
   coordsystem_manager, (*st).draw_widget, /RESTORE
   location = convert_coord( event.X, event.Y, /DEV, /TO_DATA, /DOUBLE )
   
   if (Event.type EQ 2) then begin
     wcs_object, [(*st).xaxis.wcs,(*st).yaxis.wcs], PIXEL=location[0:1], $
     		 FORMATTED_VALUE=txt
     widget_control, (*st).wcs_value, SET_VALUE=txt
     return, 0
   endif else if (Event.type EQ 0) then begin
     color_manager, RED=red, BLACK=black
     new_event.redraw = 0
     
     case event.press of
       1: begin
	  mark_window, (*st).big_marker[0], (*st).big_marker[1], 5, /DATA, COLOR=black
          (*st).big_marker   = location[0:1]
	  mark_window, (*st).big_marker[0], (*st).big_marker[1], 5, /DATA, COLOR=red
          end
       4: begin
	  mark_window, (*st).small_marker[0], (*st).small_marker[1], 3, /DATA, COLOR=black
          (*st).small_marker = location[0:1]
	  mark_window, (*st).small_marker[0], (*st).small_marker[1], 3, /DATA, COLOR=red
          end
       2: begin
          new_event.middle_button = 1
          new_event.button_coord  = location[0:1]
          end
       else:
     endcase   
   endif else new_event = 0
   end

;--------------------------------------------------------------------------
; 
  (*st).pan_upp:   begin
		   (*st).yaxis.range_desired = yrange + .75 * height
		   (*st).yaxis.default_flag  = 0
		   end
  (*st).pan_up :   begin
		   (*st).yaxis.range_desired = yrange + .10 * height
		   (*st).yaxis.default_flag  = 0
		   end
  (*st).pan_down : begin
		   (*st).yaxis.range_desired = yrange - .10 * height
		   (*st).yaxis.default_flag  = 0
		   end
  (*st).pan_downn: begin
		   (*st).yaxis.range_desired = yrange - .75 * height
		   (*st).yaxis.default_flag  = 0
		   end

  (*st).pan_rightt: begin
		    (*st).xaxis.range_desired = xrange + .75 * width
		    (*st).xaxis.default_flag  = 0
		    end
  (*st).pan_right : begin
		    (*st).xaxis.range_desired = xrange + .10 * width
		    (*st).xaxis.default_flag  = 0
		    end
  (*st).pan_left :  begin
		    (*st).xaxis.range_desired = xrange - .10 * width
		    (*st).xaxis.default_flag  = 0
		    end
  (*st).pan_leftt:  begin
		    (*st).xaxis.range_desired = xrange - .75 * width
		    (*st).xaxis.default_flag  = 0
		    end

;--------------------------------------------------------------------------
; 
  (*st).ydemag: $
   begin
   ydemag = 1
   if (*st).unity_aspect then xdemag = 1
   end
   
  (*st).ymag  : $
   begin
   ymag = 1
   if (*st).unity_aspect then xmag = 1
   end
   
  (*st).xdemag: $
   begin
   xdemag = 1
   if (*st).unity_aspect then ydemag = 1
   end
   
  (*st).xmag  : $
   begin
   xmag = 1
   if (*st).unity_aspect then ymag = 1
   end

;--------------------------------------------------------------------------
; 
  (*st).save: $
   begin
   coordsystem_manager, (*st).draw_widget, /RESTORE
 
   pathname = dialog_pickfile( GROUP=top_base, FILE='window.png',$
  			      TITLE='Save Window (PNG format)' )

   if (pathname NE '') then begin
     ;; The window must not be covered for tvrd to work properly.
     wshow, ICONIC=0
     
     device, GET_DECOMPOSED=decomposed
     
     if decomposed then begin
       r=tvrd(CHANNEL=1)
       g=tvrd(CHANNEL=2)
       b=tvrd(CHANNEL=3)
       
       xdim = (size(r, /DIM))[0]
       ydim = (size(r, /DIM))[1]
       img = bytarr(3,xdim,ydim)
       img[0,*,*] = r
       img[1,*,*] = g
       img[2,*,*] = b
       write_png, pathname, img
     endif else begin
       img=tvrd()
       tvlct, r,g,b, /GET
       write_png, pathname, img, r, g, b, /VERB
     endelse
   endif
   new_event = 0
   end

;--------------------------------------------------------------------------
; 
  (*st).edit: $
   begin
   coordsystem_manager, (*st).draw_widget, /RESTORE
   b0 = '1, BASE,, ROW, FRAME'

   l0 = '0, LABEL, Window Dimensions:, CENTER'
   
   f1 = '0, INTEGER,' + string((*st).draw_size[0]) + $
	 ', TAG=xdim, LABEL_LEFT=X, WIDTH=12'

   f2 = '2, INTEGER,' + string((*st).draw_size[1]) + $
	 ', TAG=ydim, LABEL_LEFT=Y, WIDTH=12'

   form = [b0,l0,f1,f2]
   

   f1 = '0, TEXT,' + escape_commas( (*st).title ) + $
	', TAG=title, LABEL_LEFT=Title, WIDTH=49'

   f2 = '0, TEXT,' + escape_commas( (*st).subtitle ) + $
	', TAG=subtitle, LABEL_LEFT=Sub-title, WIDTH=45'

   f3 = '0, TEXT,' + escape_commas( (*st).xaxis.title ) + $
	', TAG=xtitle, LABEL_LEFT=X-title, WIDTH=47'

   f4 = '0, TEXT,' + escape_commas( (*st).yaxis.title ) + $
	', TAG=ytitle, LABEL_LEFT=Y-title, WIDTH=47'

   f5 = '0, DROPLIST, Omit Date|Show Date, SET_VALUE=' + $
	 string((*st).show_date) + ', TAG=show_date'

    ; We have to translate the field "color", which is a string, into
    ; an index into the list of all available colors.
    color_manager, COLOR_NAMES=color_names
    color_index = 0 > (where((*st).color EQ color_names))[0]
    color_list  = string(color_names, F='(99(A,:,"|"))' )
 
   f6 = '0, BUTTON,' + color_list + ',EXCLUSIVE, SET_VALUE=' +$
 	 string(color_index) + ', TAG=color, LABEL_TOP=Axis & Title Color'

   form = [form,f1,f2,f3,f4,f5,f6]
   
   
   ;; Z AXIS
   if ((*st).z_enable) then begin
     b0 = '1, BASE,, COLUMN, FRAME'
  
     l0 = '0, LABEL, Z AXIS (for surface plots), CENTER'
     
     f0 = '0, TEXT,' + escape_commas( (*st).zaxis.title ) + $
  	  ', TAG=ztitle, LABEL_LEFT=Z-title, WIDTH=47'
  
     b1 = '1, BASE,, ROW'
  
     f1 = '0, FLOAT,' + string((*st).zaxis.range_desired[0]) + $
  	  ', TAG=zlow, LABEL_LEFT=Min Z, WIDTH=12'
  
     f2 = '2, FLOAT,' + string((*st).zaxis.range_desired[1]) + $
  	  ', TAG=zhigh, LABEL_LEFT=Max Z, WIDTH=12'
  
     f3 = '0, DROPLIST, Linear Axis | Logarithmic Axis, SET_VALUE=' + $
      	  string((*st).zaxis.log_flag) + ',TAG=log_flag'
  
  
     f4 = '2, DROPLIST, Fixed Axis Range| Automatic Axis Range, SET_VALUE=' + $
      	  string((*st).zaxis.default_flag) + ',TAG=default_flag'
  
     form = [form,b0,l0,f0,b1,f1,f2,f3,f4]
   endif
   
   ;; Font Size
   f1 = '0, FLOAT,' + string(!P.CHARSIZE, F='(%"%0.1f")') + $
	 ', TAG=charsize, LABEL_LEFT=Character Size, WIDTH=4'
   
   form = [form,f1]
   
   
   ;; MARKERS
   wcs_object, [(*st).xaxis.wcs,(*st).yaxis.wcs], IS_NULL=is_null, $
   		PIXEL=(*st).big_marker, FORMATTED_VALUE=txt
   b0 = '1, BASE,, ROW, FRAME'

   l0 = '0, LABEL, Big Marker  :, CENTER'
   
   f1 = '0, FLOAT,' + string((*st).big_marker[0]) + $
	 ', TAG=xbig, LABEL_LEFT=X, WIDTH=12'

   f2 = '2, FLOAT,' + string((*st).big_marker[1]) + $
	 ', TAG=ybig, LABEL_LEFT=Y, WIDTH=12'

   if (total(is_null) EQ 2) then begin
     form = [form,b0,l0,f1,f2]
   endif else begin
     l1 = '0, LABEL,' + txt
     form = [form,b0,l0,l1,f1,f2]
   endelse
    

   wcs_object, [(*st).xaxis.wcs,(*st).yaxis.wcs], IS_NULL=is_null, $
   		PIXEL=(*st).small_marker, FORMATTED_VALUE=txt
   b0 = '1, BASE,, ROW, FRAME'

   l0 = '0, LABEL, Small Marker:, CENTER'
   
   f1 = '0, FLOAT,' + string((*st).small_marker[0]) + $
	 ', TAG=xsmall, LABEL_LEFT=X, WIDTH=12'

   f2 = '2, FLOAT,' + string((*st).small_marker[1]) + $
	 ', TAG=ysmall, LABEL_LEFT=Y, WIDTH=12'

   if (total(is_null) EQ 2) then begin
     form = [form,b0,l0,f1,f2]
   endif else begin
     l1 = '0, LABEL,' + txt
     form = [form,b0,l0,l1,f1,f2]
   endelse
  
   f7 = '2,BUTTON,OK,QUIT,TAG=ok'

   ;; RUN THE FORM
   result = cw_form( [form,f7], /COLUMN, TITLE='Titles & Markers' )

   draw_size = [result.xdim, result.ydim] > [200,200] < [3000,3000]
   if (total( draw_size NE (*st).draw_size ) GT 0) then begin
     (*st).draw_size = draw_size
     widget_control, (*st).draw_widget, XSIZE=draw_size[0], YSIZE=draw_size[1]
   endif
   
   if ((*st).title       NE result.title)    then (*st).title_free       = 0
   (*st).title       = result.title
   
   if ((*st).subtitle    NE result.subtitle) then (*st).subtitle_free    = 0
   (*st).subtitle    = result.subtitle
   
   if ((*st).xaxis.title NE result.xtitle)   then (*st).xaxis.title_free = 0
   (*st).xaxis.title = result.xtitle
   
   if ((*st).yaxis.title NE result.ytitle)   then (*st).yaxis.title_free = 0
   (*st).yaxis.title = result.ytitle
   
   (*st).show_date   = result.show_date

   ; We have to translate "result.color", which is an index into
   ; the corresponding color name (string).
   (*st).color = color_names[result.color]
   
   if ((*st).z_enable) then begin
     zrange = [result.zlow<result.zhigh, result.zlow>result.zhigh]
     if ( max(zrange NE (*st).zaxis.range_desired) ) then begin
       (*st).zaxis.range_desired = zrange
       (*st).zaxis.default_flag  = 0B
     endif

     (*st).zaxis.log_flag     = result.log_flag
     (*st).zaxis.default_flag = result.default_flag

     if ((*st).zaxis.title NE result.ztitle) then (*st).zaxis.title_free = 0
     (*st).zaxis.title        = result.ztitle
   endif
   
   !P.CHARSIZE = result.charsize
   coordsystem_manager, (*st).draw_widget, /SAVE
   
   (*st).big_marker  = [result.xbig,   result.ybig]
   (*st).small_marker= [result.xsmall, result.ysmall]
   end
     
;--------------------------------------------------------------------------
  (*st).yedit: $
   begin
    b1 = '1, BASE,, COLUMN, FRAME'

    f1 = '0, FLOAT,' + string((*st).yaxis.range_desired[1]) + $
	 ', TAG=yhigh, LABEL_LEFT=Max Y, WIDTH=12'

    f2 = '2, FLOAT,' + string((*st).yaxis.range_desired[0]) + $
	 ', TAG=ylow, LABEL_LEFT=Min Y, WIDTH=12'

    f3 = '0, DROPLIST, Linear Axis | Logarithmic Axis, SET_VALUE=' + $
    	string((*st).yaxis.log_flag) + ', TAG=log_flag'

    f4 = '0, BUTTON,normal|reverse,EXCLUSIVE, SET_VALUE=' +$
      string((*st).yaxis.reverse_flag) + ', TAG=reverse_flag, LABEL_LEFT=Axis Orientation'

    form = [b1,f1,f2,f3,f4]
    
    b0 = '1, BASE,, COLUMN, FRAME, CENTER'

    l0 = '0, LABEL, Margins:, CENTER'
   
    f1 = '0, INTEGER,' + string((*st).yaxis.margin[1]) + $
	 ', TAG=top, LABEL_LEFT=Top..., WIDTH=4'

    f2 = '2, INTEGER,' + string((*st).yaxis.margin[0]) + $
	 ', TAG=bottom, LABEL_LEFT=Bottom, WIDTH=4'

    form = [form,b0,l0,f1,f2]

    f4 = '2,BUTTON,OK,QUIT,TAG=ok'

    r = cw_form( [form,f4], /COLUMN, TITLE='Y-axis Range' )

    yrange = [r.ylow<r.yhigh, r.ylow>r.yhigh]
    if ( max(yrange NE (*st).yaxis.range_desired) ) then begin
      (*st).yaxis.range_desired = yrange
      (*st).yaxis.default_flag  = 0B
      (*st).unity_aspect        = 0B
      ;;(*st).xaxis.range_desired = xrange
    endif

    (*st).yaxis.reverse_flag = r.reverse_flag
    (*st).yaxis.log_flag     = r.log_flag
    (*st).yaxis.margin       = [r.bottom, r.top]
   end
   
;--------------------------------------------------------------------------
  (*st).xedit: $
   begin
    b1 = '1, BASE,, ROW, FRAME'

    f1 = '0, FLOAT,' + string((*st).xaxis.range_desired[0]) + $
	 ', TAG=xlow, LABEL_LEFT=Min X, WIDTH=12'

    f2 = '2, FLOAT,' + string((*st).xaxis.range_desired[1]) + $
	 ', TAG=xhigh, LABEL_LEFT=Max X, WIDTH=12'

    f3 = '0, DROPLIST, Linear Axis | Logarithmic Axis, SET_VALUE=' + $
    	string((*st).xaxis.log_flag) + ',TAG=log_flag'

    f4 = '0, BUTTON,normal|reverse,EXCLUSIVE, SET_VALUE=' +$
      string((*st).xaxis.reverse_flag) + ', TAG=reverse_flag, LABEL_LEFT=Axis Orientation'

    form = [b1,f1,f2,f3,f4]

    b0 = '1, BASE,, ROW, FRAME'

    l0 = '0, LABEL, Margins:, CENTER'
   
    f1 = '0, INTEGER,' + string((*st).xaxis.margin[0]) + $
	 ', TAG=left, LABEL_LEFT=Left, WIDTH=4'

    f2 = '2, INTEGER,' + string((*st).xaxis.margin[1]) + $
	 ', TAG=right, LABEL_LEFT=Right, WIDTH=4'

    form = [form,b0,l0,f1,f2]

    f4 = '2,BUTTON,OK,QUIT,TAG=ok'

    r = cw_form( [form,f4], /COLUMN, TITLE='X-axis Range' )

    xrange = [r.xlow<r.xhigh, r.xlow>r.xhigh]
    if ( max(xrange NE (*st).xaxis.range_desired) ) then begin
      (*st).xaxis.range_desired = xrange
      (*st).xaxis.default_flag  = 0B
      (*st).unity_aspect        = 0B
      ;;(*st).yaxis.range_desired = yrange
    endif

    (*st).xaxis.reverse_flag = r.reverse_flag
    (*st).xaxis.log_flag     = r.log_flag
    (*st).xaxis.margin       = [r.left, r.right]
   end


;--------------------------------------------------------------------------
; 
  (*st).ydefault: $
   begin
   (*st).yaxis.default_flag = event.select
   if (event.select EQ 0) then new_event = 0
   if (*st).unity_aspect then (*st).xaxis.default_flag = event.select
   end
   
  (*st).xdefault: $
   begin
   (*st).xaxis.default_flag = event.select
   if (event.select EQ 0) then new_event = 0
   if (*st).unity_aspect then (*st).yaxis.default_flag = event.select
   end


;--------------------------------------------------------------------------
; 
  (*st).ymouse: $
   begin
   if (*st).unity_aspect then begin
     p_a = PlotWindowMouseFn( st, (*st).aspect_button, $
     			      'Select one corner of a region with the mouse.')
     p_b = PlotWindowMouseFn( st, (*st).aspect_button, $
     			      'Select the other corner.' )
     (*st).xaxis.range_desired = [p_a[0]<p_b[0], p_a[0]>p_b[0]]
     (*st).yaxis.range_desired = [p_a[1]<p_b[1], p_a[1]>p_b[1]]
     (*st).xaxis.default_flag  = 0B
     (*st).yaxis.default_flag  = 0B
   endif else begin
     p_a = PlotWindowMouseFn( st, (*st).ydefault, $
     			      'Select one Y-axis limit with the mouse.' )
     p_b = PlotWindowMouseFn( st, (*st).ydefault, $
     			      'Select the other Y-axis limit.' )
     (*st).yaxis.range_desired = [p_a[1]<p_b[1], p_a[1]>p_b[1]]
     (*st).yaxis.default_flag  = 0B
   endelse
   end

;--------------------------------------------------------------------------
; 
  (*st).xmouse: $
   begin
   if (*st).unity_aspect then begin
     p_a = PlotWindowMouseFn( st, (*st).aspect_button, $
     			      'Select one corner of a region with the mouse.')
     p_b = PlotWindowMouseFn( st, (*st).aspect_button, $
     			      'Select the other corner.' )
     (*st).xaxis.range_desired = [p_a[0]<p_b[0], p_a[0]>p_b[0]]
     (*st).yaxis.range_desired = [p_a[1]<p_b[1], p_a[1]>p_b[1]]
     (*st).xaxis.default_flag  = 0B
     (*st).yaxis.default_flag  = 0B
   endif else begin
     p_a = PlotWindowMouseFn( st, (*st).xdefault, $
     			      'Select one X-axis limit with the mouse.' )
     p_b = PlotWindowMouseFn( st, (*st).xdefault, $
     			      'Select the other X-axis limit.' )
     (*st).xaxis.range_desired = [p_a[0]<p_b[0], p_a[0]>p_b[0]]
     (*st).xaxis.default_flag  = 0B
   endelse
   end

;--------------------------------------------------------------------------
; 
  (*st).ycenter: $
   begin
   p_a = PlotWindowMouseFn( st, (*st).aspect_button, $
     			      'Select desired center with the mouse.')
   if (*st).unity_aspect then begin
     (*st).xaxis.range_desired = [p_a[0]-width/2, p_a[0]+width/2]
     (*st).yaxis.range_desired = [p_a[1]-height/2, p_a[1]+height/2]
     (*st).xaxis.default_flag  = 0B
     (*st).yaxis.default_flag  = 0B
   endif else begin
     (*st).yaxis.range_desired = [p_a[1]-height/2, p_a[1]+height/2]
     (*st).yaxis.default_flag  = 0B
   endelse
   end

;--------------------------------------------------------------------------
; 
  (*st).xcenter: $
   begin
   p_a = PlotWindowMouseFn( st, (*st).aspect_button, $
     			      'Select desired center with the mouse.')
   if (*st).unity_aspect then begin
     (*st).xaxis.range_desired = [p_a[0]-width/2, p_a[0]+width/2]
     (*st).yaxis.range_desired = [p_a[1]-height/2, p_a[1]+height/2]
     (*st).xaxis.default_flag  = 0B
     (*st).yaxis.default_flag  = 0B
   endif else begin
     (*st).xaxis.range_desired = [p_a[0]-width/2, p_a[0]+width/2]
     (*st).xaxis.default_flag  = 0B
   endelse
   end


;--------------------------------------------------------------------------
; 
  (*st).aspect_button: $
   begin
   (*st).unity_aspect = event.select
   if (event.select EQ 0) then begin
     (*st).xaxis.range_desired = xrange
     (*st).yaxis.range_desired = yrange
     new_event = 0
   endif
   end

  else: print, 'unknown event in plot_window'
endcase

if ydemag then begin
  (*st).yaxis.range_desired = ycen + [-height,height]
  (*st).yaxis.default_flag  = 0
endif
if ymag   then begin
  (*st).yaxis.range_desired = ycen + [-height,height]/4
  (*st).yaxis.default_flag  = 0
endif
if xdemag then begin
  (*st).xaxis.range_desired = xcen + [-width,width]
  (*st).xaxis.default_flag  = 0
endif
if xmag   then begin
  (*st).xaxis.range_desired = xcen + [-width,width]/4
  (*st).xaxis.default_flag  = 0
endif


widget_control, (*st).xdefault, SET_VALUE=(*st).xaxis.default_flag
widget_control, (*st).ydefault, SET_VALUE=(*st).yaxis.default_flag
widget_control, (*st).aspect_button, SET_VALUE=(*st).unity_aspect


return, new_event
end


;==========================================================================
;;; MAIN ROUTINE
;==========================================================================
PRO plot_window, top_base, PARENT=parent, Z_ENABLE=z_enable, $
		 TITLE=title, SUBTITLE=subtitle,  $
		 ZTITLE=ztitle, XTITLE=xtitle, YTITLE=ytitle, $
		 XWCS=xwcs, YWCS=ywcs, $
		 CHANGE_DRAW_SIZE=change_draw_size, $
		 GENERATE_EVENT=generate_event, $
     SHOW_DATE=show_date, $
		 SET_XMARGIN=set_xmargin, $
		 SET_YMARGIN=set_ymargin, $
		 SET_XRANGE=set_xrange, SET_YRANGE=set_yrange,$
		 SET_XDEFAULT_FLAG=set_xdefault_flag, SET_YDEFAULT_FLAG=set_ydefault_flag, $
		 SET_ZRANGE=set_zrange, ADD_MARGIN=add_margin,$
		 SET_BIG_MARKER=set_big_marker, $
		 SET_SMALL_MARKER=set_small_marker, $
		 SHOW_AXES=show_axes, SHOW_MARKERS=show_markers, $
		 FORCE_LINEAR=force_linear, $
		 FORCE_UNITY_ASPECT=force_unity_aspect, $
		 DRAW_WIDGET=draw_widget, $
		 BIG_MARKER=big_marker, SMALL_MARKER=small_marker, $
		 XAXIS=xaxis, YAXIS=yaxis, ZAXIS=zaxis, $
		 GET_TITLE=get_title, GET_SUBTITLE=get_subtitle, $ 
		 UNITY_ASPECT=unity_aspect, $
		 GET_MOUSE=get_mouse, PROMPT=prompt
		 
;; Create widget if necessary.
if keyword_set(parent) then begin
  top_base = widget_base( parent, UNAME='plot_window', $
			  EVENT_FUNC='PlotWindowEventFn', $
			  KILL_NOTIFY='PlotWindowCleanup', $
			  /COLUMN, /SPACE, /XPAD, /YPAD )

  up_base = widget_base(top_base, /ROW, /BASE_ALIGN_BOTTOM,/SPACE, /XPAD, /YPAD )
   left_base = widget_base(up_base, /COLUMN, SPACE=10, /XPAD, /YPAD )
    buttons = widget_base(left_base, /COLUMN, SPACE=6, /XPAD, /YPAD )
     yedit     = widget_button( buttons, VALUE='Y-edit' )
     pan_upp   = widget_button( buttons, VALUE='^^' )
     pan_up    = widget_button( buttons, VALUE='^' )
     ycenter   = widget_button( buttons, VALUE='Center' )
     ymag      = widget_button( buttons, VALUE='Zoom+' )
     ydefault  = cw_bgroup( buttons, ['Auto'], /NONEXCLUSIVE, SET_VALUE=1 )
     ydemag    = widget_button( buttons, VALUE='Zoom-' )
     ymouse    = widget_button( buttons, VALUE='Range' )
     pan_down  = widget_button( buttons, VALUE='v' )
     pan_downn = widget_button( buttons, VALUE='vv' )
    
    buttons = widget_base(left_base, /COLUMN, /SPACE, /XPAD, /YPAD )
      save = widget_button( buttons, VALUE='Save' )
      edit = widget_button( buttons, VALUE='Titles' )
      
      aspect_button = cw_bgroup( buttons, ['1-1'], /NONEXCLUSIVE, $
    				/SPACE, /XPAD, /YPAD )
  
   draw_size = [580,410]
   draw_widget = widget_draw(up_base, /BUTTON_EVENTS, /MOTION_EVENTS, $
			      XSIZE=draw_size[0], YSIZE=draw_size[1] )
			      
  down_base = widget_base(top_base, /ROW, /SPACE, /XPAD, /YPAD, /ALIGN_RIGHT )
    wcs_name  = 0L  ;widget_label( down_base, VALUE=' ', /DYNAMIC_RESIZE )
    wcs_value = widget_label( down_base, /DYNAMIC_RESIZE, VALUE=' ' )

    buttons = widget_base(down_base, /ROW, SPACE=4, XPAD=1, /YPAD )
     pan_leftt = widget_button( buttons, VALUE='<<' )
     pan_left  = widget_button( buttons, VALUE='<' )
     xcenter   = widget_button( buttons, VALUE='Center' )
     xdemag    = widget_button( buttons, VALUE='Zoom-' )
     xdefault  = cw_bgroup( buttons, ['Auto'], /NONEXCLUSIVE, SET_VALUE=1 )
     xmag      = widget_button( buttons, VALUE='Zoom+' )
     xmouse    = widget_button( buttons, VALUE='Range' )
     pan_right  = widget_button( buttons, VALUE='>' )
     pan_rightt = widget_button( buttons, VALUE='>>' )
     xedit     = widget_button( buttons, VALUE='X-edit' )
  
  wcs_object, wcs, /INIT
  axis = { default_flag:1, log_flag:0, reverse_flag:0, range:[0.0,0.0], add_margin:0, $
  	   image_position:0.0, image_size:0.0, num_pixels:0, $
  	   range_desired:[0.0,1.0], margin:[0,0], title:'', title_free:1B, $
  	   wcs:wcs }
  	   
  state = { xaxis:axis, yaxis:axis, zaxis:axis, $
  	    unity_aspect:0, z_enable:0, $
  	    show_date:1, big_marker:[1E6,1E6], small_marker:[1E6,1E6],$
  	    title:'', title_free:1B, subtitle:'', subtitle_free:1B, $
  	    draw_size:draw_size, color:'white', $

  	    
  	    pan_upp:pan_upp, pan_up:pan_up, yedit:yedit, ydemag:ydemag, $
  	    ydefault:ydefault, ymag:ymag, ymouse:ymouse, $
  	    pan_down:pan_down, pan_downn:pan_downn, ycenter:ycenter, $
  	    aspect_button:aspect_button, draw_widget:draw_widget, $
  	    save:save, edit:edit, wcs_name:wcs_name, wcs_value:wcs_value, $
  	    pan_leftt:pan_leftt, pan_left:pan_left, xedit:xedit, xdemag:xdemag,$
  	    xdefault:xdefault, xmag:xmag, xmouse:xmouse, $
  	    pan_right:pan_right, pan_rightt:pan_rightt, xcenter:xcenter } 
  		  
  state.xaxis.margin = [8,1]
  state.yaxis.margin = [5,2]
  
  state.xaxis.title  = 'X'
  state.yaxis.title  = 'Y'

  widget_control, top_base, SET_UVALUE=ptr_new( state, /NO_COPY )
  return
endif


;; Extract axis structures from state structure.
widget_control, top_base, GET_UVALUE=st
xaxis = (*st).xaxis
yaxis = (*st).yaxis
zaxis = (*st).zaxis

;; Handle input keywords.
if (n_elements(show_date) EQ 1) then (*st).show_date = show_date

if ((*st).title_free       AND (0 NE n_elements(title)))    then $
    (*st).title       = title
  
if ((*st).subtitle_free    AND (0 NE n_elements(subtitle))) then $
    (*st).subtitle    = subtitle
  
if ((*st).xaxis.title_free AND (0 NE n_elements(xtitle)))   then $
          xaxis.title = xtitle
          
if ((*st).yaxis.title_free AND (0 NE n_elements(ytitle)))   then $
          yaxis.title = ytitle
          
if ((*st).zaxis.title_free AND (0 NE n_elements(ztitle)))   then $
          zaxis.title = ztitle

if keyword_set(xwcs)         then xaxis.wcs           = xwcs
if keyword_set(ywcs)         then yaxis.wcs           = ywcs
if keyword_set(set_xmargin)  then xaxis.margin        = set_xmargin
if keyword_set(set_ymargin)  then yaxis.margin        = set_ymargin

if keyword_set(force_linear) then begin
  xaxis.log_flag = 0
  yaxis.log_flag = 0
endif

if keyword_set(set_xrange)   then begin
  xaxis.range_desired = set_xrange 
  xaxis.add_margin    = keyword_set(add_margin)
endif

if (n_elements(set_xdefault_flag) NE 0) then begin
  xaxis.default_flag = set_xdefault_flag
  widget_control, (*st).xdefault, SET_VALUE=set_xdefault_flag
endif

if keyword_set(set_yrange)   then begin
  yaxis.range_desired = set_yrange 
  yaxis.add_margin    = keyword_set(add_margin)
endif

if (n_elements(set_ydefault_flag) NE 0) then begin
  yaxis.default_flag = set_ydefault_flag
  widget_control, (*st).ydefault, SET_VALUE=set_ydefault_flag
endif

if keyword_set(set_zrange)   then begin
  zaxis.range_desired = set_zrange 
  zaxis.add_margin    = keyword_set(add_margin)
endif

if keyword_set(set_big_marker)   then (*st).big_marker  =set_big_marker
if keyword_set(set_small_marker) then (*st).small_marker=set_small_marker
if (n_elements(z_enable) EQ 1)   then (*st).z_enable    = z_enable

if keyword_set(force_unity_aspect) then begin
  widget_control, (*st).aspect_button, SET_VALUE=1
  (*st).unity_aspect = 1
endif

if keyword_set(change_draw_size) then begin
  (*st).draw_size = ((*st).draw_size + change_draw_size) > 20
  widget_control, (*st).draw_widget, $
  		  XSIZE=(*st).draw_size[0], YSIZE=(*st).draw_size[1]  
endif

if keyword_set(generate_event) then begin
  event={ID:top_base, TOP:top_base, HANDLER:top_base}
  widget_control, top_base, SEND_EVENT=event
endif

if keyword_set( show_axes ) then begin
  if (!D.NAME EQ 'X') then begin
    font = -1   ; "vector drawn" font
    coordsystem_manager, (*st).draw_widget, /RESTORE
  endif else begin
    font =  0   ; "hardware" font 
  endelse
   
  ;; Compute axis ranges to request from PLOT routine.
  xrange = xaxis.range_desired
  yrange = yaxis.range_desired
  
  if (xrange[0] GE xrange[1]) then xrange[1]=xrange[0]+1
  if (yrange[0] GE yrange[1]) then yrange[1]=yrange[0]+1
  
  if ((*st).unity_aspect AND NOT (xaxis.log_flag OR yaxis.log_flag)) then begin
    ; Estimate the plot region size in device units.
    xlen_est = !D.X_SIZE - !D.X_CH_SIZE * total( (*st).xaxis.margin )
    ylen_est = !D.Y_SIZE - !D.Y_CH_SIZE * total( (*st).yaxis.margin )
  
    ; Enlarge the axis ranges to center desired region and have 1-1 aspect.
    pixel_size = max( [(xrange[1] - xrange[0]) / xlen_est, $
    		     (yrange[1] - yrange[0]) / ylen_est] )
    		
    xrange = ((xrange[0]+xrange[1]) / 2) + $
    			pixel_size * xlen_est * [-0.5,0.5]
    
    yrange = ((yrange[0]+yrange[1]) / 2) + $
    			pixel_size * ylen_est * [-0.5,0.5]
  endif
  
  ;; Handle log axes.
  if (xaxis.log_flag) then begin
    if (xrange[1] LE 0) then xrange[1] = 1.0
    if (xrange[0] LE 0) then xrange[0] = xrange[1] / 1e10
  endif else begin
    if (xaxis.add_margin) then begin
      margin = abs(xrange[1]-xrange[0])*[-0.02,0.02]
      xrange = xrange + margin
    endif
  endelse

  if (yaxis.log_flag) then begin
    if (yrange[1] LE 0) then yrange[1] = 1.0
    if (yrange[0] LE 0) then yrange[0] = yrange[1] / 1e10
  endif else begin
    if (yaxis.add_margin) then begin
      margin = abs(yrange[1]-yrange[0])*[-0.02,0.02]
      yrange = yrange + margin
    endif
  endelse
  
  
  ;; Plot the axes.  The BACKGROUND keyword is ignored by the PostScript device.
  color_manager, (*st).color, plot_color, BLACK=black

  plot, [0,1], /NODATA, $
        XRANGE=xaxis.reverse_flag ? reverse(xrange) : xrange, YRANGE=yaxis.reverse_flag ? reverse(yrange) : yrange, XSTYLE=3, YSTYLE=3, $
        XMARGIN=xaxis.margin, YMARGIN=yaxis.margin, $
        TITLE=(*st).title, SUBTITLE=(*st).subtitle, $
        XTITLE=(*st).xaxis.title, YTITLE=(*st).yaxis.title, FONT=font,$
        XLOG=xaxis.log_flag, YLOG=yaxis.log_flag, $
        XTHICK=2, YTHICK=2,$
        COLOR=plot_color, BACKGROUND=black
  
  if ((*st).show_date) then $
    xyouts, 0.03, 0.01, /NORMAL, getenv('USER') + ' ' + systime(), $
  	  ALIGN=0, FONT=font, COLOR=plot_color
  
  if (!D.NAME EQ 'X') then coordsystem_manager, (*st).draw_widget, /SAVE
  
  
  ;; Compute the axis endpoints in data coords.
  region = convert_coord( !X.WINDOW, !Y.WINDOW, /NORM, /TO_DATA, /DOUBLE )
  xaxis.range = region[0,*]
  yaxis.range = region[1,*]
  
  
  ;; Compute image_position, image_size, & num_pixels.
  ll_norm = [!X.WINDOW[0], !Y.WINDOW[0]] + 0.01
  ur_norm = [!X.WINDOW[1], !Y.WINDOW[1]] - 0.01
  
  ll_data = convert_coord( ll_norm, /NORM, /TO_DATA, /DOUBLE )
  ur_data = convert_coord( ur_norm, /NORM, /TO_DATA, /DOUBLE )
  
  ll_device = convert_coord( ll_norm, /NORM, /TO_DEV )
  ur_device = convert_coord( ur_norm, /NORM, /TO_DEV )
  
  xaxis.image_position = ll_data[0]
  yaxis.image_position = ll_data[1]
  
  xaxis.image_size = (ur_data - ll_data)[0]
  yaxis.image_size = (ur_data - ll_data)[1]
  
  if (0 EQ (!d.flags and 1)) then begin
    ; If the device has fixed-size pixels (X window), then image pixels will
    ; coincide exactly with device pixels.
    xaxis.num_pixels = fix( (ur_device - ll_device)[0] )
    yaxis.num_pixels = fix( (ur_device - ll_device)[1] )
  endif else begin
    ; If the device has scalable pixels (PostScript), then the image array will
    ; be arbitrarily set to the same dimensions as the draw_widget.  
    xaxis.num_pixels = (*st).draw_size[0]
    yaxis.num_pixels = (*st).draw_size[1]
  endelse
  
; wcs_object, (*st).xaxis.wcs, CTYP=ctypx, /GET
; wcs_object, (*st).yaxis.wcs, CTYP=ctypy, /GET
; if (ctypx EQ '') then ctypx = 'x'
; if (ctypy EQ '') then ctypy = 'y'
; widget_control, (*st).wcs_name, SET_VALUE=ctypx+','+ctypy+'='
 	
endif ; keyword_set( show_axes )

;; Copy modified axis structures back to state structure.
(*st).xaxis = xaxis
(*st).yaxis = yaxis
(*st).zaxis = zaxis

;; Extract some return keyword values.
draw_widget  = (*st).draw_widget
big_marker   = (*st).big_marker
small_marker = (*st).small_marker
get_title    = (*st).title
get_subtitle = (*st).subtitle
unity_aspect = (*st).unity_aspect


if keyword_set( show_markers ) then begin
  color_manager, (*st).color, plot_color
  
  mark_window,   big_marker[0],   big_marker[1], 5, /DATA, COLOR=plot_color
  mark_window, small_marker[0], small_marker[1], 3, /DATA, COLOR=plot_color
endif

if arg_present( get_mouse ) then begin
  coordsystem_manager, (*st).draw_widget, /RESTORE
  if NOT keyword_set(prompt) then prompt = 'Select a point with the mouse.'

  get_mouse = PlotWindowMouseFn( st, (*st).aspect_button, prompt )
endif

return
end
	

