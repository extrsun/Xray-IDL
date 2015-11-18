;+
;========================================================================
;;;
;;; FILE NAME:    $Id: eb_trend_1d.pro 1599 2002-06-20 15:32:50Z patb $
;;;
;;; DESCRIPTION:  Univariate Trend Analysis Widget for Event Browser
;;;
;;;               The manager_widget parameter is the widget ID of the 
;;;               Manager Widget (the widget that is calling this function).
;;;
;;;               The UVALUE of the first child of the base is used to store
;;;               a structure representing the state of the display widget.  
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        See the document "X-ray Event Browser Software Design
;;;               Specification"
;;;
;-
;========================================================================

;==========================================================================
FUNCTION EbTrend1dDisplayEvent, event

destroy_flag      = 0
redisplay_flag    = 0
weight_name       = ''

;; Get the widget state structure.
top_base = event.handler
widget_control, top_base, GET_UVALUE=state, /NO_COPY, $
		/HOURGLASS


;; Handle the event; branch first based on which widget generated it.
case event.id of

  ;------------------------------------------------------------------------
  state.top_base: $
    begin
    case event.name of
      'working_set_stale': $
	begin
	end

      'working_set_delete': $
	begin
	end

      'working_set_fresh' : $
	begin
	state.dataset_name = event.dataset_name
        redisplay_flag     = 1
	end

      'domain_set_revised' : $
	begin
        state.relabel_flag       = 1
	end

      else: message, 'event not handled'
    endcase
    end


  ;------------------------------------------------------------------------
  state.binsize: $
    begin
    redisplay_flag     = 1
    state.relabel_flag = 1
    end

  ;------------------------------------------------------------------------
  state.min_group_size: $
    begin
    redisplay_flag     = 1
    state.relabel_flag = 1
    end

  ;------------------------------------------------------------------------
  state.trend_property: $
    begin
    state.tprop_name = state.tprop_list[event.index]
    state.new_property_flag = 1
    state.relabel_flag      = 1
    end

  ;------------------------------------------------------------------------
  state.measurement_property: $
    begin
    state.mprop_name = state.mprop_list[event.index]
    state.new_property_flag = 1
    state.relabel_flag      = 1
    end


  ;------------------------------------------------------------------------
  state.dataset_1d: $
    begin
    end

  ;------------------------------------------------------------------------
  state.replot_button: $
    begin
    redisplay_flag = 1
    end

  ;------------------------------------------------------------------------
  state.destroy_button: $
    begin
    destroy_flag   = 1
    end

  ;------------------------------------------------------------------------
  else: message, 'event not handled'
endcase


if (state.new_property_flag) then begin
  ;; Hide the plot, and enable the replot button.
  widget_control, state.dataset_1d,    SENSITIVE=0, BAD_ID=bad_id
  widget_control, state.replot_button, SENSITIVE=1
endif


if (redisplay_flag) then begin
  z=GetProperty( state.tprop_name, WDS_DATA=tdata, LABEL=tlabel )
  state.tlabel = tlabel
  z=GetProperty( state.mprop_name, WDS_DATA=mdata, LABEL=mlabel, WCS=mwcs )
  state.mlabel = mlabel
    
  ;; Remove all the existing datasets except "group0".
  for ii=1,state.num_groups-1 do begin
      name = string(ii,F='("group",I0)')
      dataset_1d, state.dataset_1d, DATASET_NAME=name, /DELETE, REDRAW=0
  endfor
  
  ;; Divide up the range of the trend property into bins and group the
  ;; measurement property data associated with each bin.
  widget_control, state.binsize       , GET_VALUE=binsize
  widget_control, state.min_group_size, GET_VALUE=min_group_size
  
  maxval = max(tdata, MIN=minval)
  
  if (state.new_property_flag) then begin  
    binsize = (maxval-minval)/2.
    widget_control, state.binsize       , SET_VALUE=binsize
  endif
  
  if ((maxval-minval)/binsize GT 500) then begin
    print, 'Changing binsize to keep bin count < 500!'
    binsize = (maxval-minval)/500.
    widget_control, state.binsize       , SET_VALUE=binsize
  endif  

  if (binsize EQ 0) then begin  
    binsize = 1.0
    widget_control, state.binsize       , SET_VALUE=binsize
  endif
  
  ; If trend property is integer-valued, then offset minval by 0.5 to keep
  ; bin edges away from data values.
  if (size(tdata, /TYPE) LE 3) then begin
    minval = minval - 0.5
  endif

  state.num_groups = 0
  binnum = 0L
  done   = 0
  while (NOT done) do begin
    low  = minval + binsize *  binnum
    high = minval + binsize * (binnum+1)
    cen  = low+binsize/2

    index=where( (low LE tdata) AND (tdata LT high), count )
    if (count LE (min_group_size>0)) then begin
;      print, 'Small group ignored.'
    endif else begin
      range_name = strcompress(string(tlabel,low,high, $
      				      F='(A,"=[",G10.4,",",G10.4,"]")'))
      print, 'Accepted group ' + range_name
      name = string(state.num_groups,F='("group",I0)')
      dataset_1d, state.dataset_1d, mdata[index], METRIC=cen, REDRAW=0, $
      		  DATASET_NAME=name, $
      		  DESCRIPTION=range_name
      
      if (state.new_property_flag) then begin
        dataset_1d, state.dataset_1d, DATASET=name, REDRAW=0, BINSIZE=0
      endif 
  
      state.num_groups = state.num_groups + 1
      if (state.num_groups GE 30) then done = 1
    endelse
    
    if (high GT maxval) then done = 1
    binnum = binnum + 1
  endwhile
  
  if (state.relabel_flag) then begin
    state.relabel_flag = 0
    dataset_1d, state.dataset_1d, DATASET='group0', /REDRAW, $
    		XTITLE=mlabel, XWCS=mwcs
  endif else dataset_1d, state.dataset_1d, DATASET='group0', /REDRAW

  state.new_property_flag = 0
  widget_control, state.dataset_1d,    SENSITIVE=1
  widget_control, state.replot_button, SENSITIVE=0
endif ;redisplay_flag SET


;; Save the state structure.
parent = state.parent
widget_control, top_base, SET_UVALUE=state, /NO_COPY


;; Destroy the widget if desired.
if (destroy_flag) then widget_control, parent, /DESTROY

return,0
end


;========================================================================
FUNCTION eb_trend_1d, manager_widget, $
	 dataset_name, tprop_name, mprop_name, $
	 UVALUE=uvalue, TITLE=title, GROUP=group, GENERIC=generic

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(title)  then title  = 'Univariate Trend Analysis'
if NOT keyword_set(group)  then group  = 0

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.
sz = get_screen_size()

parent = plot_window_topbase(UVALUE=uvalue, TITLE=title, GROUP=group,$
			     XOFFSET=sz[0]-680, YOFFSET=sz[1]-680)

top_base = widget_base( parent, /COLUMN, $
			EVENT_FUNC='EbTrend1dDisplayEvent' )

 dataset_1d_base = widget_base( top_base, /SPACE, XPAD=0, YPAD=0 )

 dataset_1d, dataset_1d, PARENT_WIDGET=dataset_1d_base


 bottom_base = widget_base( top_base, /ROW, /SPACE,XPAD=0,YPAD=0, /ALIGN_RIGHT )

		
 if keyword_set(generic) then begin
   tprop_list = [GetPropertyNames( /FITS, DIM=1, /NUMERIC_ONLY ),'timestamp','row_number']
 endif else begin
   tprop_list = ['x_position', 'y_position', 'timestamp', 'energy', 'wavelength', 'pha', 'instrument_grade', 'asca_grade', 'exposure', 'ccd_id', 'amp_id', 'generic1', 'generic2', 'generic3', 'generic4', 'generic5','generic6']
 endelse

 if keyword_set(generic) then begin
   mprop_list = [GetPropertyNames( /FITS, DIM=1, /NUMERIC_ONLY ),'timestamp','row_number']
 endif else begin
   mprop_list = ['x_position', 'y_position', 'timestamp', 'energy', 'wavelength', 'pi', 'pha', 'instrument_grade', 'asca_grade', 'exposure', 'generic1', 'generic2', 'generic3', 'generic4', 'generic5','generic6']
 endelse

  measurement_property = widget_droplist( bottom_base, VALUE=mprop_list, $
  					  TITLE='Measurement Property' )

  trend_property = widget_droplist( bottom_base, VALUE=tprop_list, $
  					  TITLE='Trend Property' )

 extra_base = widget_base( top_base, /ROW, SPACE=5,XPAD=0,YPAD=0, /ALIGN_RIGHT )

  binsize = cw_field( extra_base, /FLOAT, /RETURN_EVENTS, XSIZE=8, $
			VALUE=1, TITLE='Trend property bin size')

  min_group_size = cw_field( extra_base, /LONG, /RETURN_EVENTS, XSIZE=5, $
			VALUE=100, TITLE='Min property group size')

  replot_button = widget_button(extra_base,VALUE='Replot')

  destroy_button = widget_button( extra_base,VALUE='Dismiss' )


  trend_index       = (where(tprop_list EQ tprop_name, count))[0]
  if (count EQ 0) then begin
    trend_index = 0
    tprop_name = tprop_list[trend_index]
  endif
  
  measurement_index = (where(mprop_list EQ mprop_name, count))[0]
  if (count EQ 0) then begin
    measurement_index = 1 < (n_elements(mprop_list)-1)
    mprop_name = mprop_list[measurement_index]
  endif

widget_control, measurement_property, SET_DROPLIST_SELECT=measurement_index
widget_control, trend_property,       SET_DROPLIST_SELECT=trend_index


;; Create the widget state structure.

state = { parent:parent, $
	; The id of the Manager Widget.
	manager_widget:manager_widget, $

	;IDs of widgets that generate events or have to be updated.
	top_base:top_base, dataset_1d_base:dataset_1d_base,$
	dataset_1d:dataset_1d, binsize:binsize, min_group_size:min_group_size, $
	num_groups:0, $
	tprop_list:tprop_list, mprop_list:mprop_list, $
	tprop_name:tprop_name, mprop_name:mprop_name,$
	trend_property:trend_property, $
	measurement_property:measurement_property, $
	replot_button:replot_button, destroy_button:destroy_button, $

	; Other stuff
	tlabel:'', mlabel:'', $
	dataset_name: dataset_name, new_property_flag:1, relabel_flag:1 }


;; Realize the widget and register with manager.
widget_control, parent, /REALIZE

;; Simulate an event to initially draw the display.
event = {ID:replot_button, TOP:replot_button, $
	 HANDLER:0L, INDEX:replot_button}
widget_control, replot_button, SEND_EVENT=event

xmanager, 'EbTrend1d', parent, GROUP_LEADER=group, $
	  EVENT_HANDLER='PlotWindowTopbaseEventFn', /NO_BLOCK
	  
;; Store the state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY

return, top_base
end


