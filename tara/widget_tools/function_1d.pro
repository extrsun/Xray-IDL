;+
;========================================================================
;;;
;;; Function_1D Widget: $Id: function_1d.pro 4375 2012-10-30 20:17:03Z psb6 $
;;;
;;; Copyright (C) 1998, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION:  
;;; This tool may be used to analyze a function of one variable, y=f(x).
;;;
;========================================================================
;;; EVENTS GENERATED
;;; An event is forwarded to the client whenever the absicissa range
;;; is restricted.
;;;
;========================================================================
;;; INTERFACE TO CLIENT
;;;
;;; function_1d, top_base, x_data, X_ERROR=x_error, y_data, Y_ERROR=y_error, ERROR=error, $
;		 PARENT_WIDGET=parent, GROUP=group, BLOCK=block, $
;		 WIDGET_TITLE=widget_title,$
;		 TITLE=title,   SUBTITLE=subtitle, $
;		 XTITLE=xtitle, YTITLE=ytitle, UNITY_ASPECT=unity_aspect, LEGEND_STYLE=legend_style, $
;		 XWCS=xwcs, YWCS=ywcs, $
;			   
;		 DATASET_NAME=dataset_name, DESCRIPTION=description, $
;		 HIDE_DATASET=hide_dataset, DISABLE_DELETE=disable_delete, $
;		 COLOR=color, LINESTYLE=linestyle, PSYM=psym, PLOTSYM=plotsym, NSKIP_ERRORS=nskip_errors, $
;		 DELETE=delete,  ROI_MASK=mask, REDRAW=redraw,$
;
;    PLOT_WINDOW_OPTIONS=plot_window_options, PS_CONFIG=ps_config, PRINT=print_now
;;;
;;; The parameters PARENT_WIDGET & GROUP are the usual ones associated 
;;; with widgets.  If function_1d is a top-level widget, then WIDGET_TITLE
;;; controls the title in the X-window frame.
;;;
;;; The function_1d can work with multiple named datasets.
;;; Multiple datasets are passed with multiple calls to function_1d -- see
;;; example below.

;;; Errors (standard deviations) on the X and/or Y data points may be supplied by passing
;;; a vector of the same length as x_data & y_data via the X_ERROR and/or Y_ERROR paramters.
;;; The "ERROR" parameter is the same as Y_ERROR, and is retained for backward compatibility..

;;; A dataset may be deleted by the caller using /DELETE.

;;; COLOR can have the values 'red', 'blue', 'green', or 'white'.

;;; LINESTYLE can have the values 6 (none), 0 (solid), 1 (dotted), 
;;; 2 (dashed), 3 (dash dot), 4 (dash dot dot dot), 5 (long dashes).

;;; PSYM can have the values 0 (none), 1 (+), 2 (*), 3 (.), 
;;; 4 (diamond), 5 (triangle), 6 (box), 7 (X), 10(histogram),
;;; 8 (defined by plotsym.pro; see below).
;;;
;;; PLOTSYM is a string containing the parameters passed to plotsym.pro (AstroLib).  
;;; The most simple PLOTSYM would be just a string containing a number from 0...8, e.g. 
;;; PLOTSYM='4'.  A more complex example is PLOTSYM='7, 1.3, THICK=2'
;;; Pass PSYM=8 when using PLOTSYM.

;;; NSKIP_ERRORS is the NSKIP parameter in oploterror.pro.

;;; The state of this widget is stored as an anonymous structure in the 
;;; user value slot of the first child (the normal convention).


;==========================================================================
;;;                     CALLING OPTIONS

;;; From a non-widget program or the command line, plot Y1=f1(X) & Y2=f2(X).
;;; Widget events will be processed when the command line returns.
;;; ** function_1d, id, X, Y1,  DATASET_NAME='First'
;;; ** function_1d, id, X, Y2, DATASET_NAME='Second'

;;; From a non-widget program plot Y1=f1(X) & Y2=f2(X) but block further
;;; program execution until the function_1d widget is destroyed.
;;; ** function_1d, id, X, Y1, /BLOCK, DATASET_NAME='First'
;;; ** function_1d, id, X, Y2, /BLOCK, DATASET_NAME='Second'
;;; ** xmanager

;;; From a widget program, create a top-level function_1d.
;;; ** function_1d, id, X, Y, GROUP=creator_widget_id

;;; Create a function_1d as a child of another widget.
;;; ** function_1d, id, X, Y, PARENT_WIDGET=base

;;; Delete a dataset from an existing function_1d.
;;; ** function_1d, id, /DELETE, DATASET_NAME=name

;;; Obtain a pointer to a byte array that shows which data points in the 
;;; specified dataset are retained by the user's filter.  If no filter
;;; is defined, then the pointer points to an undefined variable.
;;; ** function_1d, id, DATASET_NAME=name, ROI_MASK=mask
;==========================================================================
;-

;==========================================================================
;;; Create the widget.
;==========================================================================
FUNCTION CreateFunction1d, PARENT_WIDGET=parent, GROUP=group, BLOCK=block, $
			  WIDGET_TITLE=widget_title

;; Call color_manager to switch to DirectColor if available.
color_manager

;; Default values
if (0 EQ n_elements(group))          then group = 0
if (0 EQ n_elements(widget_title))   then widget_title = 'Function_1D'
  
;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

this_is_top_level = (0 EQ n_elements(parent))
if this_is_top_level then begin
 center  = ScreenCenter()
 xoffset = center(0) - 500
 yoffset = center(1) - 250
 parent = plot_window_topbase(TITLE=widget_title,GROUP_LEADER=group, $
                      XOFFSET=xoffset, YOFFSET=yoffset)
endif

top_base = widget_base( parent, /BASE_ALIGN_CENTER, $
			FRAME=(this_is_top_level EQ 0), $
			EVENT_FUNC='Function1dEventFn', $
			KILL_NOTIFY='Function1dCleanup', $
			/COLUMN, /SPACE, XPAD=0, YPAD=0 )

upper_base = widget_base( top_base, /ALIGN_LEFT, /ROW, $
			   SPACE=8, XPAD=0, YPAD=0 )
			   
 menu = [{ CW_PDMENU_S, flags:1, name:'File' }, $ 
	 { CW_PDMENU_S,   0,        'Print' }, $ 
	 { CW_PDMENU_S,   0,        'Save Function (FITS Table)' }, $
	 { CW_PDMENU_S,   0,        'Save Function (ASCII)' }, $
	 { CW_PDMENU_S,   0,        'Load Function (FITS Table or ASCII)' }, $
	 { CW_PDMENU_S,   0,        'Delete Function' }, $
	 { CW_PDMENU_S,   2,        'Hide/Show Functions' }]

 if this_is_top_level then begin
   menu[6].flags = 0
   menu = [menu, { CW_PDMENU_S,     2,      'Exit' }]
 endif 
 
 file_menu = cw_pdmenu(upper_base, menu, /RETURN_INDEX)
     
 edit_button    = widget_button( upper_base, VALUE='Edit' )
 
 roi_base = widget_base(upper_base, /ROW, /SPACE, XPAD=0, YPAD=0, /FRAME)
 
   roi_mode = widget_droplist( roi_base, VALUE=['None','Stats','Filter'], $
   				  TITLE='ROI:' )
   
   roi_define = widget_button( roi_base, VALUE='Use Markers' )
  
   roi_semantics = widget_droplist( roi_base, VALUE=['Include','Exclude'] )

 menu = [{ CW_PDMENU_S, flags:1, name:'Fit' }, $ 
	 { CW_PDMENU_S,   1,   'Integrated Gaussian' }, $ 
	  { CW_PDMENU_S,       0,      'no background' }, $
	  { CW_PDMENU_S,       0,      'constant background' }, $
	  { CW_PDMENU_S,       0,      'linear background' }, $
	  { CW_PDMENU_S,       2,      'quadratic background'},$

	 { CW_PDMENU_S,   1,   'Gaussian' }, $ 
	  { CW_PDMENU_S,       0,      'no background' }, $
	  { CW_PDMENU_S,       0,      'constant background' }, $
	  { CW_PDMENU_S,       0,      'linear background' }, $
	  { CW_PDMENU_S,       2,      'quadratic background' }, $

	 { CW_PDMENU_S,   3,   'Polynomial' }, $ 
	  { CW_PDMENU_S,       0,      'linear' }, $
	  { CW_PDMENU_S,       2,      'quadratic' }] 
	 
 fit_menu = cw_pdmenu(upper_base, menu, /RETURN_INDEX)


middle_base = widget_base( top_base, /ALIGN_LEFT, /ROW, $
			   /SPACE, XPAD=0, YPAD=0 )

 dataset_list = widget_droplist( middle_base, VALUE='NULL', /DYNAMIC_RESIZE )
 
 modes = ['Y = f(X)', 'Y = INTEGRAL{ f(X) }']
 mode_list = widget_droplist( middle_base, VALUE=modes )

msg_label = widget_label( top_base, /DYNAMIC_RESIZE, VALUE=' ' )
    
plot_window, pw_id, PARENT=top_base


; Setup state structure.
dataset = { index:0B, name:'', description:'', hidden_flag:0, disable_delete:0,$

	    x_data:ptr_new(/ALLOC), x_error:ptr_new(/ALLOC),  x_error_available:0, $
      y_data:ptr_new(/ALLOC), y_error:ptr_new(/ALLOC),  y_error_available:0, $
	    
	    x_filtered_data:ptr_new(/ALLOC), x_filtered_error:ptr_new(/ALLOC),$
      y_filtered_data:ptr_new(/ALLOC), y_filtered_error:ptr_new(/ALLOC), $
	    mask:ptr_new(/ALLOC), filter_stale:1B, $
	    stats:'', stats_stale:1B, $
	    
	    ; num_data_points, x_max, x_min, etc. refer to the filtered dataset!
	    num_data_points:0L, y_max:0.0, y_min:0.0, x_max:0.0, x_min:0.0, $
	    
	    plot: {color:'white', psym:3, plotsym:'', line:0, show_errors:0, nskip_errors:4}, $
	    
	    integral: {samples:ptr_new(/ALLOC), stale:0B} $
	   } 		
	    
r2=[0.0,0.0]
state = { parent:parent, $ 
	;IDs of widgets that generate events or need to be updated. 
	roi_mode:roi_mode, $
	roi_define:roi_define, roi_semantics:roi_semantics,$
	show_roi:1, $
	
	dataset_list:dataset_list, $
	file_menu:file_menu, edit_button:edit_button, $
	fit_menu:fit_menu, $
	mode_list:mode_list, $
	msg_label:msg_label, pw_id:pw_id, $
	
	;Dataset structures
	selected_name:'', datasets: replicate( dataset, 60 ), $
	
	;ROI parameters
	roi_params: {xl:1., xh:0.}, $

	;Other state information.
	legend_style:0, share_params:1, fitnum:1, $
	note:'', note_x:0.0, note_y:0.0, stale_titles:1, $
	title:'', ytitle:'Y', annotation_routine:'', $
	ps_config:ptr_new(/ALLOC), print_now:0B }
	
ptr_free, dataset.x_data, dataset.y_data, dataset.x_error, dataset.y_error,$
	  dataset.x_filtered_data, dataset.y_filtered_data, $
	  dataset.x_filtered_error, dataset.y_filtered_error, dataset.mask, $
	  dataset.integral.samples
	  
;; Allocate heap variables in dataset structures.
; Ignore the last color name which is 'black'.
color_manager, COLOR_NAMES=color_names  & nc = n_elements(color_names)-1
line =[0,1,2,3,4,5]	& nl = n_elements(line)
for ii = 0, n_elements(state.datasets)-1 do begin
  state.datasets[ii].index   = ii
  state.datasets[ii].x_data  = ptr_new([0])
  state.datasets[ii].y_data  = ptr_new([0])
  state.datasets[ii].x_error = ptr_new([0])
  state.datasets[ii].y_error = ptr_new([0])
  state.datasets[ii].x_filtered_data = ptr_new([0])
  state.datasets[ii].y_filtered_data = ptr_new([0])
  state.datasets[ii].x_filtered_error  = ptr_new([0])
  state.datasets[ii].y_filtered_error  = ptr_new([0])
  state.datasets[ii].mask            = ptr_new(/ALLOC)
  state.datasets[ii].integral.samples= ptr_new(/ALLOC)
  
  state.datasets[ii].plot.color = color_names[ ii         mod nc]
  state.datasets[ii].plot.line  =       line [(ii  /  nc) mod nl]
endfor


;; Save state structure.
widget_control, top_base, SET_UVALUE=ptr_new(state, /NO_COPY)


;; If this is top-level widget, realize it and register.
if this_is_top_level then begin
  widget_control, parent, /REALIZE

  ; The fancy combination of JUST_REG and NO_BLOCK is necessary to get the
  ; widget to behave when called by either widget applications or normal
  ; programs (which block on tty reads).
  xmanager, 'function_1d', parent, GROUP_LEADER=group, $
	    JUST_REG=keyword_set(block), NO_BLOCK=(keyword_set(block) EQ 0), $
	    EVENT_HANDLER='PlotWindowTopbaseEventFn'
endif

return, top_base
END

;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO Function1dCleanup, top_base

widget_control, top_base, GET_UVALUE=st

;; Free the heap vars allocated locally.
ptr_free, (*st).datasets.x_data, (*st).datasets.y_data, (*st).datasets.x_error, (*st).datasets.y_error,$
          (*st).datasets.x_filtered_data, (*st).datasets.y_filtered_data, $
          (*st).datasets.x_filtered_error, (*st).datasets.y_filtered_error, $
          (*st).datasets.mask, (*st).datasets.integral.samples, $
          (*st).ps_config
ptr_free, st
return
end

;==========================================================================
;;; Save a dataset structure back to the widget state structure.
;==========================================================================
PRO Save1dFunction, dataset, st
(*st).datasets[ dataset.index ] = dataset
return
end


;==========================================================================
;;; Routine to filter a dataset or compute ROI statistics.
;;; The structure 'dataset' is modified!
;==========================================================================
PRO ApplyFunction1dRoi, dataset, st

;; First, see if the filter has already been applied.
if (dataset.filter_stale EQ 0 AND dataset.stats_stale EQ 0) then return

area = 0
	
;; If there is no filter defined, then just point to full dataset and
;; set the mask to all 1's.
roi_mode  = widget_info( (*st).roi_mode, /DROPLIST_SELECT )

if (roi_mode EQ 0 ) then begin
  x_roi_data  = dataset.x_data
  y_roi_data  = dataset.y_data
  x_roi_error = dataset.x_error  
  y_roi_error = dataset.y_error  
  mask = replicate( 1B, n_elements(*dataset.x_data) )
  
endif else begin

  ;; If we get here, then there is a ROI defined and we need to apply it.
  ;; Remember that the result  could be the empty set, represented by
  ;; x_roi_data pointing to an undefined heap var.
  roi     = (*st).roi_params
  x_data  = dataset.x_data
  y_data  = dataset.y_data
  x_error = dataset.x_error
  y_error = dataset.y_error
  
  ;; Initialize the ROI if it has never been assigned by user or if it is
  ;; null.
  if (roi.xl GT roi.xh) then begin
    roi.xh = max(*x_data, MIN=x_min, /NAN)
    roi.xl = x_min
  endif
  
  mask = (roi.xl LE *x_data AND roi.xh GE *x_data)
  area = roi.xh - roi.xl
  
  (*st).roi_params = roi

  ;; Invert the mask if exclude mode is chosen.
  if (1 EQ widget_info( (*st).roi_semantics, /DROPLIST_SELECT )) then begin
    mask = (mask EQ 0)
    area = 0
  endif
  
  ;; Compute the datapoints that are in the ROI,
  index = where( mask, count )

  if (count EQ 0) then begin
    x_roi_data  = ptr_new(/ALLOC)
    y_roi_data  = ptr_new(/ALLOC)
    x_roi_error = ptr_new(/ALLOC)
    y_roi_error = ptr_new(/ALLOC)
  endif else begin
    x_roi_data = ptr_new( (*x_data)[index], /NO_COPY)
    y_roi_data = ptr_new( (*y_data)[index], /NO_COPY)
    
    if dataset.x_error_available $
      then x_roi_error = ptr_new( (*x_error)[index], /NO_COPY) $
      else x_roi_error = ptr_new(/ALLOC)
    
    if dataset.y_error_available $
      then y_roi_error = ptr_new( (*y_error)[index], /NO_COPY) $
      else y_roi_error = ptr_new(/ALLOC)
  endelse
endelse


;; If we're supposed to be computing statistics, then do it.
if (dataset.stats_stale) then begin
  dataset.stats_stale  = 0
  
  ;; Find the max and min of the ROI dataset.
  num_points = n_elements(*x_roi_data)
  if (num_points EQ 0) then begin
    dataset.stats = 'NO DATA IN ROI'
  endif else begin
    dataset.stats = string( num_points, f='(I0," function samples")' )
              
    dataset.stats = strcompress( dataset.stats )
  endelse  
endif


;; If we're supposed to be filtering, then do it.
if (dataset.filter_stale) then begin
  dataset.filter_stale = 0

  ;; If the existing x_filtered_data tag points to its own heap var, then
  ;; get rid of that heap var.
  if (dataset.x_filtered_data NE dataset.x_data) then $
  	ptr_free, dataset.x_filtered_data
  	
  if (dataset.y_filtered_data NE dataset.y_data) then $
	ptr_free, dataset.y_filtered_data

  if (dataset.x_filtered_error NE dataset.x_error) then $
	ptr_free, dataset.x_filtered_error

  if (dataset.y_filtered_error NE dataset.y_error) then $
	ptr_free, dataset.y_filtered_error

  ;; Is filtering selected?
  if (widget_info( (*st).roi_mode, /DROPLIST_SELECT ) NE 2) then begin
    dataset.x_filtered_data  = dataset.x_data
    dataset.y_filtered_data  = dataset.y_data
    dataset.x_filtered_error = dataset.x_error
    dataset.y_filtered_error = dataset.y_error
    *dataset.mask = 0
    dum = temporary( *dataset.mask )
  endif else begin
    dataset.x_filtered_data  = x_roi_data
    dataset.y_filtered_data  = y_roi_data
    dataset.x_filtered_error = x_roi_error
    dataset.y_filtered_error = y_roi_error
    
    *dataset.mask = temporary( mask )
  endelse

  ;; Find the max and min of the filtered dataset.
  dataset.num_data_points = n_elements( *dataset.x_filtered_data )
  
  if (dataset.num_data_points EQ 0) then begin
    x_min = 0  &  y_min = 0  &  x_max = 1  &  y_max = 1
  endif else begin
    x_max = max(*dataset.x_filtered_data, MIN=x_min, /NAN)
    y_max = max(*dataset.y_filtered_data, MIN=y_min, /NAN)
  endelse  
  
  dataset.x_max           = x_max
  dataset.x_min           = x_min
  dataset.y_max           = y_max
  dataset.y_min           = y_min
   
  
  ;; Since we've changed the filtered dataset, mark all derived data structures
  ;; as stale.
  dataset.integral.stale   = 1
  
endif ; (dataset.filter_stale EQ 1)

return
end

;==========================================================================
;;; Routine to integrate a function.
;;; The structure 'dataset' is modified!
;==========================================================================
PRO ComputeFunction1dIntegral, dataset, st

d = dataset.integral

;; First, see if the integral has already been calculated.
if (d.stale EQ 0) then return

d.stale = 0

;; Now, make sure there is some data left after filtering.
if (dataset.num_data_points EQ 0) then begin
  *d.samples = lonarr(4)
  d.subtitle = 'NULL DATASET!'
  dataset.integral = d
  (*st).stale_titles = 1
  return
endif

*d.samples = total( *dataset.y_filtered_data, /CUMULATIVE ) 

dataset.integral = d

(*st).stale_titles = 1
return
end


;==========================================================================
;;; Routine to update the widget's appearance
;==========================================================================
PRO RedrawFunction1d, st

widget_control, /HOURGLASS

mode = widget_info( (*st).mode_list, /DROPLIST_SELECT )

;--------------------------------------------------------------------------
;; Find the non-null datasets and update the dataset_list droplist widget.
;--------------------------------------------------------------------------
dataset_indexes = where( (*st).datasets.name NE '' )

names      = ((*st).datasets.name)       [dataset_indexes]
hide_flags = ((*st).datasets.hidden_flag)[dataset_indexes]

droplist_index = (where( names EQ (*st).selected_name, count ))[0]

if (count EQ 0) then begin
  (*st).selected_name = names[0]
  droplist_index      = 0
endif

hidden = where( hide_flags EQ 1, count )
if (count NE 0) then names[hidden] = '{' + names[hidden] + '}'

widget_control, (*st).dataset_list, $
		SET_DROPLIST_SEL=droplist_index, SET_VAL=names

selected_index = (where( (*st).datasets.name EQ (*st).selected_name))[0]


;--------------------------------------------------------------------------
;; Filter all the datasets we're going to display -- this calculates 
;; information which may be needed to set axis ranges.
;; Compute the integrals needed -- this computes the subtitle
;; we may need to assign.
;--------------------------------------------------------------------------
;; Apply the ROI to all the datasets that will be shown and compute densities,
;; saving the modified structures back to (*st).
shown = where( ((*st).datasets.name NE '') AND $
  	       ((*st).datasets.hidden_flag EQ 0), count )

if (count EQ 0) then shown = [selected_index]
  
for ii = 0, n_elements(shown)-1 do begin
    dataset = (*st).datasets[ shown[ii] ]
    ApplyFunction1dRoi,       dataset, st
    if (mode EQ 1) then ComputeFunction1dIntegral, dataset, st
    Save1dFunction, dataset, st
endfor
    
plotsets = ((*st).datasets)[ shown ]
    
;--------------------------------------------------------------------------
;; If the selected dataset is visible, then show its statistics.
;--------------------------------------------------------------------------
selected_dataset = (*st).datasets[ selected_index ]
if (selected_dataset.hidden_flag EQ 0) then $
  widget_control, (*st).msg_label, SET_VALUE=selected_dataset.stats


;--------------------------------------------------------------------------
;; Assign default axis ranges if necessary.
;--------------------------------------------------------------------------
plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis

set_xrange=0
set_yrange=0			  
if (xaxis.default_flag) then begin
    set_xrange = [min( plotsets.x_min, /NAN ), max( plotsets.x_max, /NAN )]
endif

if (yaxis.default_flag) then begin
 case mode of
  0: set_yrange = [min( plotsets.y_min, /NAN ), max( plotsets.y_max, /NAN )]

  1: begin
     min_y =  1.0E10
     max_y = -1.0E10
     for ii = 0, n_elements(plotsets)-1 do begin
       int_max = max( *plotsets[ii].integral.samples, MIN=int_min,/NAN )
       min_y = min( [min_y, int_min], /NAN )
       max_y = max( [max_y, int_max], /NAN )
     endfor
     set_yrange = [min_y, max_y]
     end
  endcase
endif

plot_window, (*st).pw_id, /ADD_MARGIN, $
			  SET_XRANGE=set_xrange, SET_YRANGE=set_yrange


;; Display the datasets.
case (mode) of
 ;-------------------------------------------------------------------------
 0: $ Y=f(X)
 ;-------------------------------------------------------------------------
  begin
  ; Draw the plot axes.
  if ((*st).stale_titles) then begin
    case 1 of
     ((*st).title NE ''):         title = (*st).title 
     (n_elements(plotsets) EQ 1): title = plotsets[0].description
     else:			  title = ''
    endcase

    plot_window, (*st).pw_id, YTITLE=(*st).ytitle, TITLE=title
  endif
  
  plot_window, (*st).pw_id, /SHOW_AXES
  
  ;------------------------------------------------------------------------
  ; Plot each of the datasets.
  ;------------------------------------------------------------------------
  for ii = 0, n_elements(plotsets)-1 do begin
    dataset = plotsets[ii]
    
    if (dataset.num_data_points GT 0) then begin
      p = dataset.plot
      color_manager, p.color, plot_color
      if keyword_set(p.plotsym) then begin
        cmd = 'plotsym, ' + p.plotsym
        if NOT execute(cmd) then message, 'call to plotsym.pro failed', /CONTINUE
      endif
             
      if (p.line EQ 6) then begin
        ;We want symbols only.
        p.line = 0
        
        ; Protext oploterror tool from seeing xerror or yerror vectors that are all NaN---it will crash.
        if dataset.x_error_available && (total(/INT, finite(*dataset.x_filtered_error)) EQ 0) then dataset.x_error_available=0
        if dataset.y_error_available && (total(/INT, finite(*dataset.y_filtered_error)) EQ 0) then dataset.y_error_available=0
  
        if (p.show_errors && dataset.x_error_available && dataset.y_error_available) then begin
          oploterror, *dataset.x_filtered_data, *dataset.y_filtered_data, *dataset.x_filtered_error, *dataset.y_filtered_error, $
        	              PSYM=p.psym, COLOR=plot_color, ERRSTYLE=0, ERRCOLOR=plot_color, NSKIP=p.nskip_errors, /NOHAT
        
        endif else if (p.show_errors && dataset.y_error_available) then begin
          oploterror, *dataset.x_filtered_data, *dataset.y_filtered_data, *dataset.y_filtered_error, $
        	              PSYM=p.psym, COLOR=plot_color, ERRSTYLE=0, ERRCOLOR=plot_color, NSKIP=p.nskip_errors, /NOHAT
        endif else begin
          oplot,      *dataset.x_filtered_data, *dataset.y_filtered_data, $
        	              PSYM=p.psym, COLOR=plot_color
        endelse
      endif else begin                           
        ;We want both lines and symbols.
        if (p.psym NE 10) then p.psym = -p.psym
        
        if (p.show_errors && dataset.x_error_available && dataset.y_error_available) then begin
          oploterror, *dataset.x_filtered_data, *dataset.y_filtered_data, *dataset.x_filtered_error, *dataset.y_filtered_error, $
        	              LINESTYLE=p.line, PSYM=p.psym, COLOR=plot_color, ERRSTYLE=0, ERRCOLOR=plot_color, NSKIP=p.nskip_errors, /NOHAT
        
        endif else if (p.show_errors && dataset.y_error_available) then begin
          oploterror, *dataset.x_filtered_data, *dataset.y_filtered_data, *dataset.y_filtered_error, $
        	              LINESTYLE=p.line, PSYM=p.psym, COLOR=plot_color, ERRSTYLE=0, ERRCOLOR=plot_color, NSKIP=p.nskip_errors, /NOHAT
        endif else begin
          oplot,      *dataset.x_filtered_data, *dataset.y_filtered_data, $
        	              LINESTYLE=p.line, PSYM=p.psym, COLOR=plot_color
        endelse
      endelse
      
    endif
  endfor
      
  ;------------------------------------------------------------------------
  ;; Draw a legend, if desired.  Be careful of color handling.  When
  ;; in grayscale Postscript mode, there are no colors in the IDL colormap,
  ;; so just use black.
  ;------------------------------------------------------------------------
  lstyle = (*st).legend_style
  if (lstyle NE 0) then begin
        ; Convert a vector of color names to a vector of color indexes.
        colors = lonarr( n_elements(plotsets) )
        for ii = 0, n_elements(plotsets)-1 do begin
          color_manager, plotsets[ii].plot.color, color_index
          colors[ii] = color_index
        endfor
        
        psym = plotsets.plot.psym
        ind = where(psym GT 8, count)
        if (count GT 0) then psym[ind]=0
      
        ; Coyote Graphics requires color indexes to be INT, not LONG.
        al_legend, plotsets.description, PSYM=psym,  $
                LINE=plotsets.plot.line, $
                /TOP,LEFT=(lstyle EQ 1),CEN=(lstyle EQ 2),RIGHT=(lstyle EQ 3),$
                COLORS=fix(colors), TEXTCOLORS=fix(colors), CHARSIZE=0.75*!P.CHARSIZE
  endif
  end
  
 ;-------------------------------------------------------------------------
 1: $ INTEGRAL OF FUNCTION
 ;-------------------------------------------------------------------------
  begin
  ; Draw the plot axes.
  if ((*st).stale_titles) then begin
    case 1 of
     ((*st).title NE ''):         title = (*st).title 
     (n_elements(plotsets) EQ 1): title = plotsets[0].description
     else:			  title = ''
    endcase

    plot_window, (*st).pw_id, YTITLE='Integral of '+(*st).ytitle, TITLE=title
  endif
  
  plot_window, (*st).pw_id, /SHOW_AXES
  
  ;------------------------------------------------------------------------
  ; Plot each of the datasets.
  ;------------------------------------------------------------------------
  for ii = 0, n_elements(plotsets)-1 do begin
    dataset = plotsets[ii]
    
    if (dataset.num_data_points GT 0) then begin
      p = dataset.plot
      color_manager, p.color, plot_color
            
      if (p.line EQ 6) then begin
        ;We want symbols only.
        p.line = 0
  
        oplot, *dataset.x_filtered_data, *dataset.integral.samples, $
        	     PSYM=p.psym, COLOR=plot_color
  
      endif else begin                           
        ;We want both lines and symbols.
        if (p.psym NE 10) then p.psym = -p.psym
        
        oplot, *dataset.x_filtered_data, *dataset.integral.samples, $
        	     LINESTYLE=p.line, PSYM=p.psym, COLOR=plot_color
      endelse
    endif
  endfor
      
  ;------------------------------------------------------------------------
  ;; Draw a legend, if desired.  Be careful of color handling.  When
  ;; in grayscale Postscript mode, there are no colors in the IDL colormap,
  ;; so just use black.
  ;------------------------------------------------------------------------
  lstyle = (*st).legend_style
  if (lstyle NE 0) then begin
        ; Convert a vector of color names to a vector of color indexes.
        colors = lonarr( n_elements(plotsets) )
        for ii = 0, n_elements(plotsets)-1 do begin
          color_manager, plotsets[ii].plot.color, color_index
          colors[ii] = color_index
        endfor
      
        psym = plotsets.plot.psym
        ind = where(psym GT 8, count)
        if (count GT 0) then psym[ind]=0
      
        ; Coyote Graphics requires color indexes to be INT, not LONG.
        al_legend, plotsets.description, PSYM=psym,  $
                LINE=plotsets.plot.line, $
                /TOP,LEFT=(lstyle EQ 1),CEN=(lstyle EQ 2),RIGHT=(lstyle EQ 3),$
                COLORS=fix(colors), TEXTCOLORS=fix(colors), CHARSIZE=0.75*!P.CHARSIZE
  endif
  end

endcase

(*st).stale_titles = 0

;------------------------------------------------------------------------
;; Draw the note and call an annotation function if specified.
;------------------------------------------------------------------------
if ((*st).note NE '') then begin
  xyouts, (*st).note_x, (*st).note_y, (*st).note, /DATA
endif

plot_window, (*st).pw_id, /SHOW_MARKERS

if ((*st).annotation_routine NE '') then begin
   catch, error_code

   if (error_code NE 0) $
     then dum=dialog_message(['ERROR!',!ERR_STRING],DIALOG_PAR=(*st).pw_id,/ERR)$
     else call_procedure, (*st).annotation_routine
   catch, /CANCEL
endif

;------------------------------------------------------------------------
;; Draw a graphic representing the ROI.
;------------------------------------------------------------------------
color_manager, BLUE=blue, GREEN=green
if ((*st).show_roi AND $
    (widget_info( (*st).roi_mode, /DROPLIST_SELECT ) NE 0)) then begin
  
    xl=(*st).roi_params.xl   &  xh=(*st).roi_params.xh
    if (!Y.TYPE EQ 0) then begin
      yl=!Y.CRANGE(0)    &  yh=!Y.CRANGE(1)
    endif else begin
      yl=10^!Y.CRANGE(0) &  yh=10^!Y.CRANGE(1)
    endelse
    
    plots, [xl,xl], [yl,yh], /DATA, THICK=2, COLOR=blue
    plots, [xh,xh], [yl,yh], /DATA, THICK=2, COLOR=blue 
endif ; (show_roi EQ 1)

  
return
end


;==========================================================================
;;; Event Handler Function
;==========================================================================
FUNCTION Function1dEventFn, Event

widget_control, /HOURGLASS
new_event   = 0
DestroyFlag = 0

;; Get the state structure. 
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st

selected_index   = (where( (*st).datasets.name EQ (*st).selected_name))(0)
selected_dataset = (*st).datasets[ selected_index ]

;; Process the event.
case Event.ID of

;--------------------------------------------------------------------------
; Plot window mouse events.
  (*st).pw_id: $
   begin
   if (Event.middle_button AND (selected_dataset.hidden_flag EQ 0) AND $
       (selected_dataset.num_data_points GT 0)) then begin
     mode = widget_info( (*st).mode_list, /DROPLIST_SELECT )
     
     plot_window, (*st).pw_id, DRAW_WIDGET=draw_widget
       
     ; We need to locate the datapoint closest to the click.
     ; Find distance from click to each data point in the device coor system.
     coordsystem_manager, draw_widget, /RESTORE
     click = convert_coord( event.button_coord, /DATA, /TO_DEVICE )
     case (mode) of
      0: $
       begin
       x_data = *selected_dataset.x_filtered_data
       y_data = *selected_dataset.y_filtered_data
       end
      1: $
       begin
       x_data = *selected_dataset.x_filtered_data
       y_data = *selected_dataset.integral.samples
       end
     endcase 
            
     xy_dev = convert_coord( x_data, y_data, /DATA, /TO_DEVICE )
       
     distances = (xy_dev(0,*) - click[0])^2 + (xy_dev(1,*) - click[1])^2
       		 
     minDist = min(distances,Imin, /NAN)
     x_pt    = x_data[Imin]
     y_pt    = y_data[Imin]
       
     case (mode) of
      0: $
       begin
       msg = string( Imin, x_pt, y_pt, $
                     f='(%"Sample # %d, f(%11.5g)=%11.5g")' )
       end
      1: $
       begin
       msg = string(  Imin, x_pt, y_pt,$
                     f='(%"Sample # %d, integral(%11.5g)=%11.5g")' )
       end
     endcase 
            
     tvcrs, x_pt, y_pt, /DATA
     widget_control, (*st).msg_label, SET_VALUE=strcompress(msg)
   endif ;(Event.middle_button)
   
   if (event.redraw) then RedrawFunction1d, st
   end


;--------------------------------------------------------------------------
; Dataset droplist
  (*st).dataset_list: $
   begin
   indexes          = where( (*st).datasets.name NE '' )
   names            = (*st).datasets[indexes].name
   (*st).selected_name = names[ event.index ]

   (*st).stale_titles = 1
   RedrawFunction1d, st
   end


;--------------------------------------------------------------------------
; Mode droplist
  (*st).mode_list: $
   begin
   (*st).stale_titles = 1
   RedrawFunction1d, st
   end


;--------------------------------------------------------------------------
; Fit menu
(*st).fit_menu: $
 begin
 num_data_points = selected_dataset.num_data_points
 if (num_data_points EQ 0) then return, 0

 weights = 0  &  weighted_fit = 0
 if (selected_dataset.y_error_available) then begin
   dum = where( *selected_dataset.y_filtered_error LE 0, count )
   if (count EQ 0) then begin
     ;; We are weighting by 1/(standard_deviation^2) here which is
     ;; what CURVEFIT (called by intgaussfit) wants.
     ;; These standard deviations are computed outside of function_1d.
     weights = 1.0 / (*selected_dataset.y_filtered_error^2) 
     weighted_fit = 1
   endif 
 endif
 
 if (2 LE event.value AND event.value LE 5) then begin
   ; INTEGRATED GAUSSIAN FIT
   ; Try to determine a binsize, assuming that the data points are
   ; sorted in X.
   steps = (*selected_dataset.x_filtered_data)[1:num_data_points-1] - $
   	   (*selected_dataset.x_filtered_data)[0:num_data_points-2] 
   step_max = max( steps, MIN=step_min, /NAN )

   if (step_max - step_min GT 0.01 * abs(step_max)) then begin
     dum=dialog_message('NO FIT PERFORMED; could not compute binsize.',$
			      DIALOG_PARENT=(*st).fit_menu)
     fit_done = 0
   endif else begin
     binsize = median(steps)


     ; Establish an error handler for exceptions thrown in fitting routines.
     catch, error_code

     if (error_code NE 0) then begin
       dum=dialog_message(['ERROR!',!ERR_STRING],DIALOG_PAR=(*st).fit_menu,/ERR)
       fit_done = 0
     endif else begin 
       fit_done = 1

     nterms = 1 + event.value

     if (num_data_points LE nterms) then message, /NONAME, 'Too few data points'

       case Event.value of
	    ;----------------------------------------------------------------
            2: begin 
	     y_fit = intgaussfit( *selected_dataset.x_filtered_data, $
	     			  *selected_dataset.y_filtered_data, $
	     			  BINSIZE=binsize, $
				  WEIGHTS=weights, ORDER=-1, /ERRORS, $
				  CHI2=chi2, REDUCED_CHI2=reduced_chi2, $
				  GAIN=area, MEAN=mean, SIGMA=sigma )
	    
             fit_name = "INT[gaussian]: "
	    end

	    ;----------------------------------------------------------------
            3: begin 
	     y_fit = intgaussfit( *selected_dataset.x_filtered_data, $
	     			  *selected_dataset.y_filtered_data, $
	     			  BINSIZE=binsize, $
				  WEIGHTS=weights, ORDER=0, /ERRORS, $
				  CHI2=chi2, REDUCED_CHI2=reduced_chi2, $
				  GAIN=area, MEAN=mean, SIGMA=sigma, $
				  CONSTANT=constant )

             fit_name = "INT[gaussian + constant]: "
	    end

	    ;----------------------------------------------------------------
            4: $
	    begin 
	     y_fit = intgaussfit( *selected_dataset.x_filtered_data, $
	     			  *selected_dataset.y_filtered_data, $
	     			  BINSIZE=binsize, $
				  WEIGHTS=weights, ORDER=1, /ERRORS, $
				  CHI2=chi2, REDUCED_CHI2=reduced_chi2, $
				  GAIN=area, MEAN=mean, SIGMA=sigma, $
				  CONSTANT=constant, LINEAR=linear )

             fit_name = "INT[gaussian + linear]: "
	    end

	    ;----------------------------------------------------------------
            5: $
	    begin 
	     y_fit = intgaussfit( *selected_dataset.x_filtered_data, $
	     			  *selected_dataset.y_filtered_data, $
	     			  BINSIZE=binsize, $
				  WEIGHTS=weights, ORDER=2, /ERRORS, $
				  CHI2=chi2, REDUCED_CHI2=reduced_chi2, $
				  GAIN=area, MEAN=mean, SIGMA=sigma, $
				  CONSTANT=constant, LINEAR=linear, $
				  QUADRATIC=quadratic )

             fit_name = "INT[gaussian + quadratic]: "
	    end
       endcase
     endelse ;error_code EQ 0

     catch, /CANCEL
   endelse ;binsize was computed
        
 endif else if (7 LE event.value AND event.value LE 10) then begin
   ; REGULAR GAUSSIAN FIT
   ; Establish an error handler for exceptions thrown in fitting routines.
   catch, error_code

   if (error_code NE 0) then begin
     dum=dialog_message(['ERROR!',!ERR_STRING],DIALOG_PAR=(*st).fit_menu,/ERR)
     fit_done = 0
   endif else begin 
     fit_done = 1

     nterms = -4 + event.value

     if (num_data_points LE nterms) then message, /NONAME, 'Too few data points'

     y_fit = gaussfit(*selected_dataset.x_filtered_data, $
	     	      *selected_dataset.y_filtered_data, A, NTERMS=nterms )

     reduced_chi2 = 0.0
     chi2         = 0.0

     case Event.value of
	  ;----------------------------------------------------------------
          7: $
	  begin 
	   mean     = [A(1), 0]
	   sigma    = [A(2), 0]
	   area     = [A(0) * A(2) * SQRT(2*!PI), 0]
	   fit_name = "gaussian: "
	  end

	  ;----------------------------------------------------------------
          8: $
	  begin 
	   mean     = [A(1), 0]
	   sigma    = [A(2), 0]
	   area     = [A(0) * A(2) * SQRT(2*!PI), 0]
	   constant = [A(3), 0]
	   fit_name = "gaussian + constant: "
	  end

	  ;----------------------------------------------------------------
          9: $
	  begin 
	   mean     = [A(1), 0]
	   sigma    = [A(2), 0]
	   area     = [A(0) * A(2) * SQRT(2*!PI), 0]
	   constant = [A(3), 0]
	   linear   = [A(4), 0]
	   fit_name = "gaussian + linear: "
	  end

	  ;----------------------------------------------------------------
          10: $
	  begin 
	   mean     = [A(1), 0]
	   sigma    = [A(2), 0]
	   area     = [A(0) * A(2) * SQRT(2*!PI), 0]
	   constant = [A(3), 0]
	   linear   = [A(4), 0]
	   quadratic= [A(5), 0]
	   fit_name = "gaussian + quadratic: "
	  end

     endcase

   endelse ;error_code EQ 0

   catch, /CANCEL
        
 endif else if (12 LE event.value AND event.value LE 13) then begin
   ; POLYNOMIAL FIT
   ; Establish an error handler for exceptions thrown in fitting routines.
   catch, error_code

   if (error_code NE 0) then begin
     dum=dialog_message(['ERROR!',!ERR_STRING],DIALOG_PAR=(*st).fit_menu,/ERR)
     fit_done = 0
   endif else begin 
     fit_done = 1
     y_fit = 1
     chi2  = 0

     nterms = -10 + event.value

     if (num_data_points LE nterms) then message, /NONAME, 'Too few data points'

     if (weighted_fit) then begin
       A = svdfit(*selected_dataset.x_filtered_data, $
	     	  *selected_dataset.y_filtered_data, nterms, WEIGHT=weight, $
		  CHISQ=chi2, SIGMA=error, YFIT=y_fit)
       reduced_chi2 = chi2 / (num_data_points - nterms)
     endif else begin
       A = svdfit(*selected_dataset.x_filtered_data, $
	     	  *selected_dataset.y_filtered_data, nterms, $
	     	  CHISQ=chi2, SIGMA=error, YFIT=y_fit)
     endelse

     case Event.value of
      ;----------------------------------------------------------------
      12: $
      begin 
      constant = [A(0), error[0]]
      linear   = [A(1), error[1]]
      fit_name = "linear: "
      end

      ;----------------------------------------------------------------
      13: $
      begin 
      constant = [A(0), error[0]]
      linear   = [A(1), error[1]]
      quadratic= [A(2), error[2]]
      fit_name = "quadratic: "
      end
     endcase
   endelse ;error_code EQ 0

   catch, /CANCEL
 endif
 
 ;; Report the results of the fit.
if (fit_done) then begin
 if (NOT weighted_fit) then $
    dummy=dialog_message(['A NON-weighted fit was done because', $
		       'data points with standard deviation of ZERO', $
		       'were found.'], DIALOG_PARENT=top_base)

   msg = 'Fit parameters (and their errors):'
  
   if  (event.value LT 11) then begin
     mfmt='("area under gaussian =",G11.5," (",G10.4,")"/15x,"mean =",G11.5," (",G10.4,")"/14x,"sigma =",G11.5," (",G10.4,")")'
     msg = [msg,string( area, mean, sigma, f=mfmt )]

     nfmt = '(3(G11.5,:,","))'
     fit_name = fit_name + string( area[0], mean[0], sigma[0], f=nfmt )
   endif
   
   mfmt = '(A19," =",G11.5," (",G10.4,")")'
   nfmt = '(",",G11.5)'

   if (n_elements(constant) NE 0) then begin
         msg = [msg, string( "constant term", constant, f=mfmt )]
         fit_name = fit_name + string( constant[0], f=nfmt ) 
   endif

   if (n_elements(linear) NE 0) then begin
         msg = [msg, string( "linear term", linear, f=mfmt )]
         fit_name = fit_name + string( linear[0], f=nfmt ) 
   endif

   if (n_elements(quadratic) NE 0) then begin
         msg = [msg, string( "quadratic term", quadratic, f=mfmt )]
         fit_name = fit_name + string( quadratic[0], f=nfmt ) 
   endif

   if (weighted_fit) then begin
         msg = [msg, string(reduced_chi2,f='(6x,"reduced chi^2 =",G11.5)')]
   endif else begin
         msg = [msg, string(chi2,f='(10x,"chi^2=",G11.5)')]
   endelse

   TimedMessage, msg_id, msg, TITLE='Fit Results', GROUP=top_base, POS=top_base 
   print
   print, msg
   tara_clipboard, POST=msg
   
   fit_name = strcompress( fit_name )

  name = string((*st).fitnum, f='("fit",I0,": ")') + selected_dataset.name
  (*st).fitnum = (*st).fitnum + 1
  function_1d, top_base, *selected_dataset.x_filtered_data, y_fit, $
  		DATASET_NAME=name, COLOR='green', /NO_SELECT,$
		DESCRIPTION=fit_name, LINE=1
 endif ;fit_done
 end

;--------------------------------------------------------------------------
; File menu
  (*st).file_menu: $
   begin
   case (Event.value) of
    1: $ ; PRINT
     begin
     plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis
     asp_r = yaxis.num_pixels / float(xaxis.num_pixels)
     PsDevice, top_base, filename, success, CONFIG=*(*st).ps_config, BATCH_MODE=(*st).print_now, $
		XMARGIN=xaxis.margin, YMARGIN=yaxis.margin, ASPECT_RATIO=asp_r

     (*st).print_now = 0
     
     if (success EQ 1 ) then begin
       RedrawFunction1d, st
       device, /CLOSE
       color_manager, /X_PSEUDO
  
       if (Filename EQ '') then begin
         print_file
         widget_control, (*st).msg_label, SET_VALUE='Printed plot'
       endif else begin
         widget_control, (*st).msg_label, $
         		 SET_VALUE='Wrote Postscript file ' + filename
       endelse
     endif 
     end
     
    2: $ ; SAVE FUNCTION as FITS
     begin
     name = strcompress( selected_dataset.name + '.fits', /REMOVE_ALL )
     pathname = dialog_pickfile( GROUP=top_base, FILE=name(0), $
				 TITLE='Save Function (FITS)' )
     widget_control, /HOURGLASS
     
     if (pathname NE '') then begin
       ; Make sure we can write to this file.
       openw, Unit, pathname, /GET_LUN, ERROR=Error
       if (Error NE 0) then begin
         message = 'ERROR opening file '+ pathname
       endif else begin
         free_lun, Unit
                             
         ; Make up a primary FITS header.
         get_date, date_today
         mkhdr, pheader, '', /EXTEND
         sxaddpar, pheader, "CREATOR", "function_1d, $Revision: 4375 $"
	 sxaddpar, pheader, 'ORIGIN',   'Penn State University'
	 sxaddpar, pheader, "FITSVERS", "IDL Astronomy Users Library"
	 fdecomp, pathname, disk, item_path, item_name, item_qual
	 if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
	 sxaddpar, pheader, "FNFITS", item_name
	 sxaddpar, pheader, 'DATE',     date_today
	 writefits, pathname, no_data, pheader
	 
         sxaddpar, fits_header, "EXTNAME", selected_dataset.name
         sxaddpar, fits_header, "CREATOR", "function_1d, $Revision: 4375 $"
	 sxaddpar, fits_header, 'ORIGIN',   'Penn State University'
	 sxaddpar, fits_header, 'DATE',     date_today
	 
	 plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis
	 sxaddpar, fits_header, 'XTITLE', xaxis.title  
	 sxaddpar, fits_header, 'YTITLE', yaxis.title  
	 
	 if (selected_dataset.x_error_available && selected_dataset.y_error_available) then begin
	   bin_table = replicate( {X: 0.0, XERROR:0.0, Y:0.0, YERROR:0.0}, selected_dataset.num_data_points )
	 endif else if (selected_dataset.y_error_available) then begin
	   bin_table = replicate( {X: 0.0,             Y:0.0, YERROR:0.0}, selected_dataset.num_data_points )
	 endif else begin
	   bin_table = replicate( {X: 0.0,             Y:0.0            }, selected_dataset.num_data_points )
	 endelse

         bin_table.X        = *selected_dataset.x_filtered_data
         bin_table.Y        = *selected_dataset.y_filtered_data
	 if (selected_dataset.x_error_available) then $         
           bin_table.XERROR = *selected_dataset.x_filtered_error
	 if (selected_dataset.y_error_available) then $         
           bin_table.YERROR = *selected_dataset.y_filtered_error
         
         mwrfits, bin_table, pathname, fits_header            
       endelse ; (Error EQ 0)
     endif ; (pathname NE '')
     end
          
    3: $ ; SAVE FUNCTION as ASCII
     begin
     name = strcompress( selected_dataset.name + '.txt', /REMOVE_ALL )
     pathname = dialog_pickfile( GROUP=top_base, FILE=name(0), $
				 TITLE='Save Function (ASCII)' )
     widget_control, /HOURGLASS
     
     if (pathname NE '') then begin
       ; Make sure we can write to this file.
       openw, Unit, pathname, /GET_LUN, ERROR=Error
       if (Error NE 0) then begin
         message = 'ERROR opening file '+ pathname
       endif else begin

	 if (selected_dataset.x_error_available && selected_dataset.y_error_available) then begin
	   printf, unit, '        X          Y         XERROR      YERROR'
	   for ii = 0L, selected_dataset.num_data_points-1 do $
	     printf, Unit, (*selected_dataset.x_filtered_data )[ii], $
                     (*selected_dataset.y_filtered_data )[ii], $
                     (*selected_dataset.x_filtered_error)[ii], $
                     (*selected_dataset.y_filtered_error)[ii]   
	 
   endif else if (selected_dataset.y_error_available) then begin
	   printf, unit, '        X          Y         YERROR'
	   for ii = 0L, selected_dataset.num_data_points-1 do $
	     printf, Unit, (*selected_dataset.x_filtered_data )[ii], $
                     (*selected_dataset.y_filtered_data )[ii], $
                     (*selected_dataset.y_filtered_error)[ii]   
           
	 endif else begin
	   printf, unit, '        X          Y'
	   for ii = 0L, selected_dataset.num_data_points-1 do $
	     printf, Unit, (*selected_dataset.x_filtered_data )[ii], $
                     (*selected_dataset.y_filtered_data )[ii]
	 endelse
         
         free_lun, Unit
       endelse ; (Error EQ 0)
     endif ; (pathname NE '')
     end
          
    4: $ ; LOAD FUNCTION
     begin
     txt=['If you choose a FITS FILE, then its first extension must be',$
          'a binary table.  The first two columns of the table are assumed',$
          'to contain X and f(X).  If a third column is present it is',$
          'assumed to contain the error (standard deviation) on f(X).','',$
          'If you choose a ASCII FILE, then you will be presented with',$
          'the ASCII_TEMPLATE tool built into IDL.  Fill out the',$
   	  'three forms in this tool so that the first field extracted',$
   	  'contains the X data, the second field contains the f(X) data',$
   	  'and the optional third field contains the error on f(X).']
     TimedMessage, msg_id, txt, GROUP=top_base
     
     pathname = dialog_pickfile( GROUP=top_base, /MUST_EXIST, $
			       TITLE='Choose file (FITS table or ASCII)' )

     if (pathname NE '') then begin
       widget_control, /HOURGLASS

       ;; First, open the file and figure out if it is a FITS file by looking 
       ;; for a first line  that starts with SIMPLE.
       openr, unit, pathname, /GET_LUN
       stat = fstat(unit)
       first_line = bytarr( 8 < stat.size )
       readu, unit, first_line
       free_lun, unit
   
       fits_flag = (string(first_line) EQ 'SIMPLE  ')
   
       if (fits_flag) then begin
         table = mrdfits( pathname, 1, header )
         
       endif else begin
         table = read_ascii( pathname, TEMPLATE=ascii_template(pathname) )
       endelse
       
       fdecomp, pathname, disk, item_path, item_name, item_qual
       if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
   
       ;Store the data in a new dataset.
       if (n_tags(table) GT 2) then begin
         function_1d, top_base, table.(0), table.(1), DATASET_NAME=item_name,$
         		  ERROR=table.(2)
       endif else begin
         function_1d, top_base, table.(0), table.(1), DATASET_NAME=item_name
       endelse
     endif
     widget_control, msg_id, /DESTROY, BAD_ID=bad
     end

    5: $ ; DELETE FUNCTION
     begin
     ;; We do NOT let the user delete the last dataset.
     dum = where( (*st).datasets.name NE '', count )
     if ((count NE 1) AND (NOT selected_dataset.disable_delete)) then begin
       (*st).datasets[ selected_index ].name = ''
       
       ; Select the dataset just prior to the deleted one.
       (*st).selected_name = (*st).datasets[0 > (selected_index-1)].name
       (*st).stale_titles = 1
       RedrawFunction1d, st
     endif
     end

    6: $ ; HIDE/SHOW FUNCTIONS
     begin
     ;; Find the non-null datasets and update the dataset_list droplist widget.
     index = where( (*st).datasets.name NE '', count )
     
     datasets = (*st).datasets[index]
     
     name_list = escape_commas( string(datasets.name, F='(99(A,:,"|"))') )
     bgroup = '0,BUTTON,' + name_list + ',TAG=bgroup'
     form = ['1,BASE,,COLUMN', bgroup, '2,BASE']

     ;; Run the form widget.
     title = '0,LABEL, Function Visibility'
     ok = '0, BUTTON, OK, QUIT'
 
     form_base = widget_base(TITLE='function_1d')
     form      = cw_form(form_base, [title,form,ok], /COLUMN)
     widget_control, form_base, /REALIZE
     widget_control, form, GET_VALUE=form_st
     form_st.bgroup = (datasets.hidden_flag EQ 0)
     widget_control, form, SET_VALUE=form_st
     
     repeat begin
       event = widget_event(form)
     endrep until event.quit
     
     widget_control, form, GET_VALUE=form_st
     widget_control, form_base, /DESTROY
     
     ;; Extract the results
     datasets.hidden_flag = (form_st.bgroup EQ 0)
     for ii=0,count-1 do Save1dFunction, datasets[ii], st
          
     RedrawFunction1d, st
     end

    else: DestroyFlag = 1 ; EXIT button
  endcase
  end
   

;--------------------------------------------------------------------------
; Edit button: $
  (*st).edit_button: $
   begin
   ; DATASET PROPERTIES
   b0 = '1, BASE,, ROW, FRAME'
   
   l0 = '0, LABEL, SELECTED FUNCTION:'
   
   f1 = '0, TEXT,' + escape_commas( selected_dataset.name ) + $
	', TAG=name, LABEL_LEFT=  Name, WIDTH=15'

   str = (selected_dataset.name EQ selected_dataset.description) ? $
   	 '' : selected_dataset.description
   f2 = '0, TEXT,' + escape_commas( str ) + $
	', TAG=description, LABEL_LEFT=  Description, WIDTH=15'

   f3 = '2, DROPLIST, Show Dataset|Hide Dataset, SET_VALUE=' + $
   	string(selected_dataset.hidden_flag) + ', TAG=hidden_flag'

   form = [b0,l0,f1,f2,f3]
   

   ; NOTE PROPERTIES
   b0 = '1, BASE,, ROW, FRAME'

   l0 = '0, LABEL, ANNOTATION:'

   f1 = '0, TEXT,' + escape_commas( (*st).note ) + $
        ', TAG=note, WIDTH=15, LABEL_LEFT=  Text'

   f2 = '0, FLOAT,' + string((*st).note_x) + $
	 ', TAG=note_x, LABEL_LEFT=  X, WIDTH=8'

   f3 = '0, FLOAT,' + string((*st).note_y) + $
	 ', TAG=note_y, LABEL_LEFT=Y, WIDTH=8'
    
   f4 = '2, TEXT,' + escape_commas( (*st).annotation_routine ) + $
        ', TAG=annotation_routine, WIDTH=15, LABEL_LEFT=  Routine'

   form = [form,b0,l0,f1,f2,f3,f4]

   
   ; FILTER PROPERTIES
   b0 = '1, BASE,, ROW, FRAME'

   l0 = '0, LABEL, ROI:'
   
   f2 = '0, DROPLIST, Hide ROI|Show ROI, SET_VALUE=' + $
   	string((*st).show_roi) + ', TAG=show_roi'
   	
   f3 = '0, FLOAT,' + string((*st).roi_params.xl) + $
	 ', TAG=xl, LABEL_LEFT=Min, WIDTH=8'
	 
   f4 = '2, FLOAT,' + string((*st).roi_params.xh) + $
	 ', TAG=xh, LABEL_LEFT=Max, WIDTH=8'

   form = [form,b0,l0,f2,f3,f4]
      
   
   ;  PLOT PROPERTIES
    b0 = '1, BASE,, ROW, FRAME'
    
    l0 = '0, LABEL, PLOT PROPERTIES:'
    
    psym = selected_dataset.plot.psym
    
    f1 = '0, BUTTON, ' + $
         'None|+ Symbol|Asterisk|Dot|Diamond|Triangle|Box|X Symbol|user-defined|Histogram,'+$
	 'EXCLUSIVE, SET_VALUE=' + string(psym<9) + $
	 ', TAG=psym, LABEL_TOP=Symbol Style'

    f1b= '0, TEXT,' + escape_commas( selected_dataset.plot.plotsym ) + $
	 ', TAG=plotsym, LABEL_TOP=plotsym params, WIDTH=15'

    f4 = '0, BUTTON, ' + $
         'Solid|Dotted|Dashed|Dash Dot|Dash Dot Dot|Long Dashes|No Line,'+$
	 'EXCLUSIVE, SET_VALUE=' + string(selected_dataset.plot.line) + $
	 ', TAG=line, LABEL_TOP=Line Style'

    ; We have to translate the field "color", which is a string, into
    ; an index into the list of all available colors.
    color_manager, COLOR_NAMES=color_names
    color_index = 0 > (where(selected_dataset.plot.color EQ color_names))[0]
    color_list  = string(color_names, F='(99(A,:,"|"))' )
 
    f2 = '0, BUTTON,' + color_list + ',EXCLUSIVE, SET_VALUE=' +$
 	  string(color_index) + ', TAG=color, LABEL_TOP=Color'

    b1 = '1, BASE,, COLUMN, FRAME'

    f5 = '0, DROPLIST, Omit Errors|Show Errors, SET_VALUE=' + $
   	string(selected_dataset.plot.show_errors) + ', TAG=show_errors'
   	
    f6 = '2, INTEGER,' + string(selected_dataset.plot.nskip_errors) + $
	 ', TAG=nskip_errors, LABEL_LEFT=NSKIP, WIDTH=3'
	 
    f3 = '2, DROPLIST, Omit|Left|Center|Right, SET_VALUE=' + $
   	string((*st).legend_style) + ', LABEL_LEFT=Legend, TAG=legend_style'
   	
     form = [form,b0,l0,f1,f1b,f4,f2,b1,f5,f6,f3]
    

  ;; Run the form widget.
  f1 = '2,BUTTON,OK,QUIT,TAG=ok'

  r = cw_form( [form,f1], /COLUMN, TITLE='Properties' )
 
 
  ;; Extract the results
  selected_dataset.name        = r.name
  selected_dataset.description = r.description ? r.description : r.name
  selected_dataset.hidden_flag = r.hidden_flag
  (*st).stale_titles = 1
  (*st).selected_name= r.name
  
  (*st).note = r.note
  (*st).note_x = r.note_x
  (*st).note_y = r.note_y
  (*st).annotation_routine = r.annotation_routine
  
  (*st).show_roi = r.show_roi
  rp = (*st).roi_params

  (*st).roi_params.xl = r.xl
  (*st).roi_params.xh = r.xh
  
  ; If we've changed the ROI, then mark ALL the datasets as stale.
  if ((compare_struct(rp, (*st).roi_params))[0].ndiff GT 0) then begin
    (*st).datasets.stats_stale  = 1
    selected_dataset.stats_stale= 1
   
    if (widget_info( (*st).roi_mode, /DROPLIST_SELECT ) EQ 2) then begin
      (*st).datasets.filter_stale = 1
      selected_dataset.filter_stale=1
      new_event = { ID:top_base, TOP:event.top, HANDLER:0L}
    endif
  endif

  
  selected_dataset.plot.show_errors  = r.show_errors  
  selected_dataset.plot.nskip_errors = r.nskip_errors > 1

   ; Handle the case where the user selects both "no line" and "no symbol".
   if (r.line EQ 6 AND r.psym EQ 0) then r.psym = 2
   
   r.psym = (r.psym EQ 9) ? 10 : r.psym

   selected_dataset.plot.psym = r.psym
   selected_dataset.plot.plotsym = r.plotsym
   selected_dataset.plot.line = r.line
      
   ; We have to translate "result.color", which is an index into
   ; the corresponding color name (string).
   selected_dataset.plot.color = color_names[r.color]
   
   (*st).legend_style = r.legend_style
  
   ; Save the dataset structure we've modified.
   Save1dFunction, selected_dataset, st
   RedrawFunction1d, st
   end




;--------------------------------------------------------------------------
; Stats/Filter droplist
  (*st).roi_mode: $
   begin
   ; Mark ALL the datasets as stale.
   (*st).datasets.filter_stale = 1B
   (*st).datasets.stats_stale  = 1B
   new_event = { ID:top_base, TOP:event.top, HANDLER:0L}
   RedrawFunction1d, st
   end


;--------------------------------------------------------------------------
; Filter Define button
  (*st).roi_define: $
   begin
   ; Use the plot_window's markers to define a ROI.
   plot_window, (*st).pw_id, BIG_MARKER=big_marker, SMALL_MARKER=small_marker

    (*st).roi_params.xl = big_marker[0] < small_marker[0]
    (*st).roi_params.xh = big_marker[0] > small_marker[0]
   
   ; Mark ALL the datasets as stale.
   (*st).datasets.stats_stale  = 1B
   
   if (widget_info( (*st).roi_mode, /DROPLIST_SELECT ) EQ 2) then begin
     (*st).datasets.filter_stale = 1B
     new_event = { ID:top_base, TOP:event.top, HANDLER:0L}
   endif

   RedrawFunction1d, st
   end


;--------------------------------------------------------------------------
; Filter mode droplist
  (*st).roi_semantics: $
   begin
   ; Mark ALL the datasets as stale.
   (*st).datasets.stats_stale  = 1B
   
   if (widget_info( (*st).roi_mode, /DROPLIST_SELECT ) EQ 2) then begin
     (*st).datasets.filter_stale = 1B
     new_event = { ID:top_base, TOP:event.top, HANDLER:0L}
   endif

   RedrawFunction1d, st
   end


  else: print, 'unknown event in function_1d'
endcase


if DestroyFlag then widget_control, (*st).parent, /DESTROY

return, new_event
end



;==========================================================================
;;; Event Handler Procedure
;==========================================================================
PRO Function1dEvent, Event
 
event = Function1dEventFn( Event )
return
end


;==========================================================================
;;; MAIN "function_1d" ROUTINE
;==========================================================================

PRO function_1d, top_base, x_data, X_ERROR=x_error, y_data, Y_ERROR=y_error, ERROR=error, $
		 TITLE=title,   SUBTITLE=subtitle, $
		 XTITLE=xtitle, YTITLE=ytitle, UNITY_ASPECT=unity_aspect, LEGEND_STYLE=legend_style, $
		 XWCS=xwcs, YWCS=ywcs, $
		 NO_SELECT=no_select, $
		 DATASET_NAME=dataset_name, DESCRIPTION=description, $
		 HIDE_DATASET=hide_dataset, DISABLE_DELETE=disable_delete, $
		 COLOR=color, LINESTYLE=linestyle, PSYM=psym, PLOTSYM=plotsym, NSKIP_ERRORS=nskip_errors, $
		 DELETE=delete,  ROI_MASK=mask, REDRAW=redraw, $
     PLOT_WINDOW_OPTIONS=plot_window_options, PS_CONFIG=ps_config, PRINT=print_now, $
		 _EXTRA=extra

		 
;; If the widget ID of an existing function_1d was not passed, then create 
;; the widget.
create_flag = 1
if (n_elements(top_base) EQ 1) then begin
  if (widget_info( top_base, /VALID_ID )) then create_flag = 0
endif

if (create_flag) then top_base = CreateFunction1d( _STRICT_EXTRA=extra )

;;---------------------------------------------------------------------------
;; Get the state structure.
widget_control, top_base, GET_UVALUE=st
redraw_flag = 0

;;---------------------------------------------------------------------------
;; Handle title keywords by passing on to plot_window.
if (0 NE n_elements(xwcs))     then plot_window, (*st).pw_id, XWCS=xwcs
if (0 NE n_elements(ywcs))     then plot_window, (*st).pw_id, YWCS=ywcs

x_param_name = (routine_names(x_data, ARG_NAME=(-1)))[0]
y_param_name = (routine_names(y_data, ARG_NAME=(-1)))[0]

; On the first call use the parameter names (if available) to name the axes
; if XTITLE, YTITLE not supplied.
if create_flag then begin
  if (0 EQ n_elements(xtitle)) then xtitle = keyword_set(x_param_name) ? x_param_name : 'X'
  if (0 EQ n_elements(ytitle)) then ytitle = keyword_set(y_param_name) ? y_param_name : 'f()'
endif

if (0 NE n_elements(xtitle))   then plot_window, (*st).pw_id, XTITLE=xtitle
if (0 NE n_elements(ytitle))   then begin
  (*st).ytitle=ytitle
  (*st).stale_titles = 1
endif

if (0 NE n_elements(subtitle)) then plot_window, (*st).pw_id, SUBTITLE=subtitle

if (0 NE n_elements(title))    then begin
  (*st).title=title
  (*st).stale_titles = 1

endif

if keyword_set(plot_window_options) then begin
  dum = execute('plot_window, (*st).pw_id,'+plot_window_options)
endif

if keyword_set(ps_config)          then *(*st).ps_config = ps_config

if (0 NE n_elements(legend_style)) then (*st).legend_style = legend_style


;; If we just created the widget and no data was passed, then we want to
;; return to avoid creating a dataset named "x data".
if (create_flag AND (N_ELEMENTS(x_data) EQ 0)) then return

;;---------------------------------------------------------------------------
;; Find the specified dataset or a spot to put a new one.
if keyword_set(dataset_name) then begin
  ; A name was passed.
endif else if (N_ELEMENTS(x_data) NE 0) then begin
  ; Data was passed, without a name; use a default name.
  if (keyword_set(x_param_name) AND keyword_set(y_param_name)) then $
    dataset_name = y_param_name+' vs. '+x_param_name $
  else $
    dataset_name = 'f() vs. X'
endif else begin
  ; Neither a name nor data were passed.  Assume the caller is refering to the last dataset.
  ind = where( (*st).datasets.name NE '', count )
  
  dataset_name = (count EQ 0) ? 'f() vs. X' : ((*st).datasets.name)[ind[count-1]]
endelse


  dataset_index = (where( (*st).datasets.name EQ dataset_name, count ))[0]
  
  new_dataset_flag = (count EQ 0)
  
  if new_dataset_flag then begin
    dataset_index = (where( (*st).datasets.name EQ '', count ))[0]
  
    ; If there's no empty spots, we'll overwrite the first dataset.
    if (count EQ 0) then dataset_index = 0
  endif
  
  dataset = (*st).datasets[dataset_index]
  
  
  ;;---------------------------------------------------------------------------
  ;; Store new X-Y data values if supplied.

  if (N_ELEMENTS(x_data) NE 0) then begin
  
    dataset.num_data_points = n_elements(x_data) 
    *dataset.x_data = x_data 
    *dataset.y_data = y_data 
    
    dataset.x_error_available = (n_elements(x_error) EQ dataset.num_data_points)
    if dataset.x_error_available then begin
      *dataset.x_error = x_error
    endif else if keyword_set(x_error) then print, dataset.num_data_points, n_elements(x_error), F='(%"ERROR: %d data points were supplied but the parameter X_ERROR has only %d elements.")'
        
    ; Backward compatibilty:
    if (n_elements(y_error) EQ 0) && (n_elements(error) NE 0) then y_error = error
    
    dataset.y_error_available = (n_elements(y_error) EQ dataset.num_data_points)
    if dataset.y_error_available then begin
      *dataset.y_error = y_error
      dataset.plot.show_errors = 1
    endif else if keyword_set(y_error) then print, dataset.num_data_points, n_elements(y_error), F='(%"ERROR: %d data points were supplied but the parameter y_ERROR has only %d elements.")'
        
    dataset.filter_stale = 1B
    dataset.stats_stale  = 1B
    redraw_flag = 1
  endif 
  
;;---------------------------------------------------------------------------
;; If this is a new dataset, then we want to compute default values for
;; several parameters.
if new_dataset_flag then begin
  dataset.name           	= dataset_name
  dataset.description     	= dataset_name
  dataset.hidden_flag     	= 0
  dataset.disable_delete 	= 0
  dataset.plot.psym		    = 0
  dataset.plot.plotsym	   	= ''
  
  (*st).stale_titles = 1
  redraw_flag = 1
endif
  
  
  ;;---------------------------------------------------------------------------
  ;; We delete a dataset merely by setting its name to ''.
  if keyword_set(delete) then begin
    dataset.name = ''
    *dataset.x_data = [0]
    dataset.num_data_points = 1
    redraw_flag = 1
  endif
  
  
  ;;---------------------------------------------------------------------------
  ;; Handle the keywords that modify a dataset.
  if (0 NE n_elements(description))    then dataset.description     = description
  if (0 NE n_elements(color))          then dataset.plot.color      = color
  if (0 NE n_elements(linestyle))      then dataset.plot.line       = linestyle
  if (0 NE n_elements(psym))           then dataset.plot.psym       = psym
  if (0 NE n_elements(plotsym))        then dataset.plot.plotsym    = plotsym
  if (0 NE n_elements(nskip_errors))   then dataset.plot.nskip_errors = nskip_errors
  if (0 NE n_elements(disable_delete)) then dataset.disable_delete  = disable_delete 

  
  

;;---------------------------------------------------------------------------
;; Decide if we should redraw by considering the new_dataset_flag, the 
;; existing state of dataset.hidden_flag and the keyword hide_dataset.
if (new_dataset_flag) then begin
  ; Dataset is new; is HIDE_DATASET specified?
  if keyword_set(hide_dataset) then begin
    dataset.hidden_flag = 1
    redraw_flag = 0
  endif else begin
    if (~keyword_set(no_select)) then (*st).selected_name = dataset.name
    if ((*st).legend_style EQ 0) && (dataset_index GT 0) then (*st).legend_style = 3
    redraw_flag = 1
  endelse
  
endif else begin
  ; Dataset is pre-existing; is the hidden_flag being changed?
  if (0 NE n_elements(hide_dataset)) then begin
    if (hide_dataset NE dataset.hidden_flag) then begin
      dataset.hidden_flag = hide_dataset
      redraw_flag = 1
    endif
  endif else begin
    ; Otherwise we just follow the hidden_flag.
    redraw_flag = (dataset.hidden_flag EQ 0)
  endelse
endelse

  
;;---------------------------------------------------------------------------
;; Compute and return the ROI mask if desired.
if arg_present(mask) then begin
  ApplyFunction1dRoi, dataset, st
  mask = dataset.mask
  redraw_flag = 0
endif


;;---------------------------------------------------------------------------
;; Store the dataset structure we've been modifying.
Save1dFunction, dataset, st


;;---------------------------------------------------------------------------
;; The REDRAW keyword overrides redraw_flag calculated above.
if (n_elements(redraw) EQ 1) then begin
  (*st).selected_name = dataset.name
  redraw_flag = redraw
endif

if (widget_info(top_base, /REALIZED) and redraw_flag) then RedrawFunction1d, st

;;---------------------------------------------------------------------------
;; Print without user intervention, if directed.
if keyword_set(print_now) then begin
  (*st).print_now = 1
  event={ID:(*st).file_menu, TOP:top_base, HANDLER:top_base, VALUE:1}
  Function1dEvent, Event
endif

return
END
