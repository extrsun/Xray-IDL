;+
;========================================================================
;;;
;;; Dataset_2D Widget: $Id: dataset_2d.pro 4411 2012-12-24 20:31:28Z psb6 $
;;;
;;; Copyright (C) 1998, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION:  
;;; This tool may be used to analyze the distribution of 2D datasets.
;;;
;========================================================================
;;; EVENTS GENERATED
;;; An event is forwarded to the client whenever the dataset is filtered.
;;;
;========================================================================
;;; INTERFACE TO CLIENT
;;;
;;; dataset_2d, top_base, x_data, y_data, WEIGHT=weight, $
;		 PARENT_WIDGET=parent, GROUP=group, BLOCK=block, $
;		 WIDGET_TITLE=widget_title,$
;                TITLE=title, $
;		 XTITLE=xtitle, YTITLE=ytitle, UNITY_ASPECT=unity_aspect, LEGEND_STYLE=legend_style, $
;		 XWCS=xwcs, YWCS=ywcs, NAN_VALUES=nanval, $
;	   
;		 DATASET_NAME=dataset_name, DESCRIPTION=description, $
;		 COLOR=color, PSYM=psym, PLOTSYM=plotsym, $
;		 XBIN=xbin, YBIN=ybin, XEDGE=xedge, YEDGE=yedge, $
;		 HIDE_DATASET=hide_dataset, DISABLE_DELETE=disable_delete, $
;		 DELETE=delete,  ROI_MASK=mask, REDRAW=redraw, $
;		 GET_DATASETS=get_datasets
;
;    PLOT_WINDOW_OPTIONS=plot_window_options, PS_CONFIG=ps_config, PRINT=print_now
;;;
;;; The parameters PARENT_WIDGET & GROUP are the usual ones associated 
;;; with widgets.  If dataset_2d is a top-level widget, then WIDGET_TITLE
;;; controls the title in the X-window frame.
;;;
;;; The keyword WEIGHT is used to supply a weight to each (x,y) datapoint.
;;; For example a datapoint with a weight of 2 has the same effect on the
;;; estimated density function as two datapoints at that location.
;;;
;;; The dataset_2d can work with multiple named datasets.
;;; Multiple datasets are passed with multiple calls to dataset_2d -- see
;;; example below.
;;; A dataset may be deleted by the caller using /DELETE.

;;; PSYM can have the values 1 (+), 2 (*), 3 (.), 
;;; 4 (diamond), 5 (triangle), 6 (box), 7 (X), 
;;; 8 (defined by plotsym.pro; see below).
;;;
;;; PLOTSYM is a string containing the parameters passed to plotsym.pro (AstroLib).  
;;; The most simple PLOTSYM would be just a string containing a number from 0...8, e.g. 
;;; PLOTSYM='4'.  A more complex example is PLOTSYM='7, 1.3, THICK=2'
;;; Pass PSYM=8 when using PLOTSYM.

;;; COLOR can have the values 'red', 'blue', 'green', or 'white'.

;;; The state of this widget is stored as an anonymous structure in the 
;;; user value slot of the first child (the normal convention).


;==========================================================================
;;;                     CALLING OPTIONS

;;; From a non-widget program or the command line, analyze vectors X & Y.
;;; Widget events will be processed when the command line returns.
;;; ** dataset_2d, id, X, Y

;;; From a non-widget program analyze vectors X & Y but block further
;;; program execution until the dataset_2d widget is destroyed.
;;; ** dataset_2d, id, X, Y, /BLOCK
;;; ** xmanager

;;; From a widget program, create a top-level dataset_2d.
;;; ** dataset_2d, id, X, Y, GROUP=creator_widget_id

;;; Create a dataset_2d as a child of another widget.
;;; ** dataset_2d, id, X, Y, PARENT_WIDGET=base

;;; Delete a dataset from an existing dataset_2d.
;;; ** dataset_2d, id, /DELETE, DATASET_NAME=name

;;; Obtain a pointer to a byte array that shows which data points in the 
;;; specified dataset are retained by the user's filter. If no filter
;;; is defined, then the pointer points to an undefined variable.
;;; ** dataset_2d, id, DATASET_NAME=name, ROI_MASK=mask
;==========================================================================
;-

;==========================================================================
;;; Create the widget.
;==========================================================================
FUNCTION CreateDataset2d, PARENT_WIDGET=parent, GROUP=group, BLOCK=block, $
			  WIDGET_TITLE=widget_title

;; Call color_manager to switch to DirectColor if available.
color_manager

;; we have to create a dummy one here so the /CLOSE later won't die.
xinteranimate, SET=[100,100,2]

;; Default values
if (0 EQ n_elements(group))          then group = 0
if (0 EQ n_elements(widget_title))   then widget_title = 'Dataset_2D'
  

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

this_is_top_level = (0 EQ n_elements(parent))
if this_is_top_level then begin
 center  = ScreenCenter()
 xoffset = center(0) - 400
 yoffset = center(1) - 250
 parent = plot_window_topbase(TITLE=widget_title,GROUP_LEADER=group, $
                      XOFFSET=xoffset, YOFFSET=yoffset)
endif

top_base = widget_base( parent, /BASE_ALIGN_CENTER, $
			FRAME=(this_is_top_level EQ 0), $
			EVENT_FUNC='Dataset2dEventFn', $
			KILL_NOTIFY='Dataset2dCleanup', $
			/COLUMN, /SPACE, XPAD=0, YPAD=0, UNAME='dataset_2d' )

upper_base = widget_base( top_base, /ALIGN_LEFT, /ROW, $
			   SPACE=8, XPAD=0, YPAD=0 )
			   
 menu = [{ CW_PDMENU_S, flags:1, name:'File' }, $ 
	 { CW_PDMENU_S,     0,        'Print' }, $ 
	 { CW_PDMENU_S,     0,        'Save Density Function (FITS Image)' },$
	 { CW_PDMENU_S,   0,      'Save 2-D Dataset (FITS Table)' }, $
	 { CW_PDMENU_S,   0,      'Load 2-D Dataset (FITS Table or ASCII)' }, $
	 { CW_PDMENU_S,     2,        'Delete Dataset' }]

 if this_is_top_level then begin
   menu[5].flags = 0
   menu = [menu, { CW_PDMENU_S,     2,      'Exit' }]
 endif 
 
 file_menu = cw_pdmenu(upper_base, menu, /RETURN_INDEX)
     
 edit_button    = widget_button( upper_base, VALUE='Edit' )
 
 roi_base = widget_base(upper_base, /ROW, /SPACE, XPAD=0, YPAD=0, /FRAME)
 
   roi_mode = widget_droplist( roi_base, VALUE=['None','Stats','Filter'], $
   				  TITLE='ROI:' )
   
   styles = ['Box','X-range','Y-range','Annulus']
   roi_style = widget_droplist( roi_base, VALUE=styles )
  
   roi_define = widget_button( roi_base, VALUE='Use Markers' )
  
   roi_semantics = widget_droplist( roi_base, VALUE=['Include','Exclude'] )

 menu = [{ CW_PDMENU_S, flags:1, name:'Analysis' }, $ 
	 { CW_PDMENU_S,     0,   'Mark Centroid' }, $ 
	 { CW_PDMENU_S,     0,   'Radial Profile from Big Marker' }, $ 
	 { CW_PDMENU_S,     0,   'Distribution of Image Pixels' },$ 
	 { CW_PDMENU_S,     0,   'Distribution of X Data' },$ 
	 { CW_PDMENU_S,     0,   'Distribution of Y Data' },$ 
	 { CW_PDMENU_S,     0,   'Cuts Through Big Marker' },$ 
	 { CW_PDMENU_S,     0,   'Linear Regression' },$ 
	 { CW_PDMENU_S,     0,   'Linear Regression with sigma-clipping' },$ 
	 { CW_PDMENU_S,0,   'Combine Images (Difference, Ratio, RGB, etc.)' },$ 
	 { CW_PDMENU_S,     2,   'Blink Datasets' }]
	 
 derived_menu = cw_pdmenu(upper_base, menu, /RETURN_INDEX)


middle_base = widget_base( top_base, /ALIGN_LEFT, /ROW, $
			   /SPACE, XPAD=0, YPAD=0 )

 dataset_list = widget_droplist( middle_base, VALUE='NULL', /DYNAMIC_RESIZE )
 
 modes = ['Scatter Plot', 'Image', 'Surface Plot', 'Contour Plot', $
 	  'Contour on Image']
 mode_list = widget_droplist( middle_base, VALUE=modes )
 widget_control, mode_list, SET_DROPLIST_SELECT=1

 scaling_button = widget_button( middle_base, VALUE='Image Scaling  ' )

 color_table_button = widget_button( middle_base, VALUE='Color Table' )

msg_label = widget_label( top_base, /DYNAMIC_RESIZE, VALUE=' ' )
    
plot_window, pw_id, PARENT=top_base
plot_window, pw_id, SET_XMARGIN=[8,5]

; Setup state structure.
dataset = { index:0B, name:'', description:'', hidden_flag:0, disable_delete:0,$

	    x_data:ptr_new(), y_data:ptr_new(), $
	    w_data:ptr_new(), weight_available:0, $
	    
	    x_filtered_data:ptr_new(), y_filtered_data:ptr_new(), $
	    w_filtered_data:ptr_new(), $
	    mask:ptr_new(), filter_stale:1B, $
	    ; Stats and centroid (xcen,ycen) are for ROI.
	    stats:'', stats_stale:1B, xcen:0.0, ycen:0.0, $
	    
	    ; num_data_points, x_max, x_min, etc. refer to the filtered dataset!
	    num_data_points:0L, y_max:1.0, y_min:0.0, x_max:1.0, x_min:0.0, $

	    plot: {color:'white', psym:3B, plotsym:''}, $
	    
	    binning:   {desired_delta_x:0.0, desired_delta_y:0.0, $
	    		delta_x:0.0, delta_y:0.0, $
	    		xbin_location:0.0, ybin_location:0.0},$


	    		;;0=histogram, 1=Epanechnikov kernel, 2=Gaussian kernel
	    		;;Epanechnikov_width must be EVEN
	    		;;Gaussian sigma is in units of bins
	    density:   {kernel:0, Epanechnikov_width:4, Gaussian_sigma:1.0, $ 
	    		bandwidth_x:1.0, bandwidth_y:1.0, $
	    		subtitle:'', $
	    		x0:0.0, y0:0.0, xdim:0L, ydim:0L, $
	    		samples:ptr_new(), stale:0B }, $
	    		
	    image:ptr_new(), log_flag:0, $
	    bgrnd_index:ptr_new(), fgrnd_index:ptr_new(), $
	    scale_midpt:0.25, scale_width:0.5, scale_stale:0B, $
	    tvlow:0.0, tvhigh:1.0, tv_stale:1B}
	    
r2=[0.0,0.0]
state = { parent:parent,$ 
	;IDs of widgets that generate events or need to be updated. 
	roi_mode:roi_mode, roi_style:roi_style, $
	roi_define:roi_define, roi_semantics:roi_semantics,$
	show_roi:1, draft_mode:0, $
	
	dataset_list:dataset_list, $
	file_menu:file_menu, edit_button:edit_button, $
	derived_menu:derived_menu, $
	mode_list:mode_list, $
	scaling_button:scaling_button, color_table_button:color_table_button, $
	msg_label:msg_label, pw_id:pw_id, $
	
	;Dataset structures
	selected_name:'', datasets: replicate( dataset, 30 ), $
	
	;ROI parameters
	roi_params: {xl:1., xh:0., yl:1., yh:0., center:r2, $
		     rin:0., rout:-1., radii_lock:0}, $

	; FIT parameters
	fit: {name:'', number:0, x:ptr_new([0]), f_of_x:ptr_new([0])}, $
	
	;Other state information.
	surface_mode:0, x_axis_rotation:30, z_axis_rotation:30, $
	legend_style:0, color_bar:1, contour_kwds:'/FOLLOW', contour_regionfile:'', $
	share_binning_params:1, share_tv_params:1, $
	note:'', note_x:0.0, note_y:0.0, stale_titles:1, $
	title:'', $
	
	rsb_widget:0L, ee_widget:0L, theta_widget:0L, samples_widget:0L,$
	xcut_widget:0L, ycut_widget:0L, ps_config:ptr_new(/ALLOC), print_now:0B,$
        xdata_widget:0L, ydata_widget:0L, $
	image_widget:0L, rgb_widget:0L, hsv_widget:0L, $
	msg_widget:0L, $
	indexed_add_lib:'' }
	

;; Allocate heap variables in dataset structures.
color_manager, COLOR_NAMES=color_names  & nc = n_elements(color_names)-1
for ii = 0, n_elements(state.datasets)-1 do begin
  state.datasets[ii].index           = ii
  state.datasets[ii].x_data          = ptr_new([0])
  state.datasets[ii].y_data          = ptr_new([0])
  state.datasets[ii].w_data  	     = ptr_new([0])
  state.datasets[ii].x_filtered_data = ptr_new([0])
  state.datasets[ii].y_filtered_data = ptr_new([0])
  state.datasets[ii].w_filtered_data = ptr_new([0])
  state.datasets[ii].mask            = ptr_new(/ALLOC)
  state.datasets[ii].density.samples = ptr_new(/ALLOC)
  state.datasets[ii].image           = ptr_new(/ALLOC)
  state.datasets[ii].bgrnd_index     = ptr_new(/ALLOC)
  state.datasets[ii].fgrnd_index     = ptr_new(/ALLOC)

  state.datasets[ii].plot.color = color_names[ ii         mod nc]
endfor


;; Save state structure.
widget_control, top_base, SET_UVALUE=ptr_new(state, /NO_COPY)


;; If this is top-level widget, realize it and register.
if this_is_top_level then begin
  widget_control, parent, /REALIZE

  ; The fancy combination of JUST_REG and NO_BLOCK is necessary to get the
  ; widget to behave when called by either widget applications or normal
  ; programs (which block on tty reads).
  xmanager, 'dataset_2d', parent, GROUP_LEADER=group, $
	    JUST_REG=keyword_set(block), NO_BLOCK=(keyword_set(block) EQ 0), $
	    EVENT_HANDLER='PlotWindowTopbaseEventFn'
endif

return, top_base
END

;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO Dataset2dCleanup, top_base

widget_control, top_base, GET_UVALUE=st

;; Free the heap vars allocated locally.
ptr_free, (*st).datasets.x_data, (*st).datasets.y_data, (*st).datasets.w_data,$
          (*st).datasets.x_filtered_data, (*st).datasets.y_filtered_data, $
          (*st).datasets.w_filtered_data, $
          (*st).datasets.mask, $
          (*st).datasets.density.samples, (*st).datasets.image, $
          (*st).datasets.bgrnd_index, (*st).datasets.fgrnd_index, $
          (*st).fit.x , (*st).fit.f_of_x, (*st).ps_config
ptr_free, st
return
end


;==========================================================================
;;; Save a dataset structure back to the widget state structure.
;==========================================================================
PRO Save2dDataset, dataset, st
(*st).datasets[ dataset.index ] = dataset
return
end


;==========================================================================
;;; Routine to display a dataset image.  
;;; It is assumed that coordsystem_manager, /RESTORE has already been called.
;==========================================================================
PRO TvDataset2d, dataset, st, xaxis, yaxis, COLORBAR=colorbar, LOG=log, $
		 ERASE=erase, VERBOSE=verbose, SAVED_STATS=saved_stats

;; If tv_stale or scale_stale flag set, then recompute the nominal scaling range
;; for the currently displayed image (not the whole density array).
if (dataset.tv_stale OR dataset.scale_stale) then begin
  if keyword_set(saved_stats) then begin
    lowlimit=saved_stats[0] & highlimit =saved_stats[1] 
  endif else begin
    fgrnd_pixels = (*dataset.image)[*dataset.fgrnd_index]
    meanclip, congrid(fgrnd_pixels,10000), mean, sigma
    minval = min( fgrnd_pixels, MAX=maxval )
  
    lowlimit  = (mean - 5*sigma) > minval
    highlimit = (mean + 5*sigma) < maxval
    
    ; Make sure 90% of the image pixels fall in the nominal scaling range.
    dum  = where((lowlimit LT fgrnd_pixels) AND $
    		 (fgrnd_pixels LT highlimit), count )
    if (float(count)/n_elements(fgrnd_pixels) LT 0.90) then begin
      lowlimit = minval
      highlimit= maxval
    endif
    
    ; If low & high limits are the same, then expand them to the full range.
    if (lowlimit EQ highlimit) then begin
      lowlimit = minval
      if (maxval GT minval) then highlimit = maxval $
      			    else highlimit = minval + 1
    endif
        
    saved_stats = [lowlimit, highlimit]
  endelse
  
  range = (highlimit-lowlimit)

  ;; In the case where both tv_stale & scale_stale are set, we ignore tv_stale.
  ;; This occurs when the user changes tvlow or tvhigh on the Edit D-box 
  ;; (setting scale_stale) and they change something which causes the density
  ;; to be recomputed (setting tv_stale).
  if (dataset.scale_stale) then begin
    ; Recompute scale_midpt & scale_width from tvlow & tvhigh.
    dataset.scale_stale= 0
    dataset.scale_midpt = ((dataset.tvhigh+dataset.tvlow)/2 - lowlimit) / range
    dataset.scale_width =  (dataset.tvhigh-dataset.tvlow)               / range
  endif else begin
    ; Tv_stale set, so recompute tvlow & tvhigh from scale_midpt & scale_width.
    dataset.tv_stale= 0
    lowfrac        = (dataset.scale_midpt - dataset.scale_width/2.0)
    highfrac       = (dataset.scale_midpt + dataset.scale_width/2.0)
    dataset.tvlow  = limit_precision(lowlimit + lowfrac  * range, 5)  
    dataset.tvhigh = limit_precision(lowlimit + highfrac * range, 5)
  endelse
endif ;(dataset.tv_stale OR dataset.scale_stale)


tvlow  = dataset.tvlow  
tvhigh = dataset.tvhigh

if keyword_set(verbose) then begin
  msg = string(tvlow, tvhigh, $
  		f='("Scaling range: [",G10.4," to ",G10.4,"]")' )
  		
  widget_control, (*st).msg_label, SET_VALUE=msg
endif
    
color_manager, NCOLORS=ncolors, NEGATIVE_IMAGES=negative_images, BLACK=black
im = bytscl( *dataset.image, TOP=ncolors-1, MIN=tvlow, MAX=tvhigh )

if (size(*dataset.bgrnd_index, /N_DIM) EQ 1) then im[*dataset.bgrnd_index]=0B

; Show B&W PostScript as a negative image.
if (negative_images) then im = 255 - temporary(im)
tv, im, xaxis.image_position, yaxis.image_position, /DATA, $
	XSIZE=xaxis.image_size, YSIZE=yaxis.image_size

if keyword_set(colorbar) then begin
  width  = 0.02
  
  ; Size of colorbar in data units.
  xsize = xaxis.image_size * width 
  ysize = yaxis.image_size
  
  ; And size in device units.
  n_cols = ceil( xaxis.num_pixels * width )
  n_rows = yaxis.num_pixels

  im = tvlow + float(tvhigh-tvlow)/(n_rows-1)*findgen(n_rows)
  im = replicate(1.0,n_cols) # im
  im = bytscl( im, TOP=ncolors-1, MIN=tvlow, MAX=tvhigh )
  if (negative_images) then im = 255 - temporary(im)
  
  ; Position of the colorbar and labels in data units.
  x_bar_pos = xaxis.range[1] + 0.01 * (xaxis.range[1]-xaxis.range[0])
  y_bar_pos = yaxis.image_position
  char_size = (convert_coord(!D.X_CH_SIZE,!D.Y_CH_SIZE,/DEV,/TO_DATA) - $
  	       convert_coord(           0,           0,/DEV,/TO_DATA)) * 0.75
  x_label_pos = [x_bar_pos,x_bar_pos]
  y_label_pos = [y_bar_pos-1.1*char_size[1],y_bar_pos+ysize+0.2*char_size[1]]
  
  if keyword_set(erase) then begin
    mask = replicate(byte(black), !D.X_CH_SIZE * 10, !D.Y_CH_SIZE)
    tv, mask, x_label_pos[0], y_label_pos[0], /DATA
    tv, mask, x_label_pos[1], y_label_pos[1], /DATA
  endif
  
  tv, im, x_bar_pos, y_bar_pos, /DATA, XSIZE=xsize, YSIZE=ysize

  labels = strtrim(string([tvlow,tvhigh], F='(G10.3)'), 2)
  xyouts, x_label_pos, y_label_pos, labels, CHARSIZE=0.75*!P.CHARSIZE
  if keyword_set(log) then $
    xyouts, x_bar_pos+xsize+2*char_size[0], y_bar_pos+ysize/2.0, $
            'log scaling', ALIGN=0.5, ORIENT=90, CHARSIZE=0.75*!P.CHARSIZE
endif
return
end

;==========================================================================
;;; Routine to filter a dataset or compute ROI statistics.
;;; The structure 'dataset' is modified!
;==========================================================================
PRO ApplyDataset2dRoi, dataset, st

;; First, see if the filter has already been applied.
if (dataset.filter_stale EQ 0 AND dataset.stats_stale EQ 0) then return

area = 0
	
;; If there is no filter defined, then just point to full dataset and
;; set the mask to all 1's.
roi_mode  = widget_info( (*st).roi_mode, /DROPLIST_SELECT )
roi_style = widget_info( (*st).roi_style, /DROPLIST_SELECT )

if (roi_mode EQ 0 ) then begin
  x_roi_data = dataset.x_data
  y_roi_data = dataset.y_data
  w_roi_data = dataset.w_data
    
  mask = replicate( 1B, n_elements(*dataset.x_data) )
  
endif else begin

  ;; If we get here, then there is a ROI defined and we need to apply it.
  ;; Remember that the result  could be the empty set, represented by
  ;; x_roi_data & y_roi_data pointing to undefined heap vars.
  roi    = (*st).roi_params
  x_data = dataset.x_data
  y_data = dataset.y_data
  w_data = dataset.w_data
  
  ;; Initialize the ROI if it has never been assigned by user or if it is
  ;; null.
  if (roi.xl GT roi.xh) then begin
    roi.xh = max(*x_data, MIN=x_min, /NAN)
    roi.xl = x_min
  endif

  if (roi.yl GT roi.yh) then begin
    roi.yh = max(*y_data, MIN=y_min, /NAN)
    roi.yl = y_min
  endif

  if (roi.rin GT roi.rout) then begin
    x_max = max(*x_data, MIN=x_min, /NAN)
    y_max = max(*y_data, MIN=y_min, /NAN)
    roi.center = [(x_max+x_min)/2, (y_max+y_min)/2]
    roi.rin    = 0
    roi.rout   = max([(x_max-x_min)/2, (y_max-y_min)/2])
  endif

  
  case roi_style of
   1: $ ; X-range
    begin
    mask = (roi.xl LE *x_data AND roi.xh GE *x_data)
    end
  
   2: $ ; Y-range
    begin
    mask = (roi.yl LE *y_data AND roi.yh GE *y_data)
    end
  
   0: $ ; box
    begin
    mask = (roi.xl LE *x_data AND roi.xh GE *x_data AND $
  	    roi.yl LE *y_data AND roi.yh GE *y_data)
    area = (roi.xh - roi.xl) * (roi.yh - roi.yl)
    end
  
   3: $ ; annulus
    begin
    x_center=roi.center(0) &  y_center=roi.center(1)  
    
    sqr_dist = (*x_data - x_center)^2 + (*y_data - y_center)^2
    mask = (roi.rin^2 LE sqr_dist AND roi.rout^2 GE sqr_dist)
    area = !PI * roi.rout^2 - !PI * roi.rin^2

    ; Warn the user they should dither integer-valued data.
    if (size(*x_data, /TYPE) LE 3 OR size(*y_data, /TYPE) LE 3) AND $
       ((roi.rin GT 0 AND roi.rin LT 5) OR (roi.rout LT 5)) then begin
      msg=['WARNING! A small circular ROI applied to integer data can give',$
           'misleading results.  You should consider dithering your data.']
      print, msg
    endif

    end
  endcase
  
  (*st).roi_params = roi

  ;; Invert the mask if exclude mode is chosen.
  if (1 EQ widget_info( (*st).roi_semantics, /DROPLIST_SELECT )) then begin
    mask = (mask EQ 0)
    area = 0
  endif
  
  ;; Compute the datapoints that are in the ROI,
  index = where( mask, count )

  if (count EQ 0) then begin
    x_roi_data = ptr_new(/ALLOC)
    y_roi_data = ptr_new(/ALLOC)
    w_roi_data = ptr_new(/ALLOC)
  endif else begin
    x_roi_data = ptr_new( (*x_data)[index], /NO_COPY)
    y_roi_data = ptr_new( (*y_data)[index], /NO_COPY)
    if dataset.weight_available $
      then w_roi_data = ptr_new( (*w_data)[index], /NO_COPY) $
      else w_roi_data = ptr_new(/ALLOC)
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
    if dataset.weight_available then begin
      total_wt = total( *w_roi_data, /DOUBLE )
      dataset.xcen     = total(*x_roi_data * (*w_roi_data), /DOUBLE)/total_wt
      dataset.ycen     = total(*y_roi_data * (*w_roi_data), /DOUBLE)/total_wt
      
      if (area GT 0) then begin
        f='("density=",G10.4," (sum of weights=",G10.4,", area=",G10.4,")")'
        dataset.stats = string( total_wt/area, total_wt, area, F=f )
      endif else begin
        f='(I0," data points; sum of weights=",G10.4)'
        dataset.stats = string( num_points, total_wt, F=f )
      endelse
    endif else begin
      dataset.xcen = total(*x_roi_data, /DOUBLE)/num_points
      dataset.ycen = total(*y_roi_data, /DOUBLE)/num_points
      
      if (area GT 0) then begin
        f='("density=",G10.4," (",I0," data points in area of ",G10.4,")")'
        dataset.stats = string( num_points/float(area), num_points, area, F=f )
      endif else begin
        f='(I0," data points")'
        dataset.stats = string( num_points, F=f )
      endelse
    endelse
    
    dataset.stats = dataset.stats + $
      string(dataset.xcen,dataset.ycen,$
		F='("; centroid= [",G11.5,", ",G11.5," ]")')
    
    dataset.stats = strcompress( dataset.stats )
  endelse  
endif


;; If we're supposed to be filtering, then do it.
if (dataset.filter_stale) then begin
  dataset.filter_stale = 0

  ;; If the existing ?_filtered_data tags point to their own heap vars, then
  ;; get rid of those heap vars.
  if (dataset.x_filtered_data NE dataset.x_data) then $
  	ptr_free, dataset.x_filtered_data
  	
  if (dataset.y_filtered_data NE dataset.y_data) then $
	ptr_free, dataset.y_filtered_data

  if (dataset.w_filtered_data NE dataset.w_data) then $
	ptr_free, dataset.w_filtered_data

  ;; Is filtering selected?
  if (widget_info( (*st).roi_mode, /DROPLIST_SELECT ) NE 2) then begin
    dataset.x_filtered_data  = dataset.x_data
    dataset.y_filtered_data  = dataset.y_data
    dataset.w_filtered_data  = dataset.w_data
    *dataset.mask = 0
    dum = temporary( *dataset.mask )
  endif else begin
    dataset.x_filtered_data = x_roi_data
    dataset.y_filtered_data = y_roi_data
    dataset.w_filtered_data = w_roi_data
    
    *dataset.mask = temporary( mask )
  endelse

  ;; Find the max and min of the filtered dataset.
  dataset.num_data_points = n_elements( *dataset.x_filtered_data )

  if (dataset.num_data_points GT 0) then begin
    dataset.x_max           = max(*dataset.x_filtered_data, MIN=x_min, /NAN)
    dataset.x_min           = x_min
    dataset.y_max           = max(*dataset.y_filtered_data, MIN=y_min, /NAN)
    dataset.y_min           = y_min
  endif  
  
  
  ;; Since we've changed the filtered dataset, mark all derived data structures
  ;; as stale.
  dataset.density.stale   = 1
  
endif ; (dataset.filter_stale EQ 1)

*(*st).fit.x = [0]
return
end

;==========================================================================
;;; Routine to define a binning region and bin up bivariate data.
;;; (x0,y0) are the coordinates of the center of the first pixel.
;==========================================================================
PRO Bin2dDataset, dataset, MARGIN=margin, $
		  x0, y0, num_col, num_row, col, row

if (n_elements(margin) NE 2) then margin = [0,0]

bin = dataset.binning


;; Compute default sample sizes for density.  
if (bin.desired_delta_x EQ 0) then begin
      bin.delta_x = float(limit_precision((dataset.x_max - dataset.x_min)/200., 2))
      if (size(*dataset.x_data, /TYPE) LE 3) then begin
        bin.delta_x = round(bin.delta_x)
        bin.xbin_location = 0.5
      endif
      if (bin.delta_x LE 0) then bin.delta_x = 1.0
endif else bin.delta_x = bin.desired_delta_x
  
if (bin.desired_delta_y EQ 0) then begin
      bin.delta_y = float(limit_precision((dataset.y_max - dataset.y_min)/200., 2))
      if (size(*dataset.y_data, /TYPE) LE 3) then begin
        bin.delta_y = round(bin.delta_y)
        bin.ybin_location = 0.5
      endif
      if (bin.delta_y LE 0) then bin.delta_y = 1.0
endif else bin.delta_y = bin.desired_delta_y

if ((bin.desired_delta_x EQ 0) AND (bin.desired_delta_y EQ 0)) then begin  
      ; If we're computing both bins sizes, make them the same.   
      xy_bin = bin.delta_x > bin.delta_y
      bin.delta_x = xy_bin
      bin.delta_y = xy_bin
endif


;; If necessary, increase the sample intervals (delta_x & delta_y) requested 
;; until a reasonably sized array results.
maxdim = 3+ceil(sqrt(2) * 2048)
repeat begin
  xdim = (dataset.x_max - dataset.x_min) / bin.delta_x
  ydim = (dataset.y_max - dataset.y_min) / bin.delta_y

  done = 1
  if (xdim GT maxdim) then begin
    bin.delta_x = 2 * bin.delta_x
    done      = 0
  endif
  
  if (ydim GT maxdim) then begin
    bin.delta_y = 2 * bin.delta_y
    done      = 0
  endif
endrep until (done)

dataset.binning = bin


;; Calculate the signed distance from the smallest datapoint to the bin corner
;; the user specified.
datamin_to_edge_distance_x = dataset.x_min - bin.xbin_location
datamin_to_edge_distance_y = dataset.y_min - bin.ybin_location

;; Calculate the location of the bin corner which is <= the smallest 
;; datapoint.
x_min = floor( datamin_to_edge_distance_x / bin.delta_x ) * bin.delta_x $
        + bin.xbin_location
y_min = floor( datamin_to_edge_distance_y / bin.delta_y ) * bin.delta_y $
        + bin.ybin_location

;; Add the desired margin to the left & bottom edges of the binning region.	
x_min = x_min - bin.delta_x * margin[0]
y_min = y_min - bin.delta_y * margin[1]
  	     
;; Next, discretize the data vectors.
col = floor( (*dataset.x_filtered_data - x_min) / bin.delta_x ) 
row = floor( (*dataset.y_filtered_data - y_min) / bin.delta_y ) 

num_col = max(col)+1  &  num_row = max(row)+1 
 
;; Add the desired margin to the right & top edges of the binning region.	
num_col = num_col + margin[0]
num_row = num_row + margin[1]

;; Compute (x0,y0,t0) the coordinates of the center of the first pixel.
x0 = x_min + bin.delta_x/2.0
y0 = y_min + bin.delta_y/2.0

;; Make sure there are at least two bins in each direction.
num_col = num_col > 2
num_row = num_row > 2

return
end


;==========================================================================
;;; Routine to estimate a density function, sampled on a uniform 2-D grid.
;;; The structure 'dataset' is modified!
;;; The phase of the sample grid is controlled by the parameters 
;;; (*st).binning.xbin_location & ybin_location
;;;
;;; The structure tags x0 & y0 refer to the data coordinates of the CENTER 
;;; of the lower-left bin.
;==========================================================================
PRO ComputeDataset2dDensity, dataset, st

d = dataset.density

;; First, see if the density has already been calculated.
if (d.stale EQ 0) then return

d.stale = 0

;; Now, make sure there is some data left after filtering.
if (dataset.num_data_points EQ 0) then begin
  *d.samples = lonarr(4,4)
  d.xdim = 4  &  d.ydim = 4
  d.subtitle = 'NULL DATASET!'
  dataset.density = d
  (*st).stale_titles = 1
  return
endif


;; Compute a 2-D histogram.
case (d.kernel) of
   0: kernel_dim = 1
   
   ; Epanechnikov kernel
   ; The kernel always has ODD dimensions equal to d.Epanechnikov_width-1.
   1: kernel_dim = d.Epanechnikov_width-1
  
   ;Gaussian kernel
   ; The kernel always has ODD dimensions sufficient to cover 4-sigma.
   2: kernel_dim = 1 + (2 * ceil(3 * d.Gaussian_sigma ))
endcase

convolution_margin   = (kernel_dim  -1)/2
  
Bin2dDataset, dataset, MARGIN=[convolution_margin,convolution_margin],$
	      x0, y0, num_col, num_row, col, row
	      
bin    = dataset.binning
d.x0   = x0
d.y0   = y0
d.xdim = num_col
d.ydim = num_row

msg = string(bin.delta_x, bin.delta_y, $
  	f='("Computing histogram with bins ",G10.4," X ",G10.4)' )
widget_control, (*st).msg_label, SET_VALUE=msg
  
  
;; Compute the possibly weighted histogram.
if dataset.weight_available then begin
    if ((*st).indexed_add_lib EQ '') then begin
      ;; Look for a required shared library first in the UNIX path and then in the TARA library.
      ;; Administrators might put it n the UNIX path so mulitple platforms can share one TARA library.
      ;; We have to avoid searching the cwd since it might be a very large tree of user data.
      if (!VERSION.MEMORY_BITS EQ 64) then lib_name = 'indexed_add_64.so' $
                                      else lib_name = 'indexed_add.so'
      
      roots = break_path(getenv('PATH'))
      ind = where((strmatch(roots,'.*') OR strmatch(roots,'')) EQ 0, count)
      if (count GT 0) then lib_paths = file_search(roots[ind], lib_name, COUNT=count)
      if (count GT 0) then begin
        (*st).indexed_add_lib = lib_paths[0]
      endif else begin
        color_manager, UTILITY_DIR=utility_dir
        (*st).indexed_add_lib = utility_dir + lib_name
      endelse
    endif
    
    ; We must add weights in DOUBLE precision, but we convert to FLOAT
    ; afterward to reduce memory usage.
    index_1d   = row * long(num_col) + col
    samples    = dblarr( num_col, num_row )

    ; Don't forget: 1st and 3rd arg must be DOUBLE, 2nd must be LONG.
    dum = CALL_EXTERNAL( (*st).indexed_add_lib, 'indexed_add_double', $
                         samples, index_1d, $
                         double(*dataset.w_filtered_data), $
                         n_elements(index_1d) )
    *d.samples = float( temporary(samples) )
    
endif else begin
    *d.samples = hist_2d( col, row, MIN1=0L, MAX1=num_col-1, $
    				    MIN2=0L, MAX2=num_row-1 )
endelse
  

;; Now, apply kernel smoothing if requested.
case (d.kernel) of
 0: $
  begin
  ;; HISTOGRAM
  d.subtitle = strcompress( string( bin.delta_x, bin.delta_y, $
  		            f='("Binsize: ",G10.4," X ",G10.4)' ) )
  end

 1: $
  begin
  ;; EPANECHNIKOV SMOOTHING
  d.bandwidth_x = bin.delta_x * d.Epanechnikov_width/2
  d.bandwidth_y = bin.delta_y * d.Epanechnikov_width/2

  d.subtitle = strcompress( string( d.bandwidth_x, d.bandwidth_y, $
  		   f='("Epanechnikov kernel bandwidths: ",G10.4," X ",G10.4)' ) )

  widget_control, (*st).msg_label, SET_VALUE='smoothing histogram'
  
  ;; The data themselves are now discretized using the sampling intervals
  ;; delta_x & delta_y.  We want to discretize the kernel with the same
  ;; sampling intervals, then convolve the two to smooth.
    
  ; First, make a kernel array where each element is the distance from
  ; the center in array index units.
  kernel = shift( dist(kernel_dim), kernel_dim/2, kernel_dim/2)
  
  ; Now, scale these distances to the kernel coordinate system in which the
  ; kernel support is [-1,1].
  kernel = kernel * 2.0 / d.Epanechnikov_width
  
  ; Make a mask that is zero for any samples at a distance greater than 1.0
  ; so that we can zero those sample later.
  mask = (kernel LT 1.0)
  
  ; Now, compute the kernel values, normalizing so that the sum is 1.0
  ; Kernel is zero for distances greater than 1.0
  kernel = (2.0 / !PI) * (1 - kernel^2)
  kernel = kernel * mask 
  kernel = kernel / total(kernel)

  ; Now, convolve with histogram.  
  ; With the histogram padded by convolution_margin, the /EDGE_WRAP below should produce the same result as /EDGE_ZERO.
  ; I don't recall why I didn't use /EDGE_ZERO for extra protection from wrapping, but I hesitate to change the code now (2012).
  *d.samples= convol( float(*d.samples), kernel, /EDGE_WRAP )
  end


 2: $
  begin
  ;; GAUSSIAN SMOOTHING
  d.bandwidth_x = bin.delta_x * d.Gaussian_sigma 
  d.bandwidth_y = bin.delta_y * d.Gaussian_sigma

  d.subtitle = strcompress( string( d.bandwidth_x, d.bandwidth_y, $
  	f='("Gaussian kernel bandwidths (1-sigma): ",G10.4," X ",G10.4)' ) )

  widget_control, (*st).msg_label, SET_VALUE='smoothing histogram'
  
  ;; The data themselves are now discretized using the sampling intervals
  ;; delta_x & delta_y.  We want to discretize the kernel with the same
  ;; sampling intervals, then convolve the two to smooth.
    
  ; First, make a kernel array where each element is the distance from
  ; the center in array index units.
  kernel = shift( dist(kernel_dim), kernel_dim/2, kernel_dim/2)

  ; Normalize those distances by the bandwidth parameter..
  kernel = kernel / d.Gaussian_sigma
      
  ; Now, compute the kernel values, normalizing so that the sum is 1.0
  kernel = exp( (kernel^2) / (-2.0) )
  kernel = kernel / total(kernel)

  ; Now, convolve with histogram.  
  ; With the histogram padded by convolution_margin, the /EDGE_WRAP below should produce the same result as /EDGE_ZERO.
  ; I don't recall why I didn't use /EDGE_ZERO for extra protection from wrapping, but I hesitate to change the code now (2012).
  *d.samples= convol( float(*d.samples), kernel, /EDGE_WRAP )
  end
endcase


dataset.tv_stale = 1

dataset.density = d

(*st).stale_titles = 1
return
end



;==========================================================================
;;; Routine to display an image
;==========================================================================
PRO DrawDataset2dImage, dataset, st

  ; Draw the plot axes.
  if ((*st).stale_titles) then begin
    case 1 of
     ((*st).title NE ''):         title = (*st).title 
     else:			  title = dataset.description
    endcase

    plot_window, (*st).pw_id, SUBTITLE=dataset.density.subtitle, $
				 TITLE=title 
  endif

  plot_window, (*st).pw_id, /SHOW_AXES, /FORCE_LINEAR, $
  	       XAXIS=xaxis, YAXIS=yaxis
  
;  msg = 'Sampling density to form image ...'
;  widget_control, (*st).msg_label, SET_VALUE=msg

  bin = dataset.binning
  d = dataset.density

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
  resample_image, d.x0, d.y0, bin.delta_x, bin.delta_y,   *d.samples, $
		  x0_p, y0_p, delta_x_p,   delta_y_p, $
		  xdim_p, ydim_p, *dataset.image,$
		  *dataset.bgrnd_index, *dataset.fgrnd_index

  if (d.kernel EQ 0) then begin
      feature_size_x = bin.delta_x
      feature_size_y = bin.delta_y
  endif else begin
      feature_size_x = d.bandwidth_x
      feature_size_y = d.bandwidth_y
  endelse
  
  ; If our new image was all background pixels then we fix up fgrnd_index so
  ; there won't be problems later when we need to compute stats on foreground
  ; pixels.
  if (size(*dataset.fgrnd_index, /N_DIM) EQ 0) then $
    *dataset.fgrnd_index = lindgen(n_elements(*dataset.image))


  ; Warn the user if the density/stat_map is undersampled.
  show_message = (delta_x_p GT feature_size_x OR delta_y_p GT feature_size_y)
  
  if show_message then begin
    temp=string( delta_x_p, delta_y_p, f='("[",G10.4,", ",G10.4,"].")' )
    msg=['Your histogram bins (kernel bandwidths) are too small to produce an',$
         'accurate display at this magnification.',$
         'The image you see may be misleading.','',$
         'Change the bins (bandwidths) to be larger than '+strcompress(temp), $
         'OR','increase the size of the plotting window,',$
         'OR','zoom in (reduce the range of your axes).']
    
    id=(*st).msg_widget
    ; We have to supply XOFFSET to keep TimedMessage from calling 
    ; ScreenCenter() which will die when the device is PS!
    TimedMessage, id, msg, GROUP=(*st).pw_id, XOFFSET=500, TIT='WARNING!'
    (*st).msg_widget=id
  endif else begin
     widget_control, (*st).msg_widget, /DESTROY, BAD_ID=bad
  endelse
  

  ;; Now that an image array has been calculated, we may need to transform
  ;; it and compute some statistics.
;  msg = 'Scaling image and displaying ...'
;  widget_control, (*st).msg_label, SET_VALUE=msg
    
  if dataset.log_flag then begin
    index = where( *dataset.image LE 0, count )
    if (count GT 0) then $
      (*dataset.image)[index] = 2L^30
    
    *dataset.image = alog( *dataset.image  ) 
    
    if (count GT 0) then begin
      (*dataset.image)[index] = min( *dataset.image ) - 1 
    endif
  endif
    
        
  ;; Scale the image as desired and display.
  TvDataset2d, dataset, st, xaxis, yaxis, $
  		COLORBAR=(*st).color_bar, LOG=dataset.log_flag

  
  ;; Save the dataset structure, which may have been modified.
  Save2dDataset, dataset, st
return
end


;==========================================================================
;;; Routine to update the widget's appearance
;==========================================================================
PRO RedrawDataset2d, st

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


;--------------------------------------------------------------------------
;; Figure out which dataset is selected, make sure the ROI is applied, but
;; we'll delay printing the stats until the end because some of the code
;; below uses (*st).msg_label for status messages.
;--------------------------------------------------------------------------
selected_index = (where( (*st).datasets.name EQ (*st).selected_name))[0]

selected_dataset                 = (*st).datasets[ selected_index ]
ApplyDataset2dRoi, selected_dataset, st
Save2dDataset, selected_dataset, st

;--------------------------------------------------------------------------
;; Filter all the datasets we're going to display -- this calculates x_min
;; & y_min which may be needed to draw axes.
;; Compute the densities needed -- this computes the subtitle
;; we may need to assign.
;--------------------------------------------------------------------------
if (mode EQ 0) then begin
  ;; SCATTER PLOT (multiple datasets possible)
  ;; Apply the ROI to all the datasets that will be shown, saving the
  ;; modified structures back to (*st).
  shown = where( ((*st).datasets.name NE '') AND $
  		 ((*st).datasets.hidden_flag EQ 0), count )

  if (count EQ 0) then shown = [selected_index]
  
  for ii = 0, n_elements(shown)-1 do begin
    dataset = (*st).datasets[ shown[ii] ]
    ApplyDataset2dRoi, dataset, st
    Save2dDataset, dataset, st
  endfor
    
  plotsets = ((*st).datasets)[ shown ]
  
  set_xrange = [min( plotsets.x_min, /NAN ), max( plotsets.x_max, /NAN )]
  set_yrange = [min( plotsets.y_min, /NAN ), max( plotsets.y_max, /NAN )]
  add_margin = 1
  
endif else begin
  ;; SOME KIND OF DENSITY DISPLAY
  ;; Compute the density, and save the modified dataset.

  ComputeDataset2dDensity, selected_dataset, st
  Save2dDataset, selected_dataset, st
  
  x_low  = selected_dataset.density.x0 - selected_dataset.binning.delta_x/2.0
  y_low  = selected_dataset.density.y0 - selected_dataset.binning.delta_y/2.0
  x_high = x_low + selected_dataset.density.xdim * selected_dataset.binning.delta_x
  y_high = y_low + selected_dataset.density.ydim * selected_dataset.binning.delta_y
  set_xrange = [x_low,x_high]
  set_yrange = [y_low,y_high]
  add_margin = 0
endelse


;--------------------------------------------------------------------------
;; Assign default axis ranges if necessary.
;--------------------------------------------------------------------------
plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis
			  
if (xaxis.default_flag EQ 0) then set_xrange=0 
if (yaxis.default_flag EQ 0) then set_yrange=0

plot_window, (*st).pw_id, ADD_MARGIN=add_margin, $
			  SET_XRANGE=set_xrange, SET_YRANGE=set_yrange

; Unless we're printing, make sure model is X_PSEUDO (not X_TRUE).
if (!D.NAME EQ 'X') then color_manager, /X_PSEUDO
color_manager, BLUE=blue, GREEN=green


;; Display the datasets.
case (mode) of
 ;-------------------------------------------------------------------------
 0: $ SCATTER PLOT
 ;-------------------------------------------------------------------------
  begin
  ; Draw the plot axes.
  if ((*st).stale_titles) then begin
    case 1 of
     ((*st).title NE ''):         title = (*st).title 
     (n_elements(plotsets) EQ 1): title = plotsets[0].description
     else:			  title = ''
    endcase

    plot_window, (*st).pw_id, SUBTITLE='', TITLE=title
  endif
  
  plot_window, (*st).pw_id, /SHOW_AXES
  
  ;------------------------------------------------------------------------
  ; Plot each of the datasets.
  ;------------------------------------------------------------------------
  for ii = 0, n_elements(plotsets)-1 do begin
    dataset = plotsets[ii]
    
    ;; Scatter plot
    p = dataset.plot
    color_manager, p.color, plot_color
    if keyword_set(p.plotsym) then begin
      cmd = 'plotsym, ' + p.plotsym
      if NOT execute(cmd) then message, 'call to plotsym.pro failed', /CONTINUE
    endif
        
    if (dataset.num_data_points GT 0) then $
      oplot, *dataset.x_filtered_data, *dataset.y_filtered_data,  $
      	     PSYM=p.psym, COLOR=plot_color
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
        
        descriptions = plotsets.description
        psyms        = plotsets.plot.psym
        lines        = intarr( n_elements(psyms) )
        if (n_elements(*(*st).fit.x) GT 1) then begin
          colors       = [colors,green]
          descriptions = [descriptions, (*st).fit.name]
          psyms        = [psyms, 0]
          lines        = [lines, 0]
        endif 
              
        ; Coyote Graphics requires color indexes to be INT, not LONG.
        al_legend, descriptions, LINES=lines, PSYM=psyms,  $
                /TOP,LEFT=(lstyle EQ 1),CEN=(lstyle EQ 2),RIGHT=(lstyle EQ 3),$
                COLORS=fix(colors), TEXTCOLORS=fix(colors), CHARSIZE=0.75*!P.CHARSIZE
  endif
  end
  
 ;-------------------------------------------------------------------------
 1: $ IMAGE
 ;-------------------------------------------------------------------------
  begin
  DrawDataset2dImage, selected_dataset, st
  end
  
 ;-------------------------------------------------------------------------
 2: $ SURFACE PLOT
 ;-------------------------------------------------------------------------
  begin
  if ((*st).stale_titles) then begin
    case 1 of
     ((*st).title NE ''):         title = (*st).title 
     else:			  title = selected_dataset.description
    endcase

    plot_window, (*st).pw_id, SUBTITLE=selected_dataset.density.subtitle, $
				 TITLE=title 
  endif

  plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis, ZAXIS=zaxis, $
  		GET_TITLE=title, GET_SUBTITLE=subtitle, DRAW_WIDGET=draw_widget

  if (zaxis.default_flag) then begin
    maxz = max( *selected_dataset.density.samples, MIN=minz )
    plot_window, (*st).pw_id, SET_ZRANGE=[minz,maxz], ZAXIS=zaxis
  endif
  
  x = selected_dataset.density.x0 + $
      findgen(selected_dataset.density.xdim) * selected_dataset.binning.delta_x
  y = selected_dataset.density.y0 + $
      findgen(selected_dataset.density.ydim) * selected_dataset.binning.delta_y

  ;; For the 'X' device, we must select the correct window to draw in.
  if (!D.NAME EQ 'X') then coordsystem_manager, draw_widget, /RESTORE

  color_manager, selected_dataset.plot.color, plot_color, BLACK=black
  if ((*st).surface_mode EQ 2) then begin
    shade_surf, *selected_dataset.density.samples, x, y, /SAVE, $
  	   TITLE=title, SUBTITLE=subtitle, $
  	   AX=(*st).x_axis_rotation, AZ=(*st).z_axis_rotation, $
  	   CHARSIZE=2.0, $
	   COLOR=plot_color, BACKGROUND=black, $
  	   XMARGIN=xaxis.margin, XSTYLE=3, $
  	   YMARGIN=yaxis.margin, YSTYLE=3, $
  	   XLOG=xaxis.log_flag, XRANGE=xaxis.range_desired, XTITLE=xaxis.title,$
  	   YLOG=yaxis.log_flag, YRANGE=yaxis.range_desired, YTITLE=yaxis.title,$
  	   ZRANGE=zaxis.range_desired, ZTITLE=zaxis.title
  endif else begin
    surface, *selected_dataset.density.samples, x, y, /SAVE, $
  	   TITLE=title, SUBTITLE=subtitle, $
  	   AX=(*st).x_axis_rotation, AZ=(*st).z_axis_rotation, $
  	   LEGO=((*st).surface_mode EQ 1), CHARSIZE=2.0, $
	   COLOR=plot_color, BACKGROUND=black, $
  	   XMARGIN=xaxis.margin, XSTYLE=3, $
  	   YMARGIN=yaxis.margin, YSTYLE=3, $
  	   XLOG=xaxis.log_flag, XRANGE=xaxis.range_desired, XTITLE=xaxis.title,$
  	   YLOG=yaxis.log_flag, YRANGE=yaxis.range_desired, YTITLE=yaxis.title,$
  	   ZLOG=zaxis.log_flag, ZRANGE=zaxis.range_desired, ZTITLE=zaxis.title
  endelse
  end
  
 ;-------------------------------------------------------------------------
 3: $ CONTOUR PLOT
 ;-------------------------------------------------------------------------
  begin
  if ((*st).stale_titles) then begin
    case 1 of
     ((*st).title NE ''):         title = (*st).title 
     else:			  title = selected_dataset.description
    endcase

    plot_window, (*st).pw_id, SUBTITLE=selected_dataset.density.subtitle, $
				 TITLE=title 
  endif

  ;; Compute the x/y coordinates of the density samples for contour routine.
  x = selected_dataset.density.x0 + $
      findgen(selected_dataset.density.xdim) * selected_dataset.binning.delta_x
  y = selected_dataset.density.y0 + $
      findgen(selected_dataset.density.ydim) * selected_dataset.binning.delta_y


  ;; For the 'X' device, we must select the correct window to draw in.
  plot_window, (*st).pw_id, DRAW_WIDGET=draw_widget
  if (!D.NAME EQ 'X') then coordsystem_manager, draw_widget, /RESTORE
  
  ;; If necessary, parse (*st).contour_kwds into an _EXTRA structure.
  if ((*st).contour_kwds NE '') then begin
    if (0 EQ  execute( 'build_extra, extra, ' + (*st).contour_kwds)) then begin
      msg = ['ERROR parsing CONTOUR keywords:',(*st).contour_kwds]
      dummy=dialog_message(msg, /ERROR, DIALOG_PARENT=(*st).pw_id)
    endif
  endif
  
  ;; Call the contour routine to update !Z, but not draw anything.
  ;; SAVE THE GRAPHICS VARIABLES now because there is a call to 
  ;; coordsystem_manager, /RESTORE in "plot_window" just below.
  contour, *selected_dataset.density.samples, x, y, /NODATA, _EXTRA=extra
  
  if (!D.NAME EQ 'X')  then coordsystem_manager, draw_widget, /SAVE
  if (!D.NAME EQ 'PS') then device,/CLOSE
  
  ;; Now draw the axes & titles.
  plot_window, (*st).pw_id, /SHOW_AXES, /FORCE_LINEAR
    
  ;; And finally overplot the contours.
  color_manager, selected_dataset.plot.color, plot_color
  contour, *selected_dataset.density.samples, x, y, /OVERPLOT, $
  	   COLOR=plot_color, _EXTRA=extra

  ;; You would think that we could omit the first call to contour and 
  ;; simply call plot_window then contour, /OVERPLOT, but that doens't work.
  ;; The contour, /OVERPLOT call doesn't seem to calculate !Z -- RSI
  ;; probably assumes you want to overplot on top of another contour.
  ;; So, the purpose of the first call to contour is simply to calculate !Z.
  
  if keyword_set((*st).contour_regionfile) then begin
    contour, *selected_dataset.density.samples, x, y, /OVERPLOT, $
          PATH_XY=xy, PATH_INFO=info, /PATH_DATA_COORDS, _EXTRA=extra
  
    openw,  region_unit, (*st).contour_regionfile, /GET_LUN
    printf, region_unit, "# Region file format: DS9 version 3.0"
    printf, region_unit, "linear"
    !TEXTUNIT = region_unit  
    for ii=0, n_elements(info)-1 do begin
      ind = info[ii].offset + indgen(info[ii].N)
      polygon_x = float(reform(xy[0, ind]))
      polygon_y = float(reform(xy[1, ind]))
      o_poly=obj_new('IDLanROI', polygon_x, polygon_y)
      aperture_ind = where((o_poly->ContainsPoints(*selected_dataset.x_filtered_data, *selected_dataset.y_filtered_data) EQ 1), aperture_count)
      print, info[ii].value, aperture_count, (100.0*aperture_count)/selected_dataset.num_data_points, F='(%"Contour at level %0.4g encloses %d data points (%0.1f %%)")'
      
      polygon = fltarr(2,n_elements(polygon_x))
      polygon[0,*] = polygon_x
      polygon[1,*] = polygon_y

      src_region = 'polygon(' + strcompress(strjoin(string(polygon,F='(G0.3)'),","), /REMOVE) + ')'
      printf, region_unit, src_region, info[ii].value, F="(%'%s # tag={level %0.4g}')"
    endfor  
    free_lun, region_unit
    print, 'Wrote contours to region file ', (*st).contour_regionfile
  endif ; keyword_set((*st).contour_regionfile) 
  end
  
 ;-------------------------------------------------------------------------
 4: $ CONTOUR PLOT ON IMAGE
 ;-------------------------------------------------------------------------
  begin
  ;; Try to find a second non-hidden dataset for the contour.
  contour_index= (where( ((*st).datasets.name NE '') AND $
  		         ((*st).datasets.name NE (*st).selected_name) AND $
  		         ((*st).datasets.hidden_flag EQ 0), count ))[0]

  if (count EQ 0) then contour_index = selected_index
  
  ;; Filter the contour dataset and compute its density.
  contour_dataset = (*st).datasets[ contour_index ]
  ApplyDataset2dRoi, contour_dataset, st
  ComputeDataset2dDensity, contour_dataset, st
  Save2dDataset, contour_dataset, st

  
  ;; Compute the x/y coordinates of the density samples for contour routine.
  x = contour_dataset.density.x0 + $
      findgen(contour_dataset.density.xdim) * contour_dataset.binning.delta_x
  y = contour_dataset.density.y0 + $
      findgen(contour_dataset.density.ydim) * contour_dataset.binning.delta_y


  ;; For the 'X' device, we must select the correct window to draw in.
  plot_window, (*st).pw_id, DRAW_WIDGET=draw_widget

  if (!D.NAME EQ 'X') then coordsystem_manager, draw_widget, /RESTORE

  ;; If necessary, parse (*st).contour_kwds into an _EXTRA structure.
  if ((*st).contour_kwds NE '') then begin
    if (0 EQ  execute( 'build_extra, extra, ' + (*st).contour_kwds)) then begin
      msg = ['ERROR parsing CONTOUR keywords:',(*st).contour_kwds]
      dummy=dialog_message(msg, /ERROR, DIALOG_PARENT=(*st).pw_id)
    endif
  endif
  
  ;; Call the contour routine to update !Z, but not draw anything.
  ;; SAVE THE GRAPHICS VARIABLES now because there is a call to 
  ;; coordsystem_manager, /RESTORE in "plot_window" just below.
  contour, *contour_dataset.density.samples, x, y, /NODATA, _EXTRA=extra
  if (!D.NAME EQ 'X')  then coordsystem_manager, draw_widget, /SAVE
  if (!D.NAME EQ 'PS') then device,/CLOSE

  ;; Now draw the axes, titles, & image.
  DrawDataset2dImage, selected_dataset, st
    
  ;; And finally overplot the contours.
  color_manager, contour_dataset.plot.color, plot_color
  contour, *contour_dataset.density.samples, x, y, /OVERPLOT, $
  	   COLOR=plot_color, _EXTRA=extra

  ;; You would think that we could omit the first call to contour and 
  ;; simply call plot_window then contour, /OVERPLOT, but that doens't work.
  ;; The contour, /OVERPLOT call doesn't seem to calculate !Z -- RSI
  ;; probably assumes you want to overplot on top of another contour.
  ;; So, the purpose of the first call to contour is simply to calculate !Z.
  end
endcase

(*st).stale_titles = 0

;------------------------------------------------------------------------
;; Draw the note.
;------------------------------------------------------------------------
if ((*st).note NE '') then begin
  xyouts, (*st).note_x, (*st).note_y, (*st).note, /DATA
endif


;------------------------------------------------------------------------
;; Draw a graphic representing the ROI.
;------------------------------------------------------------------------
if ((*st).show_roi AND (mode NE 2)) then begin
 plot_window, (*st).pw_id, /SHOW_MARKERS, XAXIS=xaxis, YAXIS=yaxis

 if (widget_info( (*st).roi_mode, /DROPLIST_SELECT ) NE 0) then begin
 t3d = (mode EQ 2)
 case (widget_info( (*st).roi_style, /DROPLIST_SELECT )) of
   
   1: $ ; X-range
    begin
    xl=(*st).roi_params.xl   &  xh=(*st).roi_params.xh
    if (!Y.TYPE EQ 0) then begin
      yl=!Y.CRANGE(0)    &  yh=!Y.CRANGE(1)
    endif else begin
      yl=10^!Y.CRANGE(0) &  yh=10^!Y.CRANGE(1)
    endelse
    plots, [xl,xl], [yl,yh], /DATA, THICK=2, COLOR=blue, T3D=t3d 
    plots, [xh,xh], [yl,yh], /DATA, THICK=2, COLOR=blue, T3D=t3d 
    end
  
   2: $ ; Y-range
    begin
    if (!X.TYPE EQ 0) then begin
      xl=!X.CRANGE(0)    &  xh=!X.CRANGE(1)
    endif else begin
      xl=10^!X.CRANGE(0) &  xh=10^!X.CRANGE(1)
    endelse
    yl=(*st).roi_params.yl   &  yh=(*st).roi_params.yh
    plots, [xl,xh], [yh,yh], /DATA, THICK=2, COLOR=blue, T3D=t3d 
    plots, [xl,xh], [yl,yl], /DATA, THICK=2, COLOR=blue, T3D=t3d 
    end
  
   0: $ ; box
    begin
    xl=(*st).roi_params.xl  &  xh=(*st).roi_params.xh
    yl=(*st).roi_params.yl  &  yh=(*st).roi_params.yh
    plots, [xl,xh,xh,xl,xl], [yl,yl,yh,yh,yl], /DATA, THICK=2, COLOR=blue, T3D=t3d 
    end
  
   3: $ ; annulus
    begin
    tvcircle, (*st).roi_params.rin, /DATA, THICK=2, COLOR=blue, T3D=t3d, $
    	      (*st).roi_params.center(0), (*st).roi_params.center(1)

    tvcircle, (*st).roi_params.rout, /DATA, THICK=2, COLOR=blue, T3D=t3d, $
    	      (*st).roi_params.center(0), (*st).roi_params.center(1)
    end
 endcase
 endif
endif ; (show_roi EQ 1)



;------------------------------------------------------------------------
;; Draw a graphic representing the fit.
;------------------------------------------------------------------------
if (n_elements(*(*st).fit.x) GT 1) then $
  oplot, *(*st).fit.x, *(*st).fit.f_of_x, COLOR=green

;; Finally, show the dataset stats in the message field.
widget_control, (*st).msg_label, SET_VALUE=selected_dataset.stats
return
end


;==========================================================================
;;; Event Handler Function
;==========================================================================
FUNCTION Dataset2dEventFn, Event

widget_control, /HOURGLASS
new_event   = 0
DestroyFlag = 0

;; Get the state structure. 
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st

selected_index   = (where( (*st).datasets.name EQ (*st).selected_name))(0)
selected_dataset = (*st).datasets[ selected_index ]
roi_mode         = widget_info( (*st).roi_mode, /DROPLIST_SELECT )

;; Process the event.
case Event.ID of

;--------------------------------------------------------------------------
; Plot window mouse events.
  (*st).pw_id: $
   begin
   if (Event.middle_button) then begin
     mode = widget_info( (*st).mode_list, /DROPLIST_SELECT )
     
     plot_window, (*st).pw_id, DRAW_WIDGET=draw_widget
       
     coordsystem_manager, draw_widget, /RESTORE
  
     click = convert_coord( event.button_coord, /DATA, /TO_DEVICE )
  
     if ((mode EQ 0) AND (selected_dataset.hidden_flag EQ 0) AND $
         (selected_dataset.num_data_points GT 0)) then begin
         
       ; We need to locate the datapoint closest to the click.
       ; Find distance from click to each data point in the device coor system.
       locations = convert_coord( *selected_dataset.x_filtered_data, /DATA, $
       				  *selected_dataset.y_filtered_data, /TO_DEVICE )
       
       
       distances = (locations(0,*) - click[0])^2 + $
       		   (locations(1,*) - click[1])^2
       		 
       ; Display closest data point.
       minDist = min(distances,Imin, /NAN)
       x_pt    = (*selected_dataset.x_filtered_data)(Imin)
       y_pt    = (*selected_dataset.y_filtered_data)(Imin)
       
       msg = string(x_pt,y_pt,Imin,F='(%"data=(%11.5g, %11.5g), zero-based index=%d")') 
  
       widget_control, (*st).msg_label, SET_VALUE=strcompress(msg)
       tvcrs, x_pt, y_pt, /DATA
     endif
       
     if (mode NE 0) then begin
       bin = selected_dataset.binning
       fn  = selected_dataset.density
       label = 'Density sample'

       col = round( (event.button_coord[0] - fn.x0) / bin.delta_x )
       row = round( (event.button_coord[1] - fn.y0) / bin.delta_y )
       
       if (col GE 0 AND col LT fn.xdim AND row GE 0 AND row LT fn.ydim) then begin
         value = (*fn.samples)[col,row]
         msg = string( label, $
			fn.x0 + col * bin.delta_x, $
			fn.y0 + row * bin.delta_y, $
			value,$
			f='(A," at [",G11.5,", ",G11.5,"] = ",G11.5)')
       endif else msg = ' '
       widget_control, (*st).msg_label, SET_VALUE=msg
     endif

   endif ;(Event.middle_button)
   
   if (event.redraw) then RedrawDataset2d, st
   end


;--------------------------------------------------------------------------
; Dataset droplist
  (*st).dataset_list: $
   begin
   indexes          = where( (*st).datasets.name NE '' )
   names            = (*st).datasets[indexes].name
   (*st).selected_name = names[ event.index ]

   (*st).stale_titles = 1
   RedrawDataset2d, st
   end


;--------------------------------------------------------------------------
; Mode droplist
  (*st).mode_list: $
   begin
   plot_window, (*st).pw_id, Z_ENABLE=0
   case (event.index) of
    0: widget_control, (*st).scaling_button, SET_VALUE='               '
    1: widget_control, (*st).scaling_button, SET_VALUE='Image Scaling  '
    2: begin
       widget_control, (*st).scaling_button, SET_VALUE='3-D Perspective'
       plot_window, (*st).pw_id, /Z_ENABLE
       end
    3: widget_control, (*st).scaling_button, SET_VALUE='               '
    4: widget_control, (*st).scaling_button, SET_VALUE='               '
   endcase
   (*st).stale_titles = 1
   RedrawDataset2d, st
   end


;--------------------------------------------------------------------------
; Derived menu
  (*st).derived_menu: $
   begin
   case Event.value of
    ;----------------------------------------------------------------------
    1: $ ;Mark Centroid
     begin 
     plot_window, (*st).pw_id, BIG_MARKER=big_marker, SMALL_MARKER=small_marker
     offset       = small_marker - big_marker
     big_marker   = [selected_dataset.xcen,selected_dataset.ycen]
     small_marker = big_marker + offset
     plot_window, (*st).pw_id, SET_BIG_MARKER=big_marker, SET_SMALL_MARKER=small_marker
     print, big_marker, F='(%"moved BIG marker to (%11.5g,%11.5g), SMALL marker followed")'
     RedrawDataset2d, st
     end

    ;----------------------------------------------------------------------
    2: $ ;Radial Distribution from Reference Point
     begin 
     ; We can only expand non-negative integer weights.
     if selected_dataset.weight_available then begin
       w_data = selected_dataset.w_filtered_data
       if (size( *w_data, /TYPE) GT 3) OR (min(*w_data, /NAN) LT 0) then begin    
         txt=['Radial distribution analyses not available for this dataset.']
         TimedMessage, msg_id, txt, GROUP=top_base, LIFE=5
         return, 0
       endif
     endif
     
     ; Use Big Marker as reference point.
     plot_window, (*st).pw_id, BIG_MARKER=big_marker
     ref_x = big_marker[0]
     ref_y = big_marker[1]
     
     delx = (*selected_dataset.x_filtered_data - ref_x)
     dely = (*selected_dataset.y_filtered_data - ref_y)
     
     distance = sqrt( delx^2 + dely^2 )
     theta    = atan(dely, delx)
     
     if selected_dataset.weight_available then begin
       ; Expand weights into data points.
       num_expanded_data = long(total(*w_data, /DOUBLE))
       expanded_distance = fltarr(num_expanded_data)
       expanded_theta    = fltarr(num_expanded_data)
     
       evt_index = 0L
       for w_index = 0L, selected_dataset.num_data_points-1 do begin
         count = (*w_data)[w_index]
     
         if (count GT 0) then begin
           expanded_distance[evt_index] = replicate(distance[w_index], count)
           expanded_theta   [evt_index] = replicate(theta   [w_index], count)
           evt_index = evt_index + count
         endif
       endfor     

       distance = temporary(expanded_distance)
       theta    = temporary(expanded_theta)
     endif
     
     ; Construct a normalization function to account for area of annuli.
     ; Make sure it extrapolates to zero for negative distances!
     norm_absc  = (max( distance, /NAN ) / 1000.0) * (findgen(1002) - 1.0)
     norm_val   = (2 * !PI) * norm_absc
     norm_val[0:1] = 0.0
     
     ; Spawn two widgets showing distance dataset.
     xtit = strcompress( string(ref_x, ref_y, $
     			 f='("Distance from (",G11.5,", ",G11.5,")")') )
     id = (*st).rsb_widget
     dataset_1d, id, distance, GROUP=top_base, $
     		 WIDGET_TITLE='Normalized Distances from the Reference Point',$
     		 XTITLE=xtit, DATASET_NAME=selected_dataset.name+' distances',$
		 NORM_ABSC=norm_absc, NORM_VAL=norm_val, BIN_LOCATION=0, $
		 DENSITY_TITLE='Radial Surface Brightness'
     (*st).rsb_widget = id
     
     id = (*st).ee_widget
     dataset_1d, id, distance, GROUP=top_base, $
     		 WIDGET_TITLE='Distances from the Reference Point',$
     		 XTITLE=xtit, DATASET_NAME=selected_dataset.name+' distances',$
		 NORMALIZE_DENSITY=2, BIN_LOCATION=0, $
		 DISTRIBUTION_TITLE='Encircled Energy'
     (*st).ee_widget = id
     
     ; And a widget showing theta distribution.
     xtit = strcompress( string(ref_x, ref_y, $
     			 f='("Angles (degrees) about (",G11.5,", ",G11.5,")")') )
     id = (*st).theta_widget
     dataset_1d, id, theta*!RADEG, GROUP=top_base, BINSIZ=15,$
     		 WIDGET_TITLE='Angles about the Reference Point',$
     		 XTITLE=xtit, DATASET_NAME=selected_dataset.name+' angles'
     (*st).theta_widget = id
     
     ; Give user instructions.
     moi=string(total( distance^2, /DOUB ) / n_elements(distance), f='(G10.4)')
     txt=['Distances from the Big Marker to each 2-D datapoint have',$
          'been computed, forming a 1-D dataset which has been passed',$
          'to two separate dataset_1d tools for analysis.','',$
          'One tool shows a distribution function of these distances',$
          'which is an `encircled energy plot'' of your 2-D dataset',$
          'i.e. a plot of the percentage of 2-D datapoints found within a',$
          'specific radius.','',$
          'The second tool also received a normalization function',$
          'accounting for the area of annuli.  This tool shows a',$
          'normalized density function of the distances, which is a',$
          '`radial surface brightness plot'' for your 2-D dataset,',$
          'i.e. a plot of the number of 2-D datapoints per unit area',$
          'as a function of radius.','',$
          'A possible metric for the radial spread of this data is the',$
          '`normalized moment of inertia'', defined as',$
          'TOTAL( (x-x0)^2 + (y-y0)^2 )/N, which has the value','',moi]
     TimedMessage, msg_id, txt, GROUP=top_base, LIFE=60
     end

    ;----------------------------------------------------------------------
    3: $ ;Density Function Samples
     begin 
     ;; Make sure the density is computed, then save the dataset 
     ;; structure, which may have been modified.
     ComputeDataset2dDensity, selected_dataset, st
     Save2dDataset, selected_dataset, st
   
     d=selected_dataset.density
   
     id = (*st).samples_widget
     dataset_1d, id, *d.samples, GROUP=top_base,  $
     		 WIDGET_TITLE='2-D Density Function Samples',$
     		 DATASET_NAME=selected_dataset.name,$
		 XTITLE='2-D Density Function Value'
     (*st).samples_widget = id
		 
     txt=['The 2-D array of density estimates you have computed',$
          '(a regular histogram or smoothed histogram) have been formed',$
          'into a 1-D dataset and sent to a ''dataset_1d'' tool for analysis.']
     TimedMessage, msg_id, txt, GROUP=top_base, LIFE=15
     end

    ;----------------------------------------------------------------------
    4: $ ;Distribution of X Data
     begin 
     id = (*st).xdata_widget
     plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis, GET_TITLE=title
     
     ;; Apply the ROI to all the datasets that will be shown, saving the
     ;; modified structures back to (*st).
     shown = where( ((*st).datasets.name NE '') AND $
                    ((*st).datasets.hidden_flag EQ 0), count )
        
     for ii = 0, count-1 do begin
       dataset = (*st).datasets[ shown[ii] ]
       ApplyDataset2dRoi, dataset, st
       Save2dDataset, dataset, st
       
       dataset_1d, id, *dataset.x_filtered_data, GROUP=top_base,  $
     		 WIDGET_TITLE='Distribution of X Data',$
     		 DATASET_NAME=dataset.name,$
		 XTITLE=xaxis.title, DENSITY_TITLE=title
     endfor

     (*st).xdata_widget = id
     end
    
    
    ;----------------------------------------------------------------------
    5: $ ;Distribution of Y Data
     begin 
     id = (*st).ydata_widget
     plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis, GET_TITLE=title
     
     ;; Apply the ROI to all the datasets that will be shown, saving the
     ;; modified structures back to (*st).
     shown = where( ((*st).datasets.name NE '') AND $
                    ((*st).datasets.hidden_flag EQ 0), count )
        
     for ii = 0, count-1 do begin
       dataset = (*st).datasets[ shown[ii] ]
       ApplyDataset2dRoi, dataset, st
       Save2dDataset, dataset, st
       
       dataset_1d, id, *dataset.y_filtered_data, GROUP=top_base,  $
     		 WIDGET_TITLE='Distribution of Y Data',$
     		 DATASET_NAME=dataset.name,$
		 XTITLE=yaxis.title, DENSITY_TITLE=title
     endfor

     (*st).ydata_widget = id
     end
    
    
    ;----------------------------------------------------------------------
    6: $ ;Cut Through Big Marker
     begin 
     d   = selected_dataset.density
     bin = selected_dataset.binning
     plot_window, (*st).pw_id, BIG_MARKER=big_marker, XAXIS=xaxis, YAXIS=yaxis
     col = round( (big_marker[0] - d.x0) / bin.delta_x )
     row = round( (big_marker[1] - d.y0) / bin.delta_y )
     
     if (col GE 0 AND col LT d.xdim) then begin
       y = d.y0 + bin.delta_y * findgen(d.ydim)
       desc=strcompress(string( xaxis.title, d.x0 + col * bin.delta_x,$
       		        f='("Cut through {",A0,"} = ",G11.5)'))
       id = (*st).ycut_widget
       function_1d, id, y, (*d.samples)[col,*], GROUP=top_base,$
       		    WIDGET_TITLE='Vertical Cut', PSYM=10,$
       		    XTITLE=yaxis.title, YTITLE='2-D Density Function Value',$
		    DATASET_NAME=selected_dataset.name, DESCRIPTION=desc
       (*st).ycut_widget = id
     endif
       
     if (row GE 0 AND row LT d.ydim) then begin
       x = d.x0 + bin.delta_x * findgen(d.xdim)
       desc=strcompress(string( yaxis.title, d.y0 + row * bin.delta_y,$
       		        f='("Cut through {",A0,"} = ",G11.5)'))
       id = (*st).xcut_widget
       function_1d,id, x, (*d.samples)[*,row], GROUP=top_base,$
       		    WIDGET_TITLE='Horizontal Cut', PSYM=10,$
       		    XTITLE=xaxis.title, YTITLE='2-D Density Function Value',$
		    DATASET_NAME=selected_dataset.name, DESCRIPTION=desc
       (*st).xcut_widget = id
     endif
     end
     
    ;----------------------------------------------------------------------
    7: $ ;Linear Regression
     begin 
     if selected_dataset.weight_available then begin
       txt=['Linear regression not available for this dataset.']
       TimedMessage, msg_id, txt, GROUP=top_base, LIFE=5
       return, 0
     endif
     
     if (selected_dataset.num_data_points GT 0) then begin
       parameters = linfit( *selected_dataset.x_filtered_data, $
     			    *selected_dataset.y_filtered_data, /DOUBLE, $
     			    SIGMA=sigma, CHISQ=chisq, PROB=prob)
     			    
       *(*st).fit.x      = [selected_dataset.x_min, selected_dataset.x_max]
       *(*st).fit.f_of_x = parameters[0] + parameters[1] * (*(*st).fit.x)
       (*st).fit.name    = string((*st).fit.number, selected_dataset.name,$
       				  F='("fit ",I0," to ",A)' )
       (*st).fit.number  = (*st).fit.number + 1

       txt = ['MODEL is: Y = mX + b','Fit parameters (and their errors):', $
          string(parameters[1],sigma[1],f='("  m = ",G10.4," (",G10.4,")")'), $
          string(parameters[0],sigma[0],f='("  b = ",G10.4," (",G10.4,")")'), $
          string(chisq,f='(" CHISQ = ",G10.4)'), $
          string(prob, f='(" PROB  = ",G10.4)')]
          
       TimedMessage, msg_id, txt, GROUP=top_base, POS=top_base, $
       		     TIT='Results from routine LINFIT'
       print & print, txt, FORMAT='(A)'
       tara_clipboard, POST=string(parameters[1],sigma[1],$
       				   parameters[0],sigma[0],chisq,prob)
       RedrawDataset2d, st
     endif
     end

    ;----------------------------------------------------------------------
    8: $ ;Linear Regression with sigma-clipping
     begin 
     if selected_dataset.weight_available then begin
       txt=['Linear regression not available for this dataset.']
       TimedMessage, msg_id, txt, GROUP=top_base, LIFE=5
       return, 0
     endif
     
     if (selected_dataset.num_data_points GT 0) then begin
       color_manager, GREEN=green
             
       *(*st).fit.x      = [selected_dataset.x_min, selected_dataset.x_max]
       xdata = *selected_dataset.x_filtered_data
       ydata = *selected_dataset.y_filtered_data

       ;; Prompt user for initial fit line.
       prompt='Click at one end of line'
       plot_window, (*st).pw_id, GET_MOUSE=Pa, PROMPT=prompt

       prompt='Click at the other end of line'
       plot_window, (*st).pw_id, GET_MOUSE=Pb, PROMPT=prompt
       
       prompt='Click on a desired boundary'
       plot_window, (*st).pw_id, GET_MOUSE=Pc, PROMPT=prompt
  
       M = (Pa[1]-Pb[1]) / (Pa[0]-Pb[0])
       B = Pa[1] - M * Pa[0]
       clip_dist = abs( Pc[1] - (M * Pc[0] + B) )
       
       txt = ['MODEL is: Y = mX + b','User-supplied parameters:', $
               string(M,f='("  m = ",G10.4)'), string(B,f='("  b = ",G10.4)'),$
               string(clip_dist,f='("  Y clipping distance = ", G10.4)')]
       print & print, txt, FORMAT='(A)' & print
       
       n_iterations = 5
       for ii = 0,n_iterations-1 do begin
         ;; Draw clipping ranges.
         oplot, *(*st).fit.x, B + M * (*(*st).fit.x) + clip_dist, $
         	COLOR=green, LINE=2
         oplot, *(*st).fit.x, B + M * (*(*st).fit.x) - clip_dist, $
         	COLOR=green, LINE=2

         ;; Clip the FULL data set to retain only points that are < clip_dist
         ;; from line.
         all_residuals = *selected_dataset.y_filtered_data - $
         		 (M * *selected_dataset.x_filtered_data + B)

         index = where( abs(all_residuals) LT clip_dist, N )
         		
         print, 'Sigma clipping & fitting with ', N, ' datapoints...'
         xdata = (*selected_dataset.x_filtered_data)[index]
         ydata = (*selected_dataset.y_filtered_data)[index]
         
         ;; Perform fit on clipped data.
         parameters = linfit( xdata, ydata, /DOUBLE, YFIT=yfit, $
         		      SIGMA=sigma, CHISQ=chisq, PROB=prob )
         B = parameters[0]
         M = parameters[1]
         
         ;; Show errors from first fit.
         if (ii EQ 0) then begin
          txt = ['Fit parameters (and their errors) from FIRST call to LINFIT:', $
            string(M,sigma[1],f='("  m = ",G10.4," (",G10.4,")")'), $
            string(B,sigma[0],f='("  b = ",G10.4," (",G10.4,")")'), $
            string(chisq,f='(" CHISQ = ",G10.4)'), $
            string(prob, f='(" PROB  = ",G10.4)')]
          print, txt, FORMAT='(A)'
         endif

         ;; Use residuals from fitted data to define clipping boundaries.
         fit_residuals  = ydata - yfit
         clip_dist      = 2*stddev(fit_residuals,/DOUBLE)
       endfor
       
       ;; Compute the stddev of the final fit residuals.
       sigma_residual = stddev(fit_residuals,/DOUBLE)
       

       oplot, *(*st).fit.x, B + M * (*(*st).fit.x), COLOR=green
       
       *(*st).fit.f_of_x = B + M * (*(*st).fit.x)
       (*st).fit.name    = string((*st).fit.number, selected_dataset.name,$
       				  F='("fit ",I0," to ",A)' )
       (*st).fit.number  = (*st).fit.number + 1

       txt = ['MODEL is: Y = mX + b','Final fit parameters:', $
              string(M,f='("  m = ",G10.4)'), string(B,f='("  b = ",G10.4)'),$
              'Standard deviation of fit residuals is '+string(sigma_residual)]
          
       TimedMessage, msg_id, txt, GROUP=top_base, POS=top_base, $
       		     TIT='Results from routine LINFIT with sigma-clipping'
       print, txt, FORMAT='(A)'
       tara_clipboard, POST=string(M,B,sigma_residual)
     endif
     end

    ;----------------------------------------------------------------------
    9: $ ;Combine Two Densities
     begin
     plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis, UNITY_ASP=unity_aspect
     align_images, XTITLE=xaxis.title, YTITLE=yaxis.title, UNITY_ASP=unity_aspect
     end
     
    ;----------------------------------------------------------------------
    10: $ ;Blink Datasets
     begin 
       ;; Find the unhidden datasets.
       indexes = where( ((*st).datasets.hidden_flag EQ 0) AND $
       		       (*st).datasets.name NE '', count )
     
       if (count GE 2) then begin
         widget_control, /HOURGLASS
         xinteranimate, /CLOSE
         xinteranimate, /SHOWLOAD, SET=[!D.X_SIZE, !D.Y_SIZE, count], $
         		MPEG_QUALITY=100
         
	 names  = (*st).datasets[indexes].name
         for ii = 0, count-1 do begin
	  (*st).selected_name = names[ii]
	  (*st).stale_titles  = 1
	  RedrawDataset2d, st
           
          xinteranimate, FRAME=ii, WINDOW=!D.WINDOW
         endfor
         
	 xinteranimate, 5
       endif else print, 'Must have two or more unhidden datasets.'
     end

    ;----------------------------------------------------------------------
    else:
   endcase
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
       RedrawDataset2d, st
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
     
    2: $ ; SAVE  DENSITY
     begin
     name = strcompress( selected_dataset.name + '.fits', /REMOVE_ALL )
     pathname = dialog_pickfile( GROUP=top_base, FILE=name(0), $
				 TITLE='Save Density (FITS)' )
     widget_control, /HOURGLASS
     
     if (pathname NE '') then begin
       ; Make sure we can write to this file.
       openw, Unit, pathname, /GET_LUN, ERROR=Error
       if (Error NE 0) then begin
         msg = 'ERROR opening file '+ pathname
         dummy=dialog_message(msg, DIALOG_PARENT=(*st).file_menu)
       endif else begin
         free_lun, Unit
         
         plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis
         
           ;; Make sure the density is computed, then save the dataset 
           ;; structure, which may have been modified.
           ComputeDataset2dDensity, selected_dataset, st
           Save2dDataset, selected_dataset, st
           
           ; Make up a FITS header.
           get_date, date_today
           mkhdr,    fits_header, *selected_dataset.density.samples, /EXTEND
           sxaddpar, fits_header, "CREATOR", "dataset_2d, $Revision: 4411 $"
           sxaddpar, fits_header, 'ORIGIN',  'Penn State University'
           sxaddpar, fits_header, 'FITSVERS',  'IDL Astronomy Users Library'
	   fdecomp, pathname, disk, item_path, item_name, item_qual
	   if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
	   sxaddpar, pheader, "FNFITS", item_name
	   sxaddpar, fits_header, 'DATE', date_today
       
           ; Assign appropriate values to the FITS keywords 
           ; CRVALn, CRPIXn, & CDELTn which define a data coordinate
	   ; system in terms of the 1-based "pixel index" coordinates of the 
	   ; array stored in the file.
	   ;
	   ; (data - CRVALn) = CDELTn * (pixel - CRPIXn)
	   ;
	   ; If there is not a world coordinate system defined for the data
	   ; axes, then we just want the FITS keywords to allow translation
	   ; from  image pixel indexes to the data coordinate system.
	   ; For example if one of our axes is event energies in eV and we've
	   ; binned by 3.5 eV, then we'll use the FITS keywords to convert the
	   ; integer-valued array index back to real-valued eV units.
	   ;
	   ; If however we do have world coordinates defined for the data axes,
	   ; then we want the FITS keywords to allow translation from image
	   ; pixel indexes directly to those world coordinates (e.g. to RA/DEC).
	   ; The ability to recover the actual data coordinates (the numbers
	   ; we've been binning up in this tool) will be lost.
	   d   = selected_dataset.density
	   bin = selected_dataset.binning
	   wcs_object, xaxis.wcs, IS_NULL=is_null_x
	   wcs_object, yaxis.wcs, IS_NULL=is_null_y
	   fmt = '(G20.14)'
	   if (is_null_x OR is_null_y) then begin
	     sxaddpar, fits_header, 'CTYPE1', xaxis.title
             sxaddpar, fits_header, 'CTYPE2', yaxis.title
             sxaddpar, fits_header, 'CRPIX1', 1.0
             sxaddpar, fits_header, 'CRPIX2', 1.0
             sxaddpar, fits_header, 'CRVAL1', d.x0, F=fmt
             sxaddpar, fits_header, 'CRVAL2', d.y0, F=fmt
             sxaddpar, fits_header, 'CDELT1', bin.delta_x, F=fmt
             sxaddpar, fits_header, 'CDELT2', bin.delta_y, F=fmt
           endif else begin
             wcs_object, [xaxis.wcs,yaxis.wcs], CTYP=ctyp, CRVL=crvl, $
						CRPX=crpx, CDLT=cdlt, /GET
	     ; We need to calculate the coordinates of the fiducial point
	     ; in the pixel index coordinate system, given it's location in
	     ; the data coordinate system.  Then, add 1 because FITS counts
	     ; pixels starting at 1 not 0.
	     new_crpx = 1 + (crpx - [d.x0,d.y0]) / [bin.delta_x,bin.delta_y]
	     sxaddpar, fits_header, 'CTYPE1', ctyp[0]
	     sxaddpar, fits_header, 'CTYPE2', ctyp[1]
             sxaddpar, fits_header, 'CRPIX1', new_crpx[0], F=fmt
             sxaddpar, fits_header, 'CRPIX2', new_crpx[1], F=fmt
             sxaddpar, fits_header, 'CRVAL1', crvl[0], F=fmt
             sxaddpar, fits_header, 'CRVAL2', crvl[1], F=fmt
             sxaddpar, fits_header, 'CDELT1', cdlt[0] * bin.delta_x, F=fmt
             sxaddpar, fits_header, 'CDELT2', cdlt[1] * bin.delta_y, F=fmt
           endelse
           
           sxaddpar, fits_header, 'BUNIT', 'Samples of Density Function'

           writefits, pathname, *d.samples, fits_header
	   msg='Wrote FITS image to ' + pathname
          widget_control, (*st).msg_label, SET_VALUE=msg
       endelse ; (Error EQ 0)
     endif ; (pathname NE '')
     end
     
    3: $ ; SAVE DATASET
     begin
     name = strcompress( selected_dataset.name + '.fits', /REMOVE_ALL )
     pathname = dialog_pickfile( GROUP=top_base, FILE=name(0), $
				 TITLE='Save Dataset (FITS)' )
     widget_control, /HOURGLASS
     
     if (pathname NE '') then begin
       ; Make sure we can write to this file.
       openw, Unit, pathname, /GET_LUN, ERROR=Error
       if (Error NE 0) then begin
         msg = 'ERROR opening file '+ pathname
         dummy=dialog_message(msg, DIALOG_PARENT=(*st).file_menu)
       endif else begin
         free_lun, Unit
                             
         ; Make up a primary FITS header.
         get_date, date_today
         mkhdr, pheader, '', /EXTEND
         sxaddpar, pheader, "CREATOR", "dataset_2d, $Revision: 4411 $"
	 sxaddpar, pheader, 'ORIGIN',   'Penn State University'
	 sxaddpar, pheader, "FITSVERS", "IDL Astronomy Users Library"
	 fdecomp, pathname, disk, item_path, item_name, item_qual
	 if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
	 sxaddpar, pheader, "FNFITS", item_name
	 sxaddpar, pheader, 'DATE',     date_today
	 writefits, pathname, no_data, pheader
	 
         sxaddpar, fits_header, "EXTNAME", selected_dataset.name
         sxaddpar, fits_header, "CREATOR", "dataset_2d, $Revision: 4411 $"
	 sxaddpar, fits_header, 'ORIGIN',   'Penn State University'
	 sxaddpar, fits_header, 'DATE',     date_today
	 
	 plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis
	 sxaddpar, fits_header, 'XTITLE', xaxis.title  
	 sxaddpar, fits_header, 'YTITLE', yaxis.title  
	 

	 bin_table = replicate( {X: 0.0, Y:0.0}, $
	   			  selected_dataset.num_data_points )


         bin_table.X        = *selected_dataset.x_filtered_data
         bin_table.Y        = *selected_dataset.y_filtered_data
         
         mwrfits, bin_table, pathname, fits_header            
	 msg='Wrote dataset to ' + pathname
         widget_control, (*st).msg_label, SET_VALUE=msg
       endelse ; (Error EQ 0)
     endif ; (pathname NE '')
     end
          
    4: $ ; LOAD DATASET
     begin
     txt=['If you choose a FITS FILE, then its first extension must be',$
          'a binary table.  The first two columns of the table are assumed',$
          'to contain the bivariate dataset.','',$
          'If you choose a ASCII FILE, then you will be presented with',$
          'the ASCII_TEMPLATE tool built into IDL.  Fill out the',$
   	  'three forms in this tool so that the first two fields extracted',$
   	  'contain the bivariate dataset.']
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
       dataset_2d, top_base, table.(0), table.(1), DATASET_NAME=item_name
     endif
     widget_control, msg_id, /DESTROY, BAD_ID=bad
     end

     
    5: $ ; DELETE DATASET
     begin
     ;; We do NOT let the user delete the last dataset.
     dum = where( (*st).datasets.name NE '', count )
     if ((count NE 1) AND (NOT selected_dataset.disable_delete)) then begin
       (*st).datasets[ selected_index ].name = ''
       
       ; Select the dataset just prior to the deleted one.
       (*st).selected_name = (*st).datasets[0 > (selected_index-1)].name
       (*st).stale_titles = 1
       RedrawDataset2d, st
     endif
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
   
   l0 = '0, LABEL, SELECTED DATASET:'
   
   f1 = '0, TEXT,' + escape_commas( selected_dataset.name ) + $
	', TAG=name, LABEL_LEFT=  Name, WIDTH=15'

   str = (selected_dataset.name EQ selected_dataset.description) ? $
   	 '' : selected_dataset.description
   f2 = '0, TEXT,' + escape_commas( str )  + $
	', TAG=description, LABEL_LEFT=  Description, WIDTH=15'

   f3 = '2, DROPLIST, Show Dataset|Hide Dataset, SET_VALUE=' + $
   	string(selected_dataset.hidden_flag) + ', TAG=hidden_flag'

   form = [b0,l0,f1,f2,f3]
   

   ; NOTE PROPERTIES
   b0 = '1, BASE,, ROW, FRAME'

   l0 = '0, LABEL, NOTE:'

   f1 = '0, TEXT,' + escape_commas( (*st).note ) + $
        ', TAG=note, WIDTH=15, LABEL_LEFT=  Text'

   f2 = '0, FLOAT,' + string((*st).note_x) + $
	 ', TAG=note_x, LABEL_LEFT=  X, WIDTH=8'

   f3 = '2, FLOAT,' + string((*st).note_y) + $
	 ', TAG=note_y, LABEL_LEFT=Y, WIDTH=8'
    
   form = [form,b0,l0,f1,f2,f3]

   
   ; FILTER PROPERTIES
   roi_style = widget_info( (*st).roi_style, /DROPLIST_SELECT )
   
   b0 = '1, BASE,, ROW, FRAME'

   l0 = '0, LABEL, ROI:'
   
   f2 = '0, DROPLIST, Hide ROI|Show ROI, SET_VALUE=' + $
   	string((*st).show_roi) + ', TAG=show_roi'
 
   form = [form,b0,l0,f2]

   if (roi_style EQ 0 OR roi_style EQ 1) then begin
     f3 = '0, FLOAT,' + string((*st).roi_params.xl) + $
	   ', TAG=xl, LABEL_LEFT=Min X, WIDTH=8'
	 
     f4 = '0, FLOAT,' + string((*st).roi_params.xh) + $
	   ', TAG=xh, LABEL_LEFT=Max X, WIDTH=8'
	   
     form = [form,f3,f4]
   endif
   
   if (roi_style EQ 0 OR roi_style EQ 2) then begin
     f3 = '0, FLOAT,' + string((*st).roi_params.yl) + $
	   ', TAG=yl, LABEL_LEFT=Min Y, WIDTH=8'
	 
     f4 = '0, FLOAT,' + string((*st).roi_params.yh) + $
	   ', TAG=yh, LABEL_LEFT=Max Y, WIDTH=8'
	   
     form = [form,f3,f4]
   endif
   
   if (roi_style EQ 3) then begin
     b1 = '1, BASE,, COLUMN, FRAME'

     b2 = '1, BASE,, ROW'

     f3 = '0, FLOAT,' + string((*st).roi_params.center[0]) + $
	   ', TAG=xcen, LABEL_LEFT=Center: X, WIDTH=8'
	 
     f4 = '2, FLOAT,' + string((*st).roi_params.center[1]) + $
	   ', TAG=ycen, LABEL_LEFT=Y, WIDTH=8'
	   
     b3 = '1, BASE,, ROW'

     f5 = '0, FLOAT,' + string((*st).roi_params.rin) + $
	 ', TAG=rin, LABEL_LEFT=Radii: Inner, WIDTH=8'

     d1 = '0, DROPLIST, unlocked|locked, SET_VALUE=' + $
   	string((*st).roi_params.radii_lock) + ', TAG=radii_lock'

     f6 = '2, FLOAT,' + string((*st).roi_params.rout) + $
	 ', TAG=rout, LABEL_LEFT=Outer, WIDTH=8'
	 

     form = [form,b1,b2,f3,f4,b3,f5,d1,f6,'2, LABEL,  ','2, LABEL,  ']
   endif else begin
     f3 = '2, LABEL,  '
     form = [form,f3]
   endelse
   

   mode      = widget_info( (*st).mode_list, /DROPLIST_SELECT ) 

   if (mode NE 0) then begin
    b0 = '1, BASE,, COLUMN, FRAME'

    ; BINNING PROPERTIES
    b1 = '1, BASE,, COLUMN, FRAME'

    l0 = '0, LABEL, BINNING:, LEFT'

    b2 = '1, BASE,, ROW'

    f1a = '0, FLOAT,' + string(selected_dataset.binning.desired_delta_x) + $
	 ', TAG=desired_delta_x, LABEL_LEFT=Desired binsizes:  X, WIDTH=8'

    f1b = '2, FLOAT,' + string(selected_dataset.binning.desired_delta_y) + $
	 ', TAG=desired_delta_y, LABEL_LEFT=Y, WIDTH=8'

    l1 = '0, LABEL,Actual binsizes:  ' + $
    	 string(selected_dataset.binning.delta_x) + ' ' + $
    	 string(selected_dataset.binning.delta_y) + ''

    b3 = '1, BASE,, ROW'

    f2a = '0, FLOAT,' + string(selected_dataset.binning.xbin_location) + $
	 ', TAG=xbin_location, LABEL_LEFT=Location of any bin corner: X,WIDTH=8'

    f2b = '2, FLOAT,' + string(selected_dataset.binning.ybin_location) + $
	 ', TAG=ybin_location, LABEL_LEFT=Y, WIDTH=8'
 
    form = [form,b0,b1,l0,b2,f1a,f1b,l1,b3,f2a,f2b,'2, LABEL,  ']


    ; DENSITY PROPERTIES
    b0 = '1, BASE,, COLUMN, FRAME'
 
    l0 = '0, LABEL, DENSITY ESTIMATE:, LEFT'
    
    f0 = '0, DROPLIST, Histogram|Epanechnikov Smoothing|Gaussian Smoothing, '+$
          'SET_VALUE=' +string(selected_dataset.density.kernel)+ ', TAG=kernel'
  
    b3 = '1, BASE,, ROW'
 
    f3 = '0, DROPLIST, 4|6|8|10|12|14|16|18|20|22|24|26|28|30, ' + $
          'LABEL_LEFT=Epanechnikov kernel full-width (bins), '+$
          'SET_VALUE='+$
    	  string((selected_dataset.density.Epanechnikov_width-4)/2) + $
    	 ', TAG=Epanechnikov_width'
  
    f4 = '2, FLOAT,' + string(selected_dataset.density.Gaussian_sigma) + $
 	  ', TAG=Gaussian_sigma, LABEL_LEFT=Gaussian kernel sigma (bins),WIDTH=8'
    form = [form,b0,l0,f0,b3,f3,f4,'2, LABEL,  ']
        
    f5 = '2, DROPLIST, This Dataset|All Datasets,'+$
         'LABEL_LEFT=Parameters apply to:, '+$
         'SET_VALUE='+string((*st).share_binning_params) +$
         ', TAG=share_binning_params'

    form = [form,f5]
   endif ;(mode NE 0)
   
   
   case (mode) of
    0: $ ; SCATTER PLOT PROPERTIES
    begin
    b0 = '1, BASE,, ROW, FRAME'
    
    l0 = '0, LABEL, SCATTER PLOT:'
    
    f1 = '0, BUTTON, ' + $
         '+ Symbol|Asterisk|Dot|Diamond|Triangle|Box|X Symbol,'+$
	 'EXCLUSIVE, SET_VALUE=' + string(selected_dataset.plot.psym - 1) + $
	 ', TAG=psym, LABEL_TOP=Symbol Style'

    f1b= '0, TEXT,' + escape_commas( selected_dataset.plot.plotsym ) + $
	 ', TAG=plotsym, LABEL_TOP=plotsym params, WIDTH=15'

    ; We have to translate the field "color", which is a string, into
    ; an index into the list of all available colors.
    color_manager, COLOR_NAMES=color_names
    color_index = 0 > (where(selected_dataset.plot.color EQ color_names))[0]
    color_list  = string(color_names, F='(99(A,:,"|"))' )
 
    f2 = '0, BUTTON,' + color_list + ',EXCLUSIVE, SET_VALUE=' +$
 	  string(color_index) + ', TAG=color, LABEL_TOP=Color'

    f3 = '2, DROPLIST, Omit|Left|Center|Right, SET_VALUE=' + $
   	string((*st).legend_style) + ', LABEL_LEFT=Legend, TAG=legend_style'
   	    
    form = [form,b0,l0,f1,f1b,f2,f3]
    end
    
    1: $ ; IMAGE PROPERTIES
    begin
    b0 = '1, BASE,, ROW, FRAME'
    
    l0 = '0, LABEL, IMAGE:'
    
    f0 = '0, DROPLIST, Linear Axis | Logarithmic Axis, SET_VALUE=' + $
      	  string(selected_dataset.log_flag) + ',TAG=log_flag'
         
    f1 = '0, FLOAT,' + string(selected_dataset.tvlow) + $
	 ', TAG=llimit, LABEL_LEFT=low limit, WIDTH=8'
	 
    f2 = '0, FLOAT,' + string(selected_dataset.tvhigh) + $
	 ', TAG=hlimit, LABEL_LEFT=high limit, WIDTH=8'
	 
    f3 = '0, DROPLIST, This Dataset|All Datasets,'+$
         'LABEL_LEFT=Parameters apply to:, '+$
         'SET_VALUE='+string((*st).share_tv_params) +', TAG=share_tv_params'
         
    f4 = '2, DROPLIST, Omit|Show, SET_VALUE=' + $
   	string((*st).color_bar) + ',LABEL_LEFT=Colorbar,TAG=color_bar'
   	    
    form = [form,b0,l0,f0,f1,f2,f3,f4]
    end
    
    2: $ ; SURFACE PLOT PROPERTIES
    begin
    b0 = '1, BASE,, ROW, FRAME'

    l0 = '0, LABEL, SURFACE PLOT:'
   
    f1 = '2, DROPLIST, Wire Mesh|Lego Style|Shaded, SET_VALUE=' + $
   	string((*st).surface_mode) + ', TAG=surface_mode'
   	
    form = [form,b0,l0,f1]
    end
    
    3: $ ; CONTOUR PLOT PROPERTIES
    begin
    b0 = '1, BASE,, ROW, FRAME'

    l0 = '0, LABEL, CONTOUR PLOT:'
   
    ; We have to translate the field "color", which is a string, into
    ; an index into the list of all available colors.
    color_manager, COLOR_NAMES=color_names
    color_index = 0 > (where(selected_dataset.plot.color EQ color_names))[0]
    color_list  = string(color_names, F='(99(A,:,"|"))' )
 
    f1 = '0, BUTTON,' + color_list + ',EXCLUSIVE, SET_VALUE=' +$
 	  string(color_index) + ', TAG=color, LABEL_TOP=Color'

    f2 = '0, TEXT,' + escape_commas( (*st).contour_kwds ) + $
        ', TAG=contour_kwds, WIDTH=30, LABEL_LEFT=  Keywords for CONTOUR routine'
   	

    f3 = '2, TEXT,' + escape_commas( (*st).contour_regionfile ) + $
        ', TAG=contour_regionfile, WIDTH=30, LABEL_LEFT=  Save contours to region file:'
   	
    form = [form,b0,l0,f1,f2,f3]
    end
    
    4: $ ; CONTOUR ON IMAGE PROPERTIES
    begin
    end
  endcase


  ;; Run the form widget.
  f1 = '2,BUTTON,OK,QUIT,TAG=ok'

  r = cw_form( [form,f1], /COLUMN, TITLE='Properties' )
 
 
  ;; Extract the results
  selected_dataset.name        = r.name
  selected_dataset.description = r.description ? r.description : r.name
  selected_dataset.hidden_flag = r.hidden_flag
  (*st).stale_titles = 1
  (*st).selected_name= r.name
  
  (*st).note   = r.note
  (*st).note_x = r.note_x
  (*st).note_y = r.note_y
  
  (*st).show_roi = r.show_roi
  rp = (*st).roi_params
  
  if (roi_style EQ 0 OR roi_style EQ 1) then begin
    (*st).roi_params.xl = r.xl
    (*st).roi_params.xh = r.xh
  endif
  
  if (roi_style EQ 0 OR roi_style EQ 2) then begin
    (*st).roi_params.yl = r.yl
    (*st).roi_params.yh = r.yh
  endif
  
  if (roi_style EQ 3) then begin
    (*st).roi_params.center[0] = r.xcen
    (*st).roi_params.center[1] = r.ycen
    (*st).roi_params.rin  = r.rin
    (*st).roi_params.rout = r.rout
    (*st).roi_params.radii_lock = r.radii_lock
  endif
  
  ; If we've changed the ROI, then mark ALL the datasets as stale.
  if ((compare_struct(rp, (*st).roi_params))[0].ndiff GT 0) then begin
    (*st).datasets.stats_stale  = 1
    selected_dataset.stats_stale= 1
   
    if (roi_mode EQ 2) then begin
      (*st).datasets.filter_stale = 1
      selected_dataset.filter_stale=1
      new_event = { ID:top_base, TOP:event.top, HANDLER:0L}
    endif
  endif

  
  if (mode NE 0) then begin
    (*st).share_binning_params = r.share_binning_params

    ; Binning parameters.
    b=selected_dataset.binning
    selected_dataset.binning.desired_delta_x = r.desired_delta_x
    selected_dataset.binning.desired_delta_y = r.desired_delta_y
    selected_dataset.binning.xbin_location   = r.xbin_location
    selected_dataset.binning.ybin_location   = r.ybin_location
    
    if ((compare_struct(b,selected_dataset.binning))[0].ndiff GT 0) then begin
      selected_dataset.density.stale  = 1B
      
      if (*st).share_binning_params then begin
        (*st).datasets.binning.desired_delta_x    = r.desired_delta_x
        (*st).datasets.binning.desired_delta_y    = r.desired_delta_y
        (*st).datasets.binning.xbin_location      = r.xbin_location
        (*st).datasets.binning.ybin_location      = r.ybin_location
        (*st).datasets.density.stale              = 1B
      endif
    endif

     ; Density parameters.
     d=selected_dataset.density
     selected_dataset.density.kernel              = r.kernel
     selected_dataset.density.Epanechnikov_width  = 4 + 2*r.Epanechnikov_width
     selected_dataset.density.Gaussian_sigma      = r.Gaussian_sigma

     if ((compare_struct(d,selected_dataset.density))[0].ndiff GT 0) then begin
      selected_dataset.density.stale  = 1B
      
      if (*st).share_binning_params then begin
        (*st).datasets.density.kernel             = r.kernel
        (*st).datasets.density.Epanechnikov_width = 4 + 2*r.Epanechnikov_width
        (*st).datasets.density.Gaussian_sigma     = r.Gaussian_sigma
        (*st).datasets.density.stale              = 1B
      endif
     endif
  endif ;(mode NE 0)
  
  case mode of
   0: begin
      selected_dataset.plot.psym    = r.psym+1
      selected_dataset.plot.plotsym = r.plotsym
      
       ; We have to translate "result.color", which is an index into
       ; the corresponding color name (string).
       selected_dataset.plot.color = color_names[r.color]
      
       (*st).legend_style = r.legend_style
      end
      
   1: begin
      (*st).share_tv_params = r.share_tv_params
      
      ; If just the log flag is changed, then we want to recalc tv limits
      ; using the relative scaling we have now (scale_midpt,scale_width).
      if (selected_dataset.log_flag NE r.log_flag) then begin
        selected_dataset.log_flag = r.log_flag
        selected_dataset.tv_stale = 1
      
        if (*st).share_tv_params then begin
          (*st).datasets.log_flag = r.log_flag
          (*st).datasets.tv_stale = 1
        endif
      endif

      ; But if tv limits are also changed, then we want to respect them
      ; and instead recalc scale params.
      if ((selected_dataset.tvlow  NE r.llimit) OR $
          (selected_dataset.tvhigh NE r.hlimit)) then begin
        selected_dataset.tvlow      = r.llimit
        selected_dataset.tvhigh     = r.hlimit
        selected_dataset.scale_stale= 1
        selected_dataset.tv_stale   = 0
      
        if (*st).share_tv_params then begin
          (*st).datasets.tvlow      = r.llimit
          (*st).datasets.tvhigh     = r.hlimit
          (*st).datasets.scale_stale= 1
          (*st).datasets.tv_stale   = 0
        endif
      endif
       
      (*st).color_bar         = r.color_bar
      end
      
   2: begin
      (*st).surface_mode = r.surface_mode
      end     
       
   3: begin
      
       ; We have to translate "result.color", which is an index into
       ; the corresponding color name (string).
       selected_dataset.plot.color = color_names[r.color]
      
      (*st).contour_kwds       = strtrim(r.contour_kwds,2)
      (*st).contour_regionfile = strtrim(r.contour_regionfile,2)
      end   
       
   4: begin
      end   
  endcase
  
   ; Save the dataset structure we've modified.
   Save2dDataset, selected_dataset, st
   RedrawDataset2d, st
   end




;--------------------------------------------------------------------------
; Stats/Filter droplist
  (*st).roi_mode: $
   begin
   ; We can not tell what transition roi_mode has just made, so we have no
   ; choice but to mark all filtered datasets and all statistics as stale.
   (*st).datasets.filter_stale = 1B
   (*st).datasets.stats_stale  = 1B
   new_event = { ID:top_base, TOP:event.top, HANDLER:0L}
   RedrawDataset2d, st
   end


;--------------------------------------------------------------------------
; Filter style droplist
  (*st).roi_style: $
   begin
   ; If ROI is active then statistics for all datasets are now stale.
   if (roi_mode NE 0) then (*st).datasets.stats_stale  = 1B
   
   ; If ROI filtering is active then all filtered datasets are now stale.
   if (roi_mode EQ 2) then begin
     (*st).datasets.filter_stale = 1B
     new_event = { ID:top_base, TOP:event.top, HANDLER:0L}
   endif

   if (roi_mode NE 0) then RedrawDataset2d, st
   end


;--------------------------------------------------------------------------
; Filter Define button
  (*st).roi_define: $
   begin
   ; Use the plot_window's markers to define a ROI.
   plot_window, (*st).pw_id, BIG_MARKER=big_marker, SMALL_MARKER=small_marker

   case (widget_info( (*st).roi_style, /DROPLIST_SELECT )) of
   
   1: $ ; X-range
    begin
    (*st).roi_params.xl = big_marker[0] < small_marker[0]
    (*st).roi_params.xh = big_marker[0] > small_marker[0]
    end
  
   2: $ ; Y-range
    begin
    (*st).roi_params.yl = big_marker[1] < small_marker[1]
    (*st).roi_params.yh = big_marker[1] > small_marker[1]
    end
  
   0: $ ; box
    begin
    (*st).roi_params.xl = big_marker[0] < small_marker[0]
    (*st).roi_params.xh = big_marker[0] > small_marker[0]
    (*st).roi_params.yl = big_marker[1] < small_marker[1]
    (*st).roi_params.yh = big_marker[1] > small_marker[1]
    end
  
   3: $ ; annulus
    begin
    (*st).roi_params.center = big_marker
    if ((*st).roi_params.radii_lock EQ 0) then $
      (*st).roi_params.rout = sqrt( total( (big_marker - small_marker)^2 ) )
    end
   endcase
   
   ; If ROI is active then statistics for all datasets are now stale.
   if (roi_mode NE 0) then (*st).datasets.stats_stale  = 1B
   
   ; If ROI filtering is active then all filtered datasets are now stale.
   if (roi_mode EQ 2) then begin
     (*st).datasets.filter_stale = 1B
     new_event = { ID:top_base, TOP:event.top, HANDLER:0L}
   endif

   if (roi_mode NE 0) then RedrawDataset2d, st
   end


;--------------------------------------------------------------------------
; Filter mode droplist
  (*st).roi_semantics: $
   begin
   ; If ROI is active then statistics for all datasets are now stale.
   if (roi_mode NE 0) then (*st).datasets.stats_stale  = 1B
   
   ; If ROI filtering is active then all filtered datasets are now stale.
   if (roi_mode EQ 2) then begin
     (*st).datasets.filter_stale = 1B
     new_event = { ID:top_base, TOP:event.top, HANDLER:0L}
   endif

   if (roi_mode NE 0) then RedrawDataset2d, st
   end


;--------------------------------------------------------------------------
; COLOR TABLE BUTTON
  (*st).color_table_button: $
   begin
   color_manager, NCOLORS=ncolors
   xloadct, GROUP=top_base, NCOLORS=ncolors
   end
      
;--------------------------------------------------------------------------
; SCALING / PERSPECTIVE BUTTON
  (*st).scaling_button: $
   begin
   ;; Make sure the correct X window is active,
   plot_window, (*st).pw_id, DRAW_WIDGET=draw_widget, XAXIS=xaxis, YAXIS=yaxis
       
   coordsystem_manager, draw_widget, /RESTORE
   device, CURSOR_STANDARD=66
  
   mode = widget_info( (*st).mode_list, /DROPLIST_SELECT )

   if (mode EQ 1) then begin
     ;; Convert the current tvlow/tvhigh values to scale_midpt/scale_width
     ;; values.
     selected_dataset.scale_stale = 1
     TvDataset2d, selected_dataset, st, xaxis, yaxis, /VERBOSE, $
     		  SAVED_STATS=saved_stats
     
     scale_midpt = 0 > selected_dataset.scale_midpt < 1
     scale_width = 0 > selected_dataset.scale_width < 1

     ;; Give some instructions
     prompt = ['Move the mouse to rescale.',$
     	'Horizontal position <=> location of "grey" within data range.', $
     	'Vertical position   <=> % of data range between "white" & "black".',$
     	'Click when finished']
     TimedMessage, msg_id, prompt, GROUP=top_base, /NO_QUIT
  
     ;; Position the pointer to reflect these scale_midpt/scale_width values.
     tvcrs, scale_midpt, scale_width, /NORM
  
     ;; Handle the motion events.
     repeat begin
       event = widget_event( draw_widget )
       if (event.type EQ 0) then break
  
       pt = convert_coord( event.x, event.y, /DEVICE, /TO_NORM )
       selected_dataset.scale_midpt = 0.0   > (pt(0) - 0.05)/.9 < 1.0  
       selected_dataset.scale_width = 0.001 > (pt(1) - 0.05)/.9 < 1.0
       selected_dataset.tv_stale    = 1
       
       TvDataset2d, selected_dataset, st, xaxis, yaxis, /VERBOSE, $
     		    SAVED_STATS=saved_stats
     endrep until 0
  
     ;; Clear any remaining events from the draw widget.
     widget_control, draw_widget, /CLEAR_EVENTS
     widget_control, msg_id, /DESTROY, BAD_ID=bad_id
     
     ; Save the dataset structure we've modified.
     Save2dDataset, selected_dataset, st
     TvDataset2d, selected_dataset, st, xaxis, yaxis, /VERBOSE, $
       		    COLORBAR=(*st).color_bar, /ERASE
     
   endif else if (mode EQ 2) then begin
     ;; Give some instructions
     prompt = ['Move the mouse to rotate surface plot.',$
     	'Horizontal position <=> rotation about X axis', $
     	'Vertical position   <=> rotation about Z axis.',$
     	'Click when finished']
     TimedMessage, msg_id, prompt, GROUP=top_base, /NO_QUIT
  
     ;; Position the pointer.
     tvcrs, (*st).x_axis_rotation/90., (*st).z_axis_rotation/360., /NORM
  
     ;; Handle the motion events.
     repeat begin
       event = widget_event( draw_widget )
       if (event.type EQ 0) then break
  
       pt = convert_coord( event.x, event.y, /DEVICE, /TO_NORM )
       (*st).x_axis_rotation = pt[0] * 90  
       (*st).z_axis_rotation = pt[1] * 360
  
       RedrawDataset2d, st
     endrep until 0
  
     ;; Clear any remaining events from the draw widget.
     widget_control, draw_widget, /CLEAR_EVENTS
     widget_control, msg_id, /DESTROY, BAD_ID=bad_id
     
   endif 
   device, /CURSOR_ORIGINAL
   end

  else: print, 'unknown event in dataset_2d'
endcase


if DestroyFlag then widget_control, (*st).parent, /DESTROY

return, new_event
end



;==========================================================================
;;; Event Handler Procedure
;==========================================================================
PRO Dataset2dEvent, Event
 
event = Dataset2dEventFn( Event )
return
end


;==========================================================================
;;; MAIN "dataset_2d" ROUTINE
;==========================================================================

PRO dataset_2d, top_base, x_data, y_data, WEIGHT=weight, $
		 _EXTRA=extra, TITLE=title, $
		 XTITLE=xtitle, YTITLE=ytitle, UNITY_ASPECT=unity_aspect, LEGEND_STYLE=legend_style, $
		 XWCS=xwcs, YWCS=ywcs, NAN_VALUES=nanval, $
		 DATASET_NAME=dataset_name, DESCRIPTION=description, $
		 XBIN=xbin, YBIN=ybin, XEDGE=xedge, YEDGE=yedge, $
 		 COLOR=color, PSYM=psym, PLOTSYM=plotsym,$
		 HIDE_DATASET=hide_dataset, DISABLE_DELETE=disable_delete, $
		 DELETE=delete, ROI_MASK=mask, REDRAW=redraw, $
		 GET_DATASETS=get_datasets, $
     PLOT_WINDOW_OPTIONS=plot_window_options, PS_CONFIG=ps_config, PRINT=print_now

		 
;; If the widget ID of an existing dataset_2d was not passed, then create 
;; the widget.
create_flag = 1
if (n_elements(top_base) EQ 1) then begin
  if (widget_info( top_base, /VALID_ID )) then create_flag = 0
endif

if (create_flag) then top_base = CreateDataset2d( _STRICT_EXTRA=extra )


;;---------------------------------------------------------------------------
;; Get the state structure.
widget_control, top_base, GET_UVALUE=st


if keyword_set(plot_window_options) then begin
  dum = execute('plot_window, (*st).pw_id,'+plot_window_options)
endif

if keyword_set(ps_config)          then *(*st).ps_config = ps_config

if (0 NE n_elements(legend_style)) then (*st).legend_style = legend_style


;; If we just created the widget and no data was passed, then we want to
;; return to avoid creating a dataset named "x vs y".
if (create_flag AND (N_ELEMENTS(y_data) EQ 0)) then return

;;---------------------------------------------------------------------------
;; Return all the datasets if desired, with the selected one first.
if arg_present(get_datasets) then begin
  indexes = where((*st).datasets.name NE '', count )

  get_datasets = 0
  if (count GT 0) then begin
    get_datasets   = (*st).datasets[indexes]
    selected_index = (where( get_datasets.name EQ (*st).selected_name, count))[0]
    if (count GT 0) then begin
      temp = get_datasets[0]
      get_datasets[0] = get_datasets[selected_index]
      get_datasets[selected_index] = temp
    endif
  endif
  return
endif


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
  if (0 EQ n_elements(ytitle)) then ytitle = keyword_set(y_param_name) ? y_param_name : 'Y'
endif

if (0 NE n_elements(xtitle))   then plot_window, (*st).pw_id, XTITLE=xtitle
if (0 NE n_elements(ytitle))   then plot_window, (*st).pw_id, YTITLE=ytitle
if keyword_set(unity_aspect)   then plot_window, (*st).pw_id, /FORCE_UNITY_ASP

if (0 NE n_elements(title))    then begin
  (*st).title=title
  (*st).stale_titles = 1
endif

redraw_flag = 0

;;---------------------------------------------------------------------------
;; Find the specified dataset or a spot to put a new one.
if keyword_set(dataset_name) then begin
  ; A name was passed.
endif else if (N_ELEMENTS(x_data) NE 0) then begin
  ; Data was passed, without a name; use a default name.
  if (keyword_set(x_param_name) AND keyword_set(y_param_name)) then $
    dataset_name = y_param_name+' vs. '+x_param_name $
  else $
    dataset_name = '2D dataset'
endif else begin
  ; Neither a name nor data were passed.  Assume the caller is refering to the last dataset.
  ind = where( (*st).datasets.name NE '', count )
  
  dataset_name = (count EQ 0) ? '2D dataset' : ((*st).datasets.name)[ind[count-1]]
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
  ;; Store new X-Y data values if supplied.  Deal with NaN.

  if (N_ELEMENTS(y_data) NE 0) then begin
    dataset.weight_available = (n_elements(weight) GT 0)

    is_finite = finite(x_data) AND finite(y_data)
    if dataset.weight_available then is_finite = is_finite AND finite(weight)

    good_ind = where(is_finite, num_data_points, $
				COMPLEMENT=null_ind, NCOMPLEMENT=num_null)
    dataset.num_data_points = num_data_points > 1


    if            (num_null EQ 0)               then begin
      ; All the data are finite
      *dataset.x_data = x_data
      *dataset.y_data = y_data
      if dataset.weight_available then *dataset.w_data = weight
      
    endif else if (n_elements(nanval) EQ 2) then begin
      ; Replace NaN/Inf with supplied values
      *dataset.x_data = x_data
      *dataset.y_data = y_data

      (*dataset.x_data)[null_ind] = nanval[0]
      (*dataset.y_data)[null_ind] = nanval[1]
      if dataset.weight_available then (*dataset.w_data)[null_ind] = nanval[2]     
    
    endif else if (num_data_points GT 0) then begin
      ; Keep just the good data
      *dataset.x_data = x_data[good_ind]
      *dataset.y_data = y_data[good_ind]
      if dataset.weight_available then *dataset.w_data = weight[good_ind]
      
    endif else begin
      ; There are no good data points
      print, 'dataset_2d: WARNING! All data are Nan/Inf'
      *dataset.x_data = [0]
      *dataset.y_data = [0]
      if dataset.weight_available then *dataset.w_data = [0]
    endelse
    
    if dataset.weight_available then begin
      widget_control, (*st).mode_list, SET_DROPLIST_SEL=1
      widget_control, (*st).scaling_button, SET_VALUE='Image Scaling  '
    endif 
    
    dataset.filter_stale = 1B
    dataset.stats_stale  = 1B
  endif 
  
  ;;---------------------------------------------------------------------------
  ;; If this is a new dataset, then we want to compute default values for
  ;; several parameters.
  if new_dataset_flag then begin
    dataset.name           	= dataset_name
    dataset.description     	= dataset_name
    dataset.hidden_flag     	= 0
    dataset.disable_delete 	= 0
    dataset.plot.psym		    = 3B
    dataset.plot.plotsym		  = ''
    dataset.scale_midpt      = 0.25
    dataset.scale_width      = 0.5
    dataset.tv_stale		= 1
        
    (*st).stale_titles = 1
  endif


;;---------------------------------------------------------------------------
;; We delete a dataset merely by setting its name to ''.
if keyword_set(delete) then begin
    dataset.name = ''
    *dataset.x_data = [0]
    *dataset.y_data = [0]
    dataset.num_data_points = 1
endif
  
  
;;---------------------------------------------------------------------------
;; Handle some more keywords that modify a dataset.
if (0 NE n_elements(description))   then dataset.description = description
if (0 NE n_elements(disable_delete)) then $
    dataset.disable_delete = disable_delete 

if (n_elements(xbin) NE 0) then begin
  dataset.binning.desired_delta_x = abs(xbin)
  dataset.filter_stale = 1B
endif

if (n_elements(ybin) NE 0) then begin
  dataset.binning.desired_delta_y = abs(ybin)
  dataset.filter_stale = 1B
endif

if (n_elements(xedge) NE 0) then begin
  dataset.binning.xbin_location = xedge
  dataset.filter_stale = 1B
endif

if (n_elements(yedge) NE 0) then begin
  dataset.binning.ybin_location = yedge
  dataset.filter_stale = 1B
endif

if (0 NE n_elements(psym))  then begin
  dataset.plot.psym  = 1> psym <8
  widget_control, (*st).mode_list, SET_DROPLIST_SEL=0
  widget_control, (*st).scaling_button, SET_VALUE='               '
endif

if (0 NE n_elements(plotsym))  then begin
  dataset.plot.plotsym  = plotsym
  widget_control, (*st).mode_list, SET_DROPLIST_SEL=0
  widget_control, (*st).scaling_button, SET_VALUE='               '
endif

if (0 NE n_elements(color)) then dataset.plot.color = color


;;---------------------------------------------------------------------------
;; Decide if we should redraw by considering the new_dataset_flag, the 
;; existing state of dataset.hidden_flag and the keyword hide_dataset.
if (new_dataset_flag) then begin
  ; Dataset is new; is HIDE_DATASET specified?
  if keyword_set(hide_dataset) then begin
    dataset.hidden_flag = 1
    redraw_flag = 0
  endif else begin
    (*st).selected_name = dataset.name
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
  ApplyDataset2dRoi, dataset, st
  mask = dataset.mask
  redraw_flag = 0
endif


;;---------------------------------------------------------------------------
;; Store the dataset structure we've been modifying.
Save2dDataset, dataset, st


;;---------------------------------------------------------------------------
;; The REDRAW keyword overrides redraw_flag calculated above.
if (n_elements(redraw) EQ 1) then begin
  (*st).selected_name = dataset.name
  redraw_flag = redraw
endif

if (widget_info(top_base, /REALIZED) AND redraw_flag) then RedrawDataset2d, st

;;---------------------------------------------------------------------------
;; Print without user intervention, if directed.
if keyword_set(print_now) then begin
  (*st).print_now = 1
  event={ID:(*st).file_menu, TOP:top_base, HANDLER:top_base, VALUE:1}
  Dataset2dEvent, Event
endif

return
END
