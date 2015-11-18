;+
;========================================================================
;;;
;;; Dataset_3D Widget: $Id: dataset_3d.pro 4376 2012-10-30 20:17:27Z psb6 $
;;;
;;; Copyright (C) 1998, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION:  
;;; This tool may be used to analyze the distribution of 3D datasets.
;;;
;========================================================================
;;; EVENTS GENERATED
;;; An event is forwarded to the client whenever the dataset is filtered.
;;;
;========================================================================
;;; INTERFACE TO CLIENT
;;;
;;; dataset_3d, top_base, x_data, y_data, z_data,  $
;		 PARENT_WIDGET=parent, GROUP=group, BLOCK=block, $
;		 WIDGET_TITLE=widget_title,$
;                TITLE=title, $
;		 XTITLE=xtitle, YTITLE=ytitle, ZTITLE=ztitle, $
;		 UNITY_ASPECT=unity_aspect, LEGEND_STYLE=legend_style, $
;		 XWCS=xwcs, YWCS=ywcs, NAN_VALUES=nanval, $
;			   
;		 DATASET_NAME=dataset_name, DESCRIPTION=description, $
;		 XBIN=xbin, YBIN=ybin, TBIN=tbin, STAT_CODE=stat_code, $
;		 HIDE_DATASET=hide_dataset, DISABLE_DELETE=disable_delete, $
;		 DELETE=delete,  ROI_MASK=mask, REDRAW=redraw
;		 GET_DATASETS=get_datasets
;
;    PLOT_WINDOW_OPTIONS=plot_window_options, PS_CONFIG=ps_config, PRINT=print_now
;;;
;;; The parameters PARENT_WIDGET & GROUP are the usual ones associated 
;;; with widgets.  If dataset_3d is a top-level widget, then WIDGET_TITLE
;;; controls the title in the X-window frame.
;;;
;;; The dataset_3d can work with multiple named datasets.
;;; Multiple datasets are passed with multiple calls to dataset_3d -- see
;;; example below.
;;; A dataset may be deleted by the caller using /DELETE.


;;; The state of this widget is stored as an anonymous structure in the 
;;; user value slot of the first child (the normal convention).


;==========================================================================
;;;                     CALLING OPTIONS

;;; From a non-widget program or the command line, analyze vectors X & Y.
;;; Widget events will be processed when the command line returns.
;;; ** dataset_3d, id, X, Y

;;; From a non-widget program analyze vectors X & Y but block further
;;; program execution until the dataset_3d widget is destroyed.
;;; ** dataset_3d, id, X, Y, /BLOCK
;;; ** xmanager

;;; From a widget program, create a top-level dataset_3d.
;;; ** dataset_3d, id, X, Y, GROUP=creator_widget_id

;;; Create a dataset_3d as a child of another widget.
;;; ** dataset_3d, id, X, Y, PARENT_WIDGET=base

;;; Delete a dataset from an existing dataset_3d.
;;; ** dataset_3d, id, /DELETE, DATASET_NAME=name

;;; Obtain a pointer to a byte array that shows which data points in the 
;;; specified dataset are retained by the user's filter. If no filter
;;; is defined, then the pointer points to an undefined variable.
;;; ** dataset_3d, id, DATASET_NAME=name, ROI_MASK=mask
;==========================================================================
;-

;;; We need the dataset_2d program for Bin2dDataset routine.
@dataset_2d


;==========================================================================
;;; Create the widget.
;==========================================================================
FUNCTION CreateDataset3d, PARENT_WIDGET=parent, GROUP=group, BLOCK=block, $
			  WIDGET_TITLE=widget_title

;; Call color_manager to switch to DirectColor if available.
color_manager

;; we have to create a dummy one here so the /CLOSE later won't die.
xinteranimate, SET=[100,100,2]

;; Default values
if (0 EQ n_elements(group))          then group = 0
if (0 EQ n_elements(widget_title))   then widget_title = 'Dataset_3D'
  

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
			EVENT_FUNC='Dataset3dEventFn', $
			KILL_NOTIFY='Dataset3dCleanup', $
			/COLUMN, /SPACE, XPAD=0, YPAD=0, UNAME='dataset_3d' )

upper_base = widget_base( top_base, /ALIGN_LEFT, /ROW, $
			   SPACE=8, XPAD=0, YPAD=0 )
			   
 menu = [{ CW_PDMENU_S, flags:1, name:'File' }, $ 
	 { CW_PDMENU_S,     0,        'Print' }, $ 
	 { CW_PDMENU_S,     0,        'Save Density or Stat Map (FITS Image)' },$
	 { CW_PDMENU_S,   0,      'Save 3-D Dataset (FITS Table)' }, $
	 { CW_PDMENU_S,   0,      'Load 3-D Dataset (FITS Table or ASCII)' }, $
	 { CW_PDMENU_S,     0,        'Delete Dataset' }, $
	 { CW_PDMENU_S,     2,   'Select Animation Normalization File' }]

 if this_is_top_level then begin
   menu[6].flags = 0
   menu = [menu, { CW_PDMENU_S,     2,      'Exit' }]
 endif 
 
 file_menu = cw_pdmenu(upper_base, menu, /RETURN_INDEX)
     
 edit_button    = widget_button( upper_base, VALUE='Edit' )
 
 roi_base = widget_base(upper_base, /ROW, /SPACE, XPAD=0, YPAD=0, /FRAME)
 
   roi_mode = widget_droplist( roi_base, VALUE=['None','Stats','Filter'], $
   				  TITLE='ROI:' )
   
   styles = ['Cube','X-range','Y-range','Z-range']
   roi_style = widget_droplist( roi_base, VALUE=styles )
  
   roi_define = widget_button( roi_base, VALUE='Use Markers' )
  
   roi_semantics = widget_droplist( roi_base, VALUE=['Include','Exclude'] )

 menu = [{ CW_PDMENU_S, flags:1, name:'Analysis' }, $ 
	 { CW_PDMENU_S,0,   'Distribution of Density or Stat Map Values' },$
	 { CW_PDMENU_S,2,   'Combine Images (Difference, Ratio, RGB, etc.)' }]
	 
 derived_menu = cw_pdmenu(upper_base, menu, /RETURN_INDEX)


middle_base = widget_base( top_base, /ALIGN_LEFT, /ROW, $
			   /SPACE, XPAD=0, YPAD=0 )

 dataset_list = widget_droplist( middle_base, VALUE='NULL', /DYNAMIC_RESIZE )
 
 modes = ['Scatter Plot', 'Image Series', 'Surface Plot Series', $
 	  'Contour Plot Series', 'Statistic Map']
 mode_list = widget_droplist( middle_base, VALUE=modes )
 widget_control, mode_list, SET_DROPLIST_SELECT=1
 
 scaling_button = widget_button( middle_base, VALUE='Image Scaling  ' )

 color_table_button = widget_button( middle_base, VALUE='Color Table' )

lower_base = widget_base( top_base, /ALIGN_LEFT, /ROW, $
			   SPACE=10, XPAD=20, YPAD=0 )

 prev_frm = widget_button( lower_base, VALUE='<' )
 next_frm = widget_button( lower_base, VALUE='>' )
 frm_num  = cw_field(      lower_base, /INTEGER, TIT='Frm#', XSIZ=3, /RET )
 animate  = widget_button( lower_base, VALUE='Animate' )

msg_label = widget_label( top_base, /DYNAMIC_RESIZE, VALUE=' ' )
    
plot_window, pw_id, PARENT=top_base
plot_window, pw_id, SET_XMARGIN=[8,5]

; Setup state structure.
dataset = { index:0B, name:'', description:'', hidden_flag:0, disable_delete:0,$

	    x_data:ptr_new(), y_data:ptr_new(), $
	    z_data:ptr_new(),  $
	    
	    x_filtered_data:ptr_new(), y_filtered_data:ptr_new(), $
	    z_filtered_data:ptr_new(), $
	    mask:ptr_new(), filter_stale:1B, $
	    stats:'', stats_stale:1B, $
	    
	    ; num_data_points, x_max, x_min, etc. refer to the filtered dataset!
	    num_data_points:0L, $
	    z_max:1.0, z_min:0.0, y_max:1.0, y_min:0.0, x_max:1.0, x_min:0.0, $

	    plot: {color:'white', psym:3B}, $
	    
	    binning:   {desired_delta_x:0.0, desired_delta_y:0.0, $
			desired_delta_z:0.0, $
	    		delta_x:0.0, delta_y:0.0, $
	    		delta_z:0.0, $
	    		xbin_location:0.0, ybin_location:0.0, $
	    		zbin_location:0.0},$


	    		;;0=histogram, 1=Epanechnikov kernel, 2=Gaussian kernel
	    		;;Epanechnikov_width must be EVEN
	    		;;Gaussian sigma is in units of bins
	    density:   {kernel:0, Epanechnikov_width:4, Gaussian_sigma:1.0, $ 
	    		kernel_z:0,Epanechnikov_width_z:4,Gaussian_sigma_z:1.0,$ 
	    		bandwidth_x:1.0, bandwidth_y:1.0, bandwidth_z:1.0, $
	    		subtitle:'', $
	    		x0:0.0, y0:0.0, z0:0.0, xdim:0L, ydim:0L, zdim:0L, $
	    		samples:ptr_new(), stale:0B }, $
	    		
	    		;;For each stat map entry, the kernel radius will be
	    		;;enlarged until the group has the desired
	    		;;"significance" (counts/sqrt(counts)).
	    stat_map:  {min_significance:0.0, stat_code:0, $
	    		require_compatible_flux:0, $
	    		group_start:ptr_new(), group_stop:ptr_new(), $
	    		sorted_data:ptr_new(), $
	    		subtitle:'', $
	    		bandwidth_x:1.0, bandwidth_y:1.0, $
	    		x0:0.0, y0:0.0, xdim:0L, ydim:0L, $
	    		samples:ptr_new(), stale:0B, samples_completed:0L, $
	    		helper_unit:0L, kernel_descriptor_filename:'', kernel_descriptor_unit:0L,$
	    		kernel_descriptor_location:ptr_new() }, $

	    frame_index:0L, $
	    image:ptr_new(), log_flag:0, $
	    bgrnd_index:ptr_new(), fgrnd_index:ptr_new(), $
	    scale_midpt:0.25, scale_width:0.5, scale_stale:0B, $
	    tvlow:0.0, tvhigh:1.0, tv_stale:1B, tvnorm_file:'' }
	    
r2=[0.0,0.0]


state = { parent:parent,$ 
	;IDs of widgets that generate events or need to be updated. 
	roi_mode:roi_mode, roi_style:roi_style, $
	roi_define:roi_define, roi_semantics:roi_semantics,$
	show_roi:1, draft_mode:0, $
	
	dataset_list:dataset_list, $
	file_menu:file_menu, edit_button:edit_button, $
	derived_menu:derived_menu, $
	prev_frm:prev_frm, next_frm:next_frm, frm_num:frm_num, $
	animate:animate, mode_list:mode_list, $
	scaling_button:scaling_button, color_table_button:color_table_button, $
	msg_label:msg_label, pw_id:pw_id, $
	
	;Dataset structures
	selected_name:'', datasets: replicate( dataset, 20 ), $
	
	;ROI parameters
	roi_params: {xl:1., xh:0., yl:1., yh:0., zl:1., zh:0.}, $

	; FIT parameters
	fit: {name:'', number:0, x:ptr_new([0]), f_of_x:ptr_new([0])}, $
	
	;Other state information.
	surface_mode:0, x_axis_rotation:30, z_axis_rotation:30, $
	legend_style:0, color_bar:1, contour_kwds:'/FOLLOW', $
	share_binning_params:1, share_tv_params:1, $
	note:'', note_x:0.0, note_y:0.0, stale_titles:1, $
	
	time_of_last_user_event: 0L, background_processing_period: 20, $
	; 0 = nothing to do; 1 = light background load; 2 = heavy load
	background_processing_state: 0,  $
		
	stat_group_widget:0L, samples_widget:0L,$
	xcut_widget:0L, ycut_widget:0L, ps_config:ptr_new(/ALLOC), print_now:0B,$
	image_widget:0L, rgb_widget:0L, hsv_widget:0L, $
	msg_widget:0L, $
        indexed_add_lib:'' }

;; Look for a required shared library first in the UNIX path and then in the TARA library.
;; Administrators might put it n the UNIX path so mulitple platforms can share one TARA library.
;; We have to avoid searching the cwd since it might be a very large tree of user data.
if (!VERSION.MEMORY_BITS EQ 64) then lib_name = 'indexed_add_64.so' $
				else lib_name = 'indexed_add.so'

roots = break_path(getenv('PATH'))
ind = where((strmatch(roots,'.*') OR strmatch(roots,'')) EQ 0, count)
if (count GT 0) then lib_paths = file_search(roots[ind], lib_name, COUNT=count)
if (count GT 0) then begin
  state.indexed_add_lib = lib_paths[0]
endif else begin
  color_manager, UTILITY_DIR=utility_dir
  state.indexed_add_lib = utility_dir + lib_name
endelse


;; Allocate heap variables in dataset structures.
for ii = 0, n_elements(state.datasets)-1 do begin
  state.datasets[ii].index           = ii
  state.datasets[ii].x_data          = ptr_new([0])
  state.datasets[ii].y_data          = ptr_new([0])
  state.datasets[ii].z_data          = ptr_new([0])
  state.datasets[ii].x_filtered_data = ptr_new([0])
  state.datasets[ii].y_filtered_data = ptr_new([0])
  state.datasets[ii].z_filtered_data = ptr_new([0])
  state.datasets[ii].mask            = ptr_new(/ALLOC)
  state.datasets[ii].density.samples = ptr_new(/ALLOC)
  state.datasets[ii].stat_map.group_start= ptr_new(/ALLOC)
  state.datasets[ii].stat_map.group_stop = ptr_new(/ALLOC)
  state.datasets[ii].stat_map.sorted_data= ptr_new(/ALLOC)
  state.datasets[ii].stat_map.samples    = ptr_new(/ALLOC)
  state.datasets[ii].stat_map.kernel_descriptor_location = ptr_new(/ALLOC)
  state.datasets[ii].image           = ptr_new(/ALLOC)
  state.datasets[ii].bgrnd_index     = ptr_new(/ALLOC)
  state.datasets[ii].fgrnd_index     = ptr_new(/ALLOC)
endfor


;; Save state structure.
widget_control, top_base, SET_UVALUE=ptr_new(state, /NO_COPY)


;; If this is top-level widget, realize it and register.
if this_is_top_level then begin
  widget_control, parent, /REALIZE

  ; The fancy combination of JUST_REG and NO_BLOCK is necessary to get the
  ; widget to behave when called by either widget applications or normal
  ; programs (which block on tty reads).
  xmanager, 'dataset_3d', parent, GROUP_LEADER=group, $
	    JUST_REG=keyword_set(block), NO_BLOCK=(keyword_set(block) EQ 0), $
	    EVENT_HANDLER='PlotWindowTopbaseEventFn'
endif

return, top_base
END

;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO Dataset3dCleanup, top_base

widget_control, top_base, GET_UVALUE=st

helper_unit = (*st).datasets.stat_map.helper_unit
ind = where(helper_unit NE 0, count)
for ii = 0, count-1 do begin
  print, 'killing helper process'
  free_lun, helper_unit[ind[ii]]
endfor

kernel_descriptor_unit = (*st).datasets.stat_map.kernel_descriptor_unit
ind = where(kernel_descriptor_unit NE 0, count)
for ii = 0, count-1 do begin
  print, 'removing kernel descriptor file'
  free_lun, kernel_descriptor_unit[ind[ii]]
endfor


;; Free the heap vars allocated locally.
ptr_free, (*st).datasets.x_data, (*st).datasets.y_data, (*st).datasets.z_data,$
          (*st).datasets.x_filtered_data, (*st).datasets.y_filtered_data, $
          (*st).datasets.z_filtered_data, $
          (*st).datasets.mask, $
          (*st).datasets.density.samples, (*st).datasets.stat_map.samples, $
          (*st).datasets.stat_map.group_start, $
          (*st).datasets.stat_map.group_stop, $
          (*st).datasets.stat_map.sorted_data, $
          (*st).datasets.stat_map.kernel_descriptor_location, $
          (*st).datasets.image, $
          (*st).datasets.bgrnd_index, (*st).datasets.fgrnd_index, $
          (*st).fit.x , (*st).fit.f_of_x, (*st).ps_config
ptr_free, st
return
end

;==========================================================================
;;; Save a dataset structure back to the widget state structure.
;==========================================================================
PRO Save3dDataset, dataset, st
(*st).datasets[ dataset.index ] = dataset
return
end


;==========================================================================
;;; Routine to display a dataset image.  
;;; It is assumed that coordsystem_manager, /RESTORE has already been called.
;==========================================================================
PRO TvDataset3d, dataset, st, xaxis, yaxis, COLORBAR=colorbar, LOG=log, $
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
    dum  = where((lowlimit LE fgrnd_pixels) AND $
    		 (fgrnd_pixels LE highlimit), count )
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
  char_height = (convert_coord(0,!D.Y_CH_SIZE,/DEV,/TO_DATA) - $
  		 convert_coord(0,0,/DEV,/TO_DATA))[1]
  x_label_pos = [x_bar_pos,x_bar_pos]
  y_label_pos = [y_bar_pos-1.1*char_height,y_bar_pos+ysize+0.2*char_height]
  
  if keyword_set(erase) then begin
    mask = replicate(byte(black), !D.X_CH_SIZE * 10, !D.Y_CH_SIZE)
    tv, mask, x_label_pos[0], y_label_pos[0], /DATA
    tv, mask, x_label_pos[1], y_label_pos[1], /DATA
  endif
  
  tv, im, x_bar_pos, y_bar_pos, /DATA, XSIZE=xsize, YSIZE=ysize

  labels = strtrim(string([tvlow,tvhigh], F='(G10.3)'), 2)
  xyouts, x_label_pos, y_label_pos, labels
  if keyword_set(log) then $
    xyouts, x_bar_pos+xsize+char_height, y_bar_pos+ysize/2.0, $
            'log scaling', ALIGN=0.5, ORIENT=90
endif
return
end

;==========================================================================
;;; Routine to filter a dataset or compute ROI statistics.
;;; The structure 'dataset' is modified!
;==========================================================================
PRO ApplyDataset3dRoi, dataset, st

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
  z_roi_data = dataset.z_data
    
  mask = replicate( 1B, n_elements(*dataset.x_data) )
  
endif else begin

  ;; If we get here, then there is a ROI defined and we need to apply it.
  ;; Remember that the result  could be the empty set, represented by
  ;; x_roi_data & y_roi_data pointing to undefined heap vars.
  roi    = (*st).roi_params
  x_data = dataset.x_data
  y_data = dataset.y_data
  z_data = dataset.z_data
  
  ;; Initialize the ROI if it has never been assigned by user or if it is
  ;; null.
  if (roi.xl GT roi.xh) then begin
    roi.xh = max(*x_data, MIN=x_min)
    roi.xl = x_min
  endif

  if (roi.yl GT roi.yh) then begin
    roi.yh = max(*y_data, MIN=y_min)
    roi.yl = y_min
  endif
  
  if (roi.zl GT roi.zh) then begin
    roi.zh = max(*z_data, MIN=z_min)
    roi.zl = z_min
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
  
   0: $ ; cube
    begin
    mask = (roi.xl LE *x_data AND roi.xh GE *x_data AND $
  	    roi.yl LE *y_data AND roi.yh GE *y_data AND $
  	    roi.zl LE *z_data AND roi.zh GE *z_data)
    end
  
   3: $ ; T-range
    begin
    mask = (roi.zl LE *z_data AND roi.zh GE *z_data)
    end
  endcase
  
  (*st).roi_params = roi

  ;; Invert the mask if exclude mode is chosen.
  if (1 EQ widget_info( (*st).roi_semantics, /DROPLIST_SELECT )) then begin
    mask = (mask EQ 0)
  endif
  
  ;; Compute the datapoints that are in the ROI,
  index = where( mask, count )

  if (count EQ 0) then begin
    x_roi_data = ptr_new(/ALLOC)
    y_roi_data = ptr_new(/ALLOC)
    z_roi_data = ptr_new(/ALLOC)
  endif else begin
    x_roi_data = ptr_new( (*x_data)[index], /NO_COPY)
    y_roi_data = ptr_new( (*y_data)[index], /NO_COPY)
    z_roi_data = ptr_new( (*z_data)[index], /NO_COPY) 
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
    xcen = total(*x_roi_data, /DOUBLE)/num_points
    ycen = total(*y_roi_data, /DOUBLE)/num_points
    tcen = total(*z_roi_data, /DOUBLE)/num_points

    f='(I0," data points; centroid= [",G11.5,", ",G11.5,", ",G11.5," ]")'
    dataset.stats = string(num_points,xcen,ycen,tcen, FORMAT=f)
    
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

  if (dataset.z_filtered_data NE dataset.z_data) then $
	ptr_free, dataset.z_filtered_data

  ;; Is filtering selected?
  if (widget_info( (*st).roi_mode, /DROPLIST_SELECT ) NE 2) then begin
    dataset.x_filtered_data  = dataset.x_data
    dataset.y_filtered_data  = dataset.y_data
    dataset.z_filtered_data  = dataset.z_data
    *dataset.mask = 0
    dum = temporary( *dataset.mask )
  endif else begin
    dataset.x_filtered_data = x_roi_data
    dataset.y_filtered_data = y_roi_data
    dataset.z_filtered_data = z_roi_data
    
    *dataset.mask = temporary( mask )
  endelse

  ;; Find the max and min of the filtered dataset.
  dataset.num_data_points = n_elements( *dataset.x_filtered_data )

  if (dataset.num_data_points GT 0) then begin
    dataset.x_max           = max(*dataset.x_filtered_data, MIN=x_min)
    dataset.x_min           = x_min
    dataset.y_max           = max(*dataset.y_filtered_data, MIN=y_min)
    dataset.y_min           = y_min
    dataset.z_max           = max(*dataset.z_filtered_data, MIN=z_min)
    dataset.z_min           = z_min
  endif  
  
  
  ;; Since we've changed the filtered dataset, mark all derived data structures
  ;; as stale.
  dataset.density.stale   = 1
  dataset.stat_map.stale  = 1
  
endif ; (dataset.filter_stale EQ 1)

*(*st).fit.x = [0]
return
end


;==========================================================================
;;; Routine to define a binning region and bin up trivariate data.
;;; (x0,y0,z0) are the coordinates of the center of the first pixel.
;==========================================================================
PRO Bin3dDataset, dataset, MARGIN=margin, $
		  x0, y0, z0, num_col, num_row, num_frm, col, row, frm

if (n_elements(margin) NE 3) then margin = [0,0,0]

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

if (bin.desired_delta_z EQ 0) then begin
      bin.delta_z = float(limit_precision((dataset.z_max - dataset.z_min)/10., 2))
      if (size(*dataset.z_data, /TYPE) LE 3) then begin
        bin.delta_z = round(bin.delta_z)
        bin.zbin_location = 0.5
      endif
      if (bin.delta_z LE 0) then bin.delta_z = 1.0
endif else bin.delta_z = bin.desired_delta_z


if ((bin.desired_delta_x EQ 0) AND (bin.desired_delta_y EQ 0)) then begin  
      ; If we're computing both bins sizes, make them the same.   
      xy_bin = bin.delta_x > bin.delta_y
      bin.delta_x = xy_bin
      bin.delta_y = xy_bin
endif


;; If necessary, increase the sample intervals (delta_x & delta_y) requested 
;; until a reasonably sized array results.
maxdim = 1200
repeat begin
  xdim = (dataset.x_max - dataset.x_min) / bin.delta_x
  ydim = (dataset.y_max - dataset.y_min) / bin.delta_y
  zdim = (dataset.z_max - dataset.z_min) / bin.delta_z

  done = 1
  if (xdim GT maxdim) then begin
    bin.delta_x = 2 * bin.delta_x
    done      = 0
  endif
  
  if (ydim GT maxdim) then begin
    bin.delta_y = 2 * bin.delta_y
    done      = 0
  endif
  
  if (zdim*xdim*ydim GT 1E7) then begin
    bin.delta_z = 2 * bin.delta_z
    done      = 0
  endif
endrep until (done)

dataset.binning = bin


;; Calculate the signed distance from the smallest datapoint to the bin corner
;; the user specified.
datamin_to_edge_distance_x = dataset.x_min - bin.xbin_location
datamin_to_edge_distance_y = dataset.y_min - bin.ybin_location
datamin_to_edge_distance_z = dataset.z_min - bin.zbin_location

;; Calculate the location of the bin corner which is <= the smallest 
;; datapoint.
x_min = floor( datamin_to_edge_distance_x / bin.delta_x ) * bin.delta_x $
        + bin.xbin_location
y_min = floor( datamin_to_edge_distance_y / bin.delta_y ) * bin.delta_y $
        + bin.ybin_location
z_min = floor( datamin_to_edge_distance_z / bin.delta_z ) * bin.delta_z $
        + bin.zbin_location

;; Add the desired margin to the left & bottom edges of the binning region.	
x_min = x_min - bin.delta_x * margin[0]
y_min = y_min - bin.delta_y * margin[1]
z_min = z_min - bin.delta_z * margin[2]
  	     
;; Next, discretize the data vectors.
col = floor( (*dataset.x_filtered_data - x_min) / bin.delta_x ) 
row = floor( (*dataset.y_filtered_data - y_min) / bin.delta_y ) 
frm = floor( (*dataset.z_filtered_data - z_min) / bin.delta_z ) 

num_col = max(col)+1  &  num_row = max(row)+1  &  num_frm = max(frm)+1
 
;; Add the desired margin to the right & top edges of the binning region.	
num_col = num_col + margin[0]
num_row = num_row + margin[1]
num_frm = num_frm + margin[2]

;; Compute (x0,y0,z0) the coordinates of the center of the first pixel.
x0 = x_min + bin.delta_x/2.0
y0 = y_min + bin.delta_y/2.0
z0 = z_min + bin.delta_z/2.0

;; Make sure there are at least two bins in each direction.
num_col = num_col > 2
num_row = num_row > 2
num_frm = num_frm > 2

return
end


;==========================================================================
;;; Routine to compute statistics on T-values , sampled on a uniform XY grid. 
;;; The structure 'dataset' is modified!
;;; The phase of the sample grid is controlled by the parameters 
;;; (*st).binning.xbin_location & ybin_location
;;;
;;; The structure tags x0 & y0 refer to the data coordinates of the CENTER 
;;; of the lower-left bin.
;==========================================================================
PRO ComputeDataset3dMap, dataset, st

s = dataset.stat_map

;; First, see if the density has already been calculated.
;; For the stat map we adopt this convention for the stale flag:
;;  0: stat map is fully computed
;;  1: stat map is stale, i.e. we need to start helper process now
;;  3: helper process is running, stat map is partially computed.
if (s.stale EQ 0) OR (s.stale EQ 3) then return

;; We do NOT clear stale flag here since the stat map will be computed
;; asynchronously via a helper process.  We'll clear it later when we've
;; found that the stat map is complete.  For now we set it to 3.
s.stale = 3

;; Now, make sure there is some data left after filtering.
if (dataset.num_data_points EQ 0) then begin
  *s.samples = lonarr(4,4)
  s.xdim = 4  &  s.ydim = 4
  s.subtitle = 'NULL DATASET!'
  s.stale = 0
  dataset.stat_map = s
  (*st).stale_titles = 1
  return
endif


;; Kill any existing helper process, and remove any existing helper output file.
if (s.helper_unit NE 0) then begin
  print, 'killing helper process'
  free_lun, s.helper_unit
  s.helper_unit = 0
endif


;; Compute a 3-D histogram.
Bin2dDataset, dataset, $
	      x0, y0, num_col, num_row, col, row

bin    = dataset.binning
s.x0   = x0
s.y0   = y0
s.xdim = num_col
s.ydim = num_row
s.bandwidth_x = bin.delta_x 
s.bandwidth_y = bin.delta_y 

;msg = string(bin.delta_x, bin.delta_y, $
;  	f='("Sorting data with bins ",G10.4," X ",G10.4)' )
;widget_control, (*st).msg_label, SET_VALUE=msg


;; Compute a 2-D histogram with reverse indexes.
index_1d  = row * long(num_col) + col
nbins     = num_col*num_row
counts_per_group = histogram( index_1d, MIN=0L, MAX=nbins-1, $
			      REVERSE_INDICES=rindex )

counts_per_group = reform(counts_per_group, num_col, num_row, /OVERWRITE)
if (n_elements(counts_per_group) NE nbins) then message, 'nbins not correct'


;; Expand the first segment of REVERSE_INDICES into a 2-D map of group start
;; indexes and group stop indexes that can be easily subscripted to extract
;; 2-D kernel regions, e.g. group_start[a:b,c:d].
;; Re-order the data vector so that data belonging to each bin are grouped
;; together, and those groups appear in the order of the bins.
;; Sorting the data up front will avoid applying REVERSE_INDEXES to the data
;; vector multiple times later.
*s.group_start =reform(rindex[0:nbins-1] - (nbins + 1),     num_col,num_row)
*s.group_stop  =reform(rindex[1:nbins]   - (nbins + 1) - 1, num_col,num_row)
sorting_indexes= rindex[nbins+1:*]
*s.sorted_data = (*dataset.z_filtered_data)[sorting_indexes]


widget_control, (*st).msg_label, SET_VALUE='Spawning helper process ...'

; Generate a scratch filename in /tmp (for speed).
input_filename = filepath(string(long(systime(1)),F='(I0,".ds_3d")'), /TMP)

;; Write the data structures the helper process will need.
name                    = dataset.name
min_significance        = s.min_significance 
require_compatible_flux = s.require_compatible_flux

save, FILENAME=input_filename, $
      name, min_significance, $
      require_compatible_flux, counts_per_group 


;; Close & remove any previous kernel descriptor file that might still be open.
if (s.kernel_descriptor_unit NE 0) then begin
  free_lun, s.kernel_descriptor_unit
  s.kernel_descriptor_unit = 0
endif


;; Spawn the helper process, send it the name of its input file,
;; and read back the name of the kernel descriptor file.
;; We copy !PATH to the environment in case the user is changing path after
;; IDL starts, e.g. via a startup program.
setenv, 'IDL_PATH='+!path
spawn, getenv("IDL_DIR") + '/bin/idl', UNIT=u, /NOSHELL
s.helper_unit = u
printf, u, 'dataset_3d_helper'
printf, u, input_filename

fn = ''
readf, u, fn
s.kernel_descriptor_filename = fn

;; Block until the kernel descriptor file can be opened.
for ii=0,9 do begin
  openr, unit, s.kernel_descriptor_filename, /DELETE, /GET_LUN, ERROR=Error
  if (Error EQ 0) then break
  wait, 1
  print, 'trying to open ' + s.kernel_descriptor_filename
endfor

;; If we've successfully opened the file then enable the process of 
;; reading from it to build the stat map.
;; To force the first background processing session to start immediately
;; we set background_processing_state=2 ("bkg process is behind") and
;; set time_of_last_user_event=0 ("user is idle").
if (Error EQ 0) then begin
  s.kernel_descriptor_unit = unit
  (*st).background_processing_state = 2
  (*st).time_of_last_user_event     = 0
endif else begin
  msg = 'Could not open file ' + s.kernel_descriptor_filename
  dummy=dialog_message(msg, /ERROR, DIALOG_PARENT=(*st).pw_id)
endelse

;; Create a null stat map to display immediately.
*s.samples                    = replicate(!VALUES.F_NAN,num_col,num_row)
s.samples_completed           = 0
*s.kernel_descriptor_location = replicate(-1L,num_col,num_row)

case s.stat_code of
 0: subtitle = 'MEAN'
 1: subtitle = 'MEANCLIP'
 2: subtitle = 'MEDIAN'
 3: subtitle = '"Background" via MMM'
 4: subtitle = 'TOTAL'
 5: subtitle = 'MAX'
 6: subtitle = 'MIN'
 7: subtitle = 'VARIANCE'
 8: subtitle = 'STDDEV'
 9: subtitle = 'Burst Length'
10: subtitle = 'Density'
endcase

plot_window, (*st).pw_id, ZAXIS=zaxis
s.subtitle = subtitle + ' of ' + zaxis.title


dataset.tv_stale = 1

dataset.stat_map = s

(*st).stale_titles = 1
return
end


;==========================================================================
;;; Routine to read the kernel descriptors written by the helper process,
;;; and compute the desired statistic.
;;; The structure 'dataset' is modified!
;==========================================================================
PRO ProcessKernelDescriptors, stat_map, st
   group_start = stat_map.group_start
   group_stop  = stat_map.group_stop
   samples     = stat_map.samples
   sorted_data = stat_map.sorted_data
   kernel_descriptor_location = stat_map.kernel_descriptor_location

   ;; Spend up to background_processing_period seconds processing any 
   ;; available kernel descriptors.
   (*st).background_processing_state = 1
   t0 = systime(1)
   num_pixels = 0L
   num_groups = 0L
   
   while (1) do begin
     ; Remain in loop only if at least 2 words (num_pixels & num_groups) can be read from file.
     file_info       = fstat( stat_map.kernel_descriptor_unit )
     num_words_avail = (file_info.size - file_info.cur_ptr) / 4
     if (num_words_avail LT 2) then break
     
     ; Read # of members of kernel (>=1).
     readu, stat_map.kernel_descriptor_unit, num_pixels, num_groups
     
     ; If the rest of the kernel descriptor is not yet available, give up.
     if ((num_words_avail-2) LT num_groups) then begin
;       print, 'partial kernel descriptor found; will try again'
       point_lun, stat_map.kernel_descriptor_unit, file_info.cur_ptr
       break
     endif
     
     ; Read central pizel, plus other non-zero members of kernel.
     groups_in_kernel = lonarr(num_groups)
     readu, stat_map.kernel_descriptor_unit, groups_in_kernel
     
     ; The first member of the kernel is the central group/pixel.
     root_index = groups_in_kernel[0]
     
     ; Save the offset in the file where this kernel descriptor can be
     ; found later.
     (*kernel_descriptor_location)[root_index] = file_info.cur_ptr
       
     ;; Locate the data associated with each group.
     gstart = (*group_start)[groups_in_kernel]
     gstop  = (*group_stop) [groups_in_kernel]
         
     num_data   = total( (gstop - gstart + 1) > 0 )
   
     
     ;; Compute the statistic.
     if (num_data GT 0) then begin
      if (num_groups EQ 1) then begin
        data = (*sorted_data)[gstart[0]:gstop[0]]
      endif else begin
        concat_array_segments, *sorted_data, gstart, gstop, data
      endelse
   
      case stat_map.stat_code of
       ; MEAN
       0: begin
          if (num_data EQ 1) then (*samples)[root_index] = data[0] $
          			  else (*samples)[root_index] = mean(data, /DOUBLE)
          end
       
       ; MEANCLIP
       1: begin
          if (num_data EQ 1) then mean = data[0] $
          			  else meanclip, double(data), mean
          (*samples)[root_index] = mean
          end
       
       ; MEDIAN
       2: (*samples)[root_index] = median(data, /EVEN)
       
       ; "Background" via MMM
       3: begin
          ; The MMM routine can fail in several ways, so we must catch those
          ; errors.
          catch, error_code
          if (error_code NE 0) then begin
            printf, -2, !ERR_STRING
            mode = median(data, /EVEN)
          endif else begin
            mmm, double(data), mode
          endelse
          catch, /CANCEL
          (*samples)[root_index] = mode
          end
       
       ; TOTAL
       4: (*samples)[root_index] = total(data, /DOUBLE)
       
       ; MAX
       5: (*samples)[root_index] = max(data)
       
       ; MIN
       6: (*samples)[root_index] = min(data)
       
       ; VARIANCE
       7: begin
          if (num_data EQ 1) then (*samples)[root_index] = 0 $
          			  else (*samples)[root_index] = variance(data, /DOUBLE)
          end
          
       ; STDDEV
       8: begin
          if (num_data EQ 1) then (*samples)[root_index] = 0 $
          			  else (*samples)[root_index] = stddev(data, /DOUBLE)
          end
       
       ; Burst Metric
       9: begin
          data = data[sort(data)]
         
          ;; This loop finds the longest burst of data with at most one missing 
          ;; datum.  The data are assumed to be integers.  
          ;; This statistic doesn't make much sense if there are repeated values,
          ;; and we do NOT check for repeated values.
          burst_length = replicate(1L,num_data)
          continue = 1
          ii   = 1L
          
          while (continue AND (ii LT num_data)) do begin
           entend_burst = ((data[ii:*] - data) LE (ii+1))
           if (total(entend_burst) GT 0) then begin
             burst_length = burst_length + entend_burst
             ii = ii+1
           endif else continue = 0
          endwhile
         
          (*samples)[root_index] = max(burst_length)
          end
          
       ; DENSITY
       10: (*samples)[root_index] = float(num_data)/num_pixels
           
       else:
      endcase
     endif ;(num_data GT 0)
     
     ;; Once in a while, check if background_processing_period seconds have
     ;; elapsed and break.
     if ((root_index mod 100) EQ 0) then begin
       if ((systime(1)-t0) GT (*st).background_processing_period) then begin
         (*st).background_processing_state = 2
         break
       endif
     endif
   endwhile ; kernel descriptor available
   
   if keyword_set(root_index) then stat_map.samples_completed = root_index + 1
;help,root_index
return
end


;==========================================================================
;;; Routine to estimate a density function, sampled on a uniform 3-D grid. 
;;; The structure 'dataset' is modified!
;;; The phase of the sample grid is controlled by the parameters 
;;; (*st).binning.xbin_location & ybin_location
;;;
;;; The structure tags x0 & y0 refer to the data coordinates of the CENTER 
;;; of the lower-left bin.
;==========================================================================
PRO ComputeDataset3dDensity, dataset, st

d = dataset.density

;; First, see if the density has already been calculated.
if (d.stale EQ 0) then return

d.stale = 0

;; Now, make sure there is some data left after filtering.
if (dataset.num_data_points EQ 0) then begin
  *d.samples = lonarr(4,4,2)
  d.xdim = 4  &  d.ydim = 4  &  d.tim = 2
  d.subtitle = 'NULL DATASET!'
  dataset.density = d
  dataset.frame_index = 0 > dataset.frame_index < (d.zdim-1)
  (*st).stale_titles = 1
  return
endif

;; Compute a 3-D histogram.
;; If we're going to be smoothing with a kernel, then we need to make sure
;; there are margins of zeros at the left & bottom edges of the histogram.  
case (d.kernel) of
   0: kernel_dim = 1
   
   ; Epanechnikov kernel
   ; The kernel always has ODD dimensions equal to d.Epanechnikov_width-1.
   1: kernel_dim = d.Epanechnikov_width-1
  
   ;Gaussian kernel
   ; The kernel always has ODD dimensions sufficient to cover 4-sigma.
   2: kernel_dim = 1 + (2 * ceil(3 * d.Gaussian_sigma ))
endcase
  
case (d.kernel_z) of
   0: kernel_dim_z = 1
   
   ; Epanechnikov kernel
   ; The kernel always has ODD dimensions equal to d.Epanechnikov_width_z-1.
   1: kernel_dim_z = d.Epanechnikov_width_z-1
  
   ;Gaussian kernel
   ; The kernel always has ODD dimensions sufficient to cover 4-sigma.
   2: kernel_dim_z = 1 + (2 * ceil(3 * d.Gaussian_sigma_z ))
endcase

convolution_margin   = (kernel_dim  -1)/2
convolution_margin_z = (kernel_dim_z-1)/2
  
Bin3dDataset, dataset,  $
  	   MARGIN=[convolution_margin,convolution_margin,convolution_margin_z],$
	      x0, y0, z0, num_col, num_row, num_frm, col, row, frm

bin    = dataset.binning
d.x0   = x0
d.y0   = y0
d.z0   = z0
d.xdim = num_col
d.ydim = num_row
d.zdim = num_frm
dataset.frame_index = num_frm/2

msg = string(bin.delta_x, bin.delta_y, bin.delta_z, $
  	f='("Computing histogram with bins ",G10.4," X ",G10.4," X ",G10.4)' )
widget_control, (*st).msg_label, SET_VALUE=msg
  

;; Compute the 3-D histogram.
index_1d   = (frm * long(num_row*num_col)) + (row * long(num_col)) + col
*d.samples = lonarr( num_col, num_row, num_frm )

; Don't forget: 1st and 3rd arg must be LONG, 2nd must be LONG.
num_data_points = n_elements( index_1d )
dum = CALL_EXTERNAL( (*st).indexed_add_lib, 'indexed_add_long', $
                     *d.samples, index_1d, $
                     replicate(1L, num_data_points), num_data_points )
   
    
;; Now, apply X/Y kernel smoothing if requested.
case (d.kernel) of
 0: $
  begin
  ;; HISTOGRAM
  f='("Binsize (xy): ",G10.3," X ",G10.3)'
  xy_subtitle = strcompress( string(bin.delta_x, bin.delta_y, F=f))
  end


 1: $
  begin
  ;; EPANECHNIKOV SMOOTHING
  d.bandwidth_x = bin.delta_x * d.Epanechnikov_width/2
  d.bandwidth_y = bin.delta_y * d.Epanechnikov_width/2

  xy_subtitle = strcompress( string( d.bandwidth_x, d.bandwidth_y, $
  		f='("Epanechnikov bandwidth (xy): ",G10.3," X ",G10.3)' ) )

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

  ; Force this XY kernel to be 3-D and convolve with 3-D histogram.
  ; With the histogram padded by convolution_margin, the /EDGE_WRAP below should produce the same result as /EDGE_ZERO.
  ; I don't recall why I didn't use /EDGE_ZERO for extra protection from wrapping, but I hesitate to change the code now (2012).
  kernel = reform( kernel, kernel_dim, kernel_dim, 1 )
  *d.samples= convol( float(*d.samples), kernel, /EDGE_WRAP )
  end


 2: $
  begin
  ;; GAUSSIAN SMOOTHING
  d.bandwidth_x = bin.delta_x * d.Gaussian_sigma 
  d.bandwidth_y = bin.delta_y * d.Gaussian_sigma

  xy_subtitle = strcompress( string( d.bandwidth_x, d.bandwidth_y, $
  	f='("Gaussian bandwidth (xy,1-sigma): ",G10.3," X ",G10.3)' ) )

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

  ; Force this XY kernel to be 3-D and convolve with 3-D histogram.
  ; With the histogram padded by convolution_margin, the /EDGE_WRAP below should produce the same result as /EDGE_ZERO.
  ; I don't recall why I didn't use /EDGE_ZERO for extra protection from wrapping, but I hesitate to change the code now (2012).
  kernel = reform( kernel, kernel_dim, kernel_dim, 1 )
  *d.samples= convol( float(*d.samples), kernel, /EDGE_WRAP )
  end
endcase


;; And apply kernel smoothing along the "time" axis if requested.
case (d.kernel_z) of
 0: $
  begin
  ;; HISTOGRAM
  f='("Binsize (t): ",G10.3)'
  t_subtitle = strcompress( string(bin.delta_z, F=f))
  end


 1: $
  begin
  ;; EPANECHNIKOV SMOOTHING
  d.bandwidth_z = bin.delta_z * d.Epanechnikov_width_z/2

  t_subtitle = strcompress( string( d.bandwidth_z, $
  		f='("Epanechnikov bandwidth (t): ",G10.3)' ) )

  widget_control, (*st).msg_label, SET_VALUE='smoothing histogram'
  
  ;; The data themselves are now discretized using the sampling interval
  ;; delta_z.  We want to discretize the kernel with the same
  ;; sampling interval, then convolve the two to smooth.
    
  ; First, make a kernel vector where each element is the distance from
  ; the center in array index units.
  kernel = findgen(kernel_dim_z) - kernel_dim_z/2
  
  ; Now, scale these distances to the kernel coordinate system in which the
  ; kernel support is [-1,1].
  kernel = kernel * 2.0 / d.Epanechnikov_width_z
  
  ; Make a mask that is zero for any samples at a distance greater than 1.0
  ; so that we can zero those sample later.
  mask = (kernel LT 1.0)
  
  ; Now, compute the kernel values, normalizing so that the sum is 1.0
  ; Kernel is zero for distances greater than 1.0
  kernel = 0.75 * (1 - kernel^2)
  kernel = kernel * mask 
  kernel = kernel / total(kernel)

  ; Force this T kernel to be 3-D and convolve with 3-D histogram.
  ; With the histogram padded by convolution_margin, the /EDGE_WRAP below should produce the same result as /EDGE_ZERO.
  ; I don't recall why I didn't use /EDGE_ZERO for extra protection from wrapping, but I hesitate to change the code now (2012).
  kernel = reform( kernel, 1, 1, kernel_dim_z )
  *d.samples= convol( float(*d.samples), kernel, /EDGE_WRAP )
  end


 2: $
  begin
  ;; GAUSSIAN SMOOTHING
  d.bandwidth_z = bin.delta_z * d.Gaussian_sigma_z

  t_subtitle = strcompress( string( d.bandwidth_z, $
  		f='("Gaussian bandwidth (t,1-sigma): ",G10.3)' ) )

  widget_control, (*st).msg_label, SET_VALUE='smoothing histogram'
  
  ;; The data themselves are now discretized using the sampling intervals
  ;; delta_x & delta_y.  We want to discretize the kernel with the same
  ;; sampling intervals, then convolve the two to smooth.
    
  ; First, make a kernel vector where each element is the distance from
  ; the center in array index units.
  kernel = findgen(kernel_dim_z) - kernel_dim_z/2

  ; Normalize those distances by the bandwidth parameter..
  kernel = kernel / d.Gaussian_sigma_z
      
  ; Now, compute the kernel values, normalizing so that the sum is 1.0
  kernel = exp( (kernel^2) / (-2.0) )
  kernel = kernel / total(kernel)

  ; Force this T kernel to be 3-D and convolve with 3-D histogram.
  ; With the histogram padded by convolution_margin, the /EDGE_WRAP below should produce the same result as /EDGE_ZERO.
  ; I don't recall why I didn't use /EDGE_ZERO for extra protection from wrapping, but I hesitate to change the code now (2012).
  kernel = reform( kernel, 1, 1, kernel_dim_z )
  *d.samples= convol( float(*d.samples), kernel, /EDGE_WRAP )
  end

endcase

d.subtitle = xy_subtitle +'; ' + t_subtitle



;; If desired, normalize the animation frames.
if (dataset.tvnorm_file NE '') then begin
  msg = string(dataset.tvnorm_file, $
		f='("Normalizing animation frames using ",A0)' )
  widget_control, (*st).msg_label, SET_VALUE=msg

  table     = (read_ascii( dataset.tvnorm_file , COMMENT=';' )).(0)
  tvals     = table[0,*]
  normvals  = table[1,*]
  frame_tvals = d.z0 + findgen(d.zdim)*bin.delta_z
  tvnorm = interpol( normvals, tvals, frame_tvals )
  
  *d.samples = float(*d.samples)
  for ii=0, d.zdim-1 do begin
    print, frame_tvals[ii], tvnorm[ii]
    (*d.samples)[0,0,ii] = (*d.samples)[*,*,ii] * tvnorm[ii]
  endfor
endif

dataset.tv_stale = 1

dataset.density = d

(*st).stale_titles = 1
return
end



;==========================================================================
;;; Routine to display an image
;==========================================================================
PRO DrawDataset3dImage, dataset, st, subtitle

  ; Draw the plot axes.
  if ((*st).stale_titles) then begin
    plot_window, (*st).pw_id, SUBTITLE=subtitle, $
				 TITLE=dataset.description 
  endif

  plot_window, (*st).pw_id, /SHOW_AXES, /FORCE_LINEAR, $
  	       XAXIS=xaxis, YAXIS=yaxis
  
;  msg = 'Sampling density to form image ...'
;  widget_control, (*st).msg_label, SET_VALUE=msg

  bin = dataset.binning

  ; Specify the position in the X/Y coordinate system, pixel size, and 
  ; dimensions of the image we need to "tv" into the axes that have been
  ; already drawn.
  x0_p      = xaxis.image_position 
  y0_p      = yaxis.image_position 
  delta_x_p = (xaxis.image_size/xaxis.num_pixels) 
  delta_y_p = (yaxis.image_size/yaxis.num_pixels)
  xdim_p    = xaxis.num_pixels
  ydim_p    = yaxis.num_pixels
  
  ; Resample the density array to create the image required for TV.
  mode = widget_info( (*st).mode_list, /DROPLIST_SELECT )
  if (mode EQ 4) then begin
    sm = dataset.stat_map
    resample_image, sm.x0, sm.y0, bin.delta_x, bin.delta_y,   $
  		    *sm.samples, $
		    x0_p,  y0_p,  delta_x_p,   delta_y_p, $
		    xdim_p, ydim_p, *dataset.image,$
		    *dataset.bgrnd_index, *dataset.fgrnd_index, $
		    BACKGROUND_VALUE=0, /SCREEN_NAN
  
    feature_size_x = sm.bandwidth_x
    feature_size_y = sm.bandwidth_y
  endif else begin
    d = dataset.density
    resample_image, d.x0, d.y0, bin.delta_x, bin.delta_y,   $
  		    (*d.samples)[*,*,dataset.frame_index], $
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
  endelse

  ; If our new image was all background pixels then we fix up fgrnd_index so
  ; there won't be problems later when we need to compute stats on foreground
  ; pixels.
  if (size(*dataset.fgrnd_index, /N_DIM) EQ 0) then begin
    *dataset.fgrnd_index = lindgen(n_elements(*dataset.image))
    *dataset.image       = lonarr(xdim_p, ydim_p)
  endif

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
  TvDataset3d, dataset, st, xaxis, yaxis, $
  		COLORBAR=(*st).color_bar, LOG=dataset.log_flag

  
  ;; Save the dataset structure, which may have been modified.
  Save3dDataset, dataset, st
return
end


;==========================================================================
;;; Routine to update the widget's appearance
;==========================================================================
PRO RedrawDataset3d, st

widget_control, /HOURGLASS
show_stats = 1

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
ApplyDataset3dRoi, selected_dataset, st
Save3dDataset, selected_dataset, st


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
    ApplyDataset3dRoi, dataset, st
    Save3dDataset, dataset, st
  endfor
    
  plotsets = ((*st).datasets)[ shown ]
  
  set_xrange = [min( plotsets.x_min ), max( plotsets.x_max )]
  set_yrange = [min( plotsets.y_min ), max( plotsets.y_max )]
  
endif else if (mode EQ 4) then begin
  ;; STATISTIC MAP
  ;; Compute the map, and save the modified dataset.

  ComputeDataset3dMap, selected_dataset, st
  Save3dDataset, selected_dataset, st
  
  set_xrange = [selected_dataset.x_min, selected_dataset.x_max]
  set_yrange = [selected_dataset.y_min, selected_dataset.y_max]
endif else begin
  ;; SOME KIND OF DENSITY DISPLAY
  ;; Compute the density, and save the modified dataset.

  ComputeDataset3dDensity, selected_dataset, st
  Save3dDataset, selected_dataset, st
  
  t_frame = selected_dataset.density.z0 + $
	    selected_dataset.frame_index * selected_dataset.binning.delta_z
  frame_name = "; z="+strcompress(string( t_frame, FORMAT='(G10.3)' ), /REMOVE)

  set_xrange = [selected_dataset.x_min, selected_dataset.x_max]
  set_yrange = [selected_dataset.y_min, selected_dataset.y_max]
endelse

widget_control, (*st).frm_num, SET_VALUE=selected_dataset.frame_index

;--------------------------------------------------------------------------
;; Assign default axis ranges if necessary.
;--------------------------------------------------------------------------
plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis
			  
if (xaxis.default_flag EQ 0) then set_xrange=0 
if (yaxis.default_flag EQ 0) then set_yrange=0

plot_window, (*st).pw_id, SET_XRANGE=set_xrange, SET_YRANGE=set_yrange
color_manager, BLUE=blue, GREEN=green


;; Display the datasets.
case (mode) of
 ;-------------------------------------------------------------------------
 0: $ SCATTER PLOT
 ;-------------------------------------------------------------------------
  begin
  ; Draw the plot axes.
  if ((*st).stale_titles) then begin
    if (n_elements(plotsets) EQ 1) then title=plotsets[0].description $
				   else title=''
    plot_window, (*st).pw_id, TITLE=title, SUBTITLE=''
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
  DrawDataset3dImage, selected_dataset, st, $
  		      selected_dataset.density.subtitle+frame_name
  end
  
 ;-------------------------------------------------------------------------
 2: $ SURFACE PLOT
 ;-------------------------------------------------------------------------
  begin
  if ((*st).stale_titles) then begin
    plot_window, (*st).pw_id, TITLE=selected_dataset.description, $
    		 SUBTITLE=selected_dataset.density.subtitle+frame_name
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
    shade_surf, $
        (*selected_dataset.density.samples)[*,*,selected_dataset.frame_index],$
	   x, y, /SAVE, $
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
    surface, $
        (*selected_dataset.density.samples)[*,*,selected_dataset.frame_index],$
           x, y, /SAVE, $
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
    plot_window, (*st).pw_id, TITLE=selected_dataset.description, $
    		 SUBTITLE=selected_dataset.density.subtitle+frame_name
  endif

  plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis, ZAXIS=zaxis, $
		GET_TITLE=title, GET_SUBTITLE=subtitle, DRAW_WIDGET=draw_widget
  
  x = selected_dataset.density.x0 + $
      findgen(selected_dataset.density.xdim) * selected_dataset.binning.delta_x
  y = selected_dataset.density.y0 + $
      findgen(selected_dataset.density.ydim) * selected_dataset.binning.delta_y

  ;; For the 'X' device, we must select the correct window to draw in.
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
  img = (*selected_dataset.density.samples)[*,*,selected_dataset.frame_index]
  contour, img, x, y, /NODATA, _EXTRA=extra

  if (!D.NAME EQ 'X')  then coordsystem_manager, draw_widget, /SAVE
  if (!D.NAME EQ 'PS') then device,/CLOSE

  ;; Now draw the axes & titles.
  plot_window, (*st).pw_id, /SHOW_AXES, /FORCE_LINEAR
    
  ;; And finally overplot the contours.
  color_manager, selected_dataset.plot.color, plot_color
  contour, img, x, y, /OVERPLOT, COLOR=plot_color, _EXTRA=extra

  ;; You would think that we could omit the first call to contour and 
  ;; simply call plot_window then contour, /OVERPLOT, but that doens't work.
  ;; The contour, /OVERPLOT call doesn't seem to calculate !Z -- RSI
  ;; probably assumes you want to overplot on top of another contour.
  ;; So, the purpose of the first call to contour is simply to calculate !Z.
  end
  
 ;-------------------------------------------------------------------------
 4: $ STATISTIC MAP
 ;-------------------------------------------------------------------------
  begin
  s = selected_dataset.stat_map
  
  if (s.samples_completed EQ 0) then begin
    ; When no data available, this is quicker than DrawDataset3dImage.
    plot_window, (*st).pw_id, /SHOW_AXES, /FORCE_LINEAR
  endif else begin
    DrawDataset3dImage, selected_dataset, st, s.subtitle
  endelse

  if (s.samples_completed LT (s.xdim*s.ydim)) then begin
    show_stats = 0
    msg=string((100*s.samples_completed/(s.xdim*s.ydim)), $
		f='("Map is ",I0,"% completed")')
    widget_control, (*st).msg_label, SET_VALUE=msg
  endif
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
if ((*st).show_roi AND $
    (widget_info( (*st).roi_mode, /DROPLIST_SELECT ) NE 0)) then begin
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
  
   0: $ ; cube
    begin
    xl=(*st).roi_params.xl  &  xh=(*st).roi_params.xh
    yl=(*st).roi_params.yl  &  yh=(*st).roi_params.yh
    plots, [xl,xh,xh,xl,xl], [yl,yl,yh,yh,yl], /DATA, $
    	   THICK=2, COLOR=blue, T3D=t3d 
    end
  
   3: $ ; T-range
    begin
    end
 endcase
endif ; (show_roi EQ 1)


plot_window, (*st).pw_id, /SHOW_MARKERS, XAXIS=xaxis, YAXIS=yaxis

;------------------------------------------------------------------------
;; Draw a graphic representing the ROI.
;------------------------------------------------------------------------
if (n_elements(*(*st).fit.x) GT 1) then $
  oplot, *(*st).fit.x, *(*st).fit.f_of_x, COLOR=green

;; Finally, show the dataset stats in the message field.
if show_stats then $
  widget_control, (*st).msg_label, SET_VALUE=selected_dataset.stats
return
end


;==========================================================================
;;; Event Handler Function
;==========================================================================
FUNCTION Dataset3dEventFn, Event

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
     
     plot_window, (*st).pw_id, DRAW_WIDGET=draw_widget, ZAXIS=zaxis
       
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
       minDist = min(distances,Imin)
       x_pt    = (*selected_dataset.x_filtered_data)(Imin)
       y_pt    = (*selected_dataset.y_filtered_data)(Imin)
       
       msg = string(x_pt,y_pt,f='("Closest datapoint: [",G11.5,", ",G11.5,"]")')
  
       widget_control, (*st).msg_label, SET_VALUE=strcompress(msg)
       tvcrs, x_pt, y_pt, /DATA
     endif
       
     if (mode NE 0) then begin
       bin = selected_dataset.binning
       if (mode EQ 4) then begin
         fn = selected_dataset.stat_map
         label = 'Statistic'
       endif else begin
         fn = selected_dataset.density
         label = 'Density sample'
       endelse

       col = round( (event.button_coord[0] - fn.x0) / bin.delta_x )
       row = round( (event.button_coord[1] - fn.y0) / bin.delta_y )
       
       xcen  = fn.x0 + col * bin.delta_x
       ycen  = fn.y0 + row * bin.delta_y
       
       if (col GE 0 AND col LT fn.xdim AND row GE 0 AND row LT fn.ydim) then begin
         if (mode EQ 4) then begin
           value = (*fn.samples)[col,row]
           msg = string( label, xcen, ycen, value,$
			 f='(A," at [",G11.5,", ",G11.5,"] = ",G11.5)')

	   ; Find the Z-data that went into the stat map for this point.
	   kernel_descriptor_location = (*fn.kernel_descriptor_location)[col,row]
	   
	   if (kernel_descriptor_location GE 0) then begin
	     ; Open the kernel descriptor file and find the specified descriptor.
	     openr, kernel_descriptor_unit, fn.kernel_descriptor_filename, /GET_LUN
	     point_lun, kernel_descriptor_unit, kernel_descriptor_location
	     
	     num_pixels = 0L
	     num_groups = 0L
	     readu, kernel_descriptor_unit, num_pixels, num_groups
     
	     groups_in_kernel = lonarr(num_groups)
	     readu, kernel_descriptor_unit, groups_in_kernel
	     free_lun, kernel_descriptor_unit

	     ;; Locate the data associated with each group.
	     gstart = (*selected_dataset.stat_map.group_start)[groups_in_kernel]
	     gstop  = (*selected_dataset.stat_map.group_stop) [groups_in_kernel]
         
	     num_data   = total( (gstop - gstart + 1) > 0 )

	     if (num_data GT 0) then begin
	       if (num_groups EQ 1) then begin
		 data = (*selected_dataset.stat_map.sorted_data)[gstart[0]:gstop[0]]
	       endif else begin
		 concat_array_segments, *selected_dataset.stat_map.sorted_data, gstart, gstop, data
	       endelse
	       
	       id = (*st).stat_group_widget
	       nam = string(num_pixels,xcen,ycen,$
	       		  f='(I0," pixels around [",G10.4,", ",G10.4,"]")')
	       dataset_1d, id, data, GROUP=top_base,  $
     	  	 WIDGET_TITLE='Data Associated with Statistic Map Entry',$
     	  	 DATASET_NAME=strcompress(nam), XTITLE=zaxis.title
	       (*st).stat_group_widget = id
	     endif ;(num_data GT 0)
	   endif ;(kernel_descriptor_location GE 0)
	   
	 endif else begin
	   ; Mode NE 4
           value = (*fn.samples)[col,row,selected_dataset.frame_index]
           msg = string( label, xcen, ycen, $
			fn.z0 + selected_dataset.frame_index * bin.delta_z,$
			value,$
			f='(A," at [",G11.5,", ",G11.5,", ",G11.5,"] = ",G11.5)')
         endelse
       endif else msg = ''
       widget_control, (*st).msg_label, SET_VALUE=msg
     endif

   endif ;(Event.middle_button)
   
   if (event.redraw) then RedrawDataset3d, st
   end


;--------------------------------------------------------------------------
; Dataset droplist
  (*st).dataset_list: $
   begin
   indexes          = where( (*st).datasets.name NE '' )
   names            = (*st).datasets[indexes].name
   (*st).selected_name = names[ event.index ]

   (*st).stale_titles = 1
   RedrawDataset3d, st
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
    4: widget_control, (*st).scaling_button, SET_VALUE='Map Scaling    '
   endcase
   
   animation_enabled = ((event.index NE 0) AND (event.index NE 4))
   widget_control, (*st).prev_frm, SENSITIVE=animation_enabled
   widget_control, (*st).next_frm, SENSITIVE=animation_enabled
   widget_control, (*st).frm_num,  SENSITIVE=animation_enabled
   widget_control, (*st).animate,  SENSITIVE=animation_enabled
   (*st).stale_titles = 1
   RedrawDataset3d, st
   end


;--------------------------------------------------------------------------
; Derived menu
  (*st).derived_menu: $
   begin
   case Event.value of
    ;----------------------------------------------------------------------
    1: $ ;Density Function or Stat Map Samples
     begin 
     mode = widget_info( (*st).mode_list, /DROPLIST_SELECT )
     if (mode EQ 4) then begin
       fn = selected_dataset.stat_map
       label = fn.subtitle
     endif else begin
       fn = selected_dataset.density
       label = '3-D Density Value'
     endelse
     
     if (fn.stale EQ 1) then begin
      msg=label+' is not available.'
      dummy=dialog_message(msg, /INF, DIALOG_PARENT=(*st).file_menu)
     endif else begin
      index = where(finite(*fn.samples), count)
      if (count GT 0) then begin
        id = (*st).samples_widget
        dataset_1d, id, (*fn.samples)[index], GROUP=top_base,  $
     		 WIDGET_TITLE=label+' Samples',$
     		 DATASET_NAME=selected_dataset.name,$
		 XTITLE=label
        (*st).samples_widget = id
		 
        txt=['The 3-D cube of density estimates, or 2-D statistic map values',$
             'have been formed',$
             'into a 1-D dataset and sent to a ''dataset_1d'' tool for analysis.']
        TimedMessage, msg_id, txt, GROUP=top_base, LIFE=15
      endif ;(count GT 0)
     endelse ; (fn.stale NE 1)
     end

    ;----------------------------------------------------------------------
    2: $ ;Combine Two Densities
     begin
     plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis, UNITY_ASP=unity_aspect
     align_images, XTITLE=xaxis.title, YTITLE=yaxis.title, UNITY_ASP=unity_aspect
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
       RedrawDataset3d, st
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
     
    2: $ ; SAVE DENSITY or STAT MAP
     begin
     mode = widget_info( (*st).mode_list, /DROPLIST_SELECT )
     if (mode EQ 4) then begin
       fn = selected_dataset.stat_map
       label = '2-D Statistic Map'
     endif else begin
       fn = selected_dataset.density
       label = '3-D Density'
     endelse
     
     if (fn.stale EQ 1) then begin
      msg=label+' is not available.'
      dummy=dialog_message(msg, /INF, DIALOG_PARENT=(*st).file_menu)
     endif else begin
      name = strcompress( selected_dataset.name + '.fits', /REMOVE_ALL )
      pathname = dialog_pickfile( GROUP=top_base, FILE=name(0), $
				 TITLE='Save '+label+' (FITS Array)' )
      widget_control, /HOURGLASS
     
      if (pathname NE '') then begin
       ; Make sure we can write to this file.
       openw, Unit, pathname, /GET_LUN, ERROR=Error
       if (Error NE 0) then begin
         message = 'ERROR opening file '+ pathname
       endif else begin
         free_lun, Unit
         
         plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis, ZAXIS=zaxis
                    
           ; Make up a FITS header.
           get_date, date_today
           mkhdr,    fits_header, *fn.samples, /EXTEND
           sxaddpar, fits_header, "CREATOR", "dataset_3d, $Revision: 4376 $"
           sxaddpar, fits_header, 'ORIGIN',  'Penn State University'
           sxaddpar, fits_header, 'FITSVERS',  'IDL Astronomy Users Library'
	   fdecomp, pathname, disk, item_path, item_name, item_qual
	   if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
	   sxaddpar, pheader, "FNFITS", item_name
	   sxaddpar, fits_header, 'DATE', date_today
       
           ; The FITS keywords CRVALn, CRPIXn, & CDELTn define a data coordinate
	   ; system in terms of the 1-based "pixel index" coordinates of the 
	   ; array stored in the file.
	   ;
	   ; (data - CRVALn) = CDELTn * (pixel - CRPIXn)
	   ;
	   ; 
	   sxaddpar, fits_header, 'CTYPE1', xaxis.title
           sxaddpar, fits_header, 'CTYPE2', yaxis.title
           sxaddpar, fits_header, 'CRPIX1', 1.0
           sxaddpar, fits_header, 'CRPIX2', 1.0
           sxaddpar, fits_header, 'CRVAL1', fn.x0
           sxaddpar, fits_header, 'CRVAL2', fn.y0
           sxaddpar, fits_header, 'CDELT1', selected_dataset.binning.delta_x
           sxaddpar, fits_header, 'CDELT2', selected_dataset.binning.delta_y
           sxaddpar, fits_header, 'BUNIT', 'Samples of '+label
           if (mode NE 4) then begin
            sxaddpar, fits_header, 'CTYPE3', zaxis.title
            sxaddpar, fits_header, 'CRPIX3', 1.0
            sxaddpar, fits_header, 'CRVAL3', fn.z0
            sxaddpar, fits_header, 'CDELT3', selected_dataset.binning.delta_z
           endif

           writefits, pathname, *fn.samples, fits_header
       endelse ; (Error EQ 0)
      endif ; (pathname NE '')
     endelse ; (fn.stale NE 1)
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
         message = 'ERROR opening file '+ pathname
       endif else begin
         free_lun, Unit
                             
         ; Make up a primary FITS header.
         get_date, date_today
         mkhdr, pheader, '', /EXTEND
         sxaddpar, pheader, "CREATOR", "dataset_3d, $Revision: 4376 $"
	 sxaddpar, pheader, 'ORIGIN',   'Penn State University'
	 sxaddpar, pheader, "FITSVERS", "IDL Astronomy Users Library"
	 fdecomp, pathname, disk, item_path, item_name, item_qual
	 if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
	 sxaddpar, pheader, "FNFITS", item_name
	 sxaddpar, pheader, 'DATE',     date_today
	 writefits, pathname, no_data, pheader
	 
         sxaddpar, fits_header, "EXTNAME", selected_dataset.name
         sxaddpar, fits_header, "CREATOR", "dataset_3d, $Revision: 4376 $"
	 sxaddpar, fits_header, 'ORIGIN',   'Penn State University'
	 sxaddpar, fits_header, 'DATE',     date_today
	 
	 plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis
	 sxaddpar, fits_header, 'XTITLE', xaxis.title  
	 sxaddpar, fits_header, 'YTITLE', yaxis.title  
	 

	 bin_table = replicate( {X: 0.0, Y:0.0, Z:0.0}, $
	   			  selected_dataset.num_data_points )


         bin_table.X        = *selected_dataset.x_filtered_data
         bin_table.Y        = *selected_dataset.y_filtered_data
         bin_table.Z        = *selected_dataset.z_filtered_data
         
         mwrfits, bin_table, pathname, fits_header            
       endelse ; (Error EQ 0)
     endif ; (pathname NE '')
     end
          
    4: $ ; LOAD DATASET
     begin
     txt=['If you choose a FITS FILE, then its first extension must be',$
          'a binary table.  The first three columns of the table are assumed',$
          'to contain the trivariate dataset.','',$
          'If you choose a ASCII FILE, then you will be presented with',$
          'the ASCII_TEMPLATE tool built into IDL.  Fill out the',$
   	  'three forms in this tool so that the first three fields extracted',$
   	  'contain the trivariate dataset.']
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
       dataset_3d, top_base, table.(0),table.(1),table.(2),DATASET_NAM=item_name
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
       RedrawDataset3d, st
     endif
     end
     
    6: $ ; NORMALIZATION TABLE
     begin
     prompt='Select an ASCII normalization table (T_value, frame_normalization)'
     result = routine_info( 'eb_parameter', /SOURCE )
     fdecomp, result.PATH, disk, dir, name, qual
     selected_dataset.tvnorm_file = $
     	  dialog_pickfile( GROUP=top_base, /MUST_EXIST, TITLE=prompt, $
			   PATH=dir(0),  FILTER='*.norm' )

     ; Save the dataset structure we've modified.
     selected_dataset.density.stale = 1B
     Save3dDataset, selected_dataset, st
     RedrawDataset3d, st
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
   
   if (roi_style EQ 0 OR roi_style EQ 3) then begin
     f3 = '0, FLOAT,' + string((*st).roi_params.zl) + $
	   ', TAG=zl, LABEL_LEFT=Min T, WIDTH=8'
	 
     f4 = '2, FLOAT,' + string((*st).roi_params.zh) + $
	   ', TAG=zh, LABEL_LEFT=Max T, WIDTH=8'
	   
     form = [form,f3,f4]
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

    f1b = '0, FLOAT,' + string(selected_dataset.binning.desired_delta_y) + $
	 ', TAG=desired_delta_y, LABEL_LEFT=Y, WIDTH=8'

    f1c = '2, FLOAT,' + string(selected_dataset.binning.desired_delta_z) + $
	 ', TAG=desired_delta_z, LABEL_LEFT=T, WIDTH=8'

    l1 = '0, LABEL,Actual binsizes:  ' + $
    	 string(selected_dataset.binning.delta_x) + ' ' + $
    	 string(selected_dataset.binning.delta_y) + ' ' + $
    	 string(selected_dataset.binning.delta_z) + ''


    b3 = '1, BASE,, ROW'

    f2a = '0, FLOAT,' + string(selected_dataset.binning.xbin_location) + $
	 ', TAG=xbin_location, LABEL_LEFT=Location of any bin corner: X,WIDTH=8'

    f2b = '0, FLOAT,' + string(selected_dataset.binning.ybin_location) + $
	 ', TAG=ybin_location, LABEL_LEFT=Y, WIDTH=8'
 
    f2c = '2, FLOAT,' + string(selected_dataset.binning.zbin_location) + $
	 ', TAG=zbin_location, LABEL_LEFT=T, WIDTH=8'
 
    form = [form,b0,b1,l0,b2,f1a,f1b,f1c,l1,b3,f2a,f2b,f2c,'2, LABEL,  ']


   if (mode EQ 4) then begin
    ; STATISTIC MAP PROPERTIES
    b0 = '1, BASE,, COLUMN, FRAME'
 
    l0 = '0, LABEL, STATISTIC MAP:, LEFT'
    
    f0 = '0, DROPLIST, MEAN|MEANCLIP|MEDIAN|"Background" via MMM|'+$
    	 'TOTAL|MAX|MIN|VARIANCE|STDDEV|Burst Length|Density,SET_VALUE=' +$
    	 string(selected_dataset.stat_map.stat_code)+ ', TAG=stat_code' +$
    	 ', LABEL_LEFT=Statistic:'

    f2 = '0, DROPLIST, circular kernel|"compatible flux" kernel,SET_VALUE=' +$
    	 string(selected_dataset.stat_map.require_compatible_flux)+ $
    	 ', TAG=require_compatible_flux, LABEL_LEFT=Kernel style:'

    f1 = '0, FLOAT,' + string(selected_dataset.stat_map.min_significance) + $
	 ', TAG=min_significance, LABEL_LEFT=Minimum significance (counts/sqrt(counts)), WIDTH=8'
 
    l1 = '2, LABEL, If min significance > 0 variable kernel size is used, LEFT'


   form = [form,b0,l0,f0,f2,f1,l1]
    
   endif else begin
    ; DENSITY PROPERTIES
    b0 = '1, BASE,, COLUMN, FRAME'
 
    l0 = '0, LABEL, DENSITY ESTIMATE (X&Y AXES):, LEFT'
    
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
        

    b0 = '1, BASE,, COLUMN, FRAME'
 
    l0 = '0, LABEL, DENSITY ESTIMATE (T AXIS):, LEFT'
    
    f0 = '0, DROPLIST, Histogram|Epanechnikov Smoothing|Gaussian Smoothing, '+$
          'SET_VALUE=' +string(selected_dataset.density.kernel_z)+ $
          ', TAG=kernel_z'
  
    b3 = '1, BASE,, ROW'
 
    f3 = '0, DROPLIST, 4|6|8|10|12|14|16|18|20|22|24|26|28|30, ' + $
          'LABEL_LEFT=Epanechnikov kernel full-width (bins), '+$
          'SET_VALUE='+$
    	  string((selected_dataset.density.Epanechnikov_width_z-4)/2) + $
    	 ', TAG=Epanechnikov_width_z'
  
    f4 = '2, FLOAT,' + string(selected_dataset.density.Gaussian_sigma_z) + $
 	', TAG=Gaussian_sigma_z, LABEL_LEFT=Gaussian kernel sigma (bins),WIDTH=8'
    form = [form,b0,l0,f0,b3,f3,f4,'2, LABEL,  ']
   endelse ;(mode NE 4)   


    f5 = '2, DROPLIST, This Dataset|All Datasets,'+$
         'LABEL_LEFT=Parameters apply to:, '+$
         'SET_VALUE='+string((*st).share_binning_params) +$
         ', TAG=share_binning_params'

    form = [form,f5]
   endif ;(mode NE 0)
   
   
   ;; Build a sub-form for image display parameters (since they are used both
   ;; in IMAGE and STAT MAP modes below.
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
   	    
    tv_form = [f0,f1,f2,f3,f4]
   
   
   case (mode) of
    0: $ ; SCATTER PLOT PROPERTIES
    begin
    b0 = '1, BASE,, ROW, FRAME'
    
    l0 = '0, LABEL, SCATTER PLOT:'
    
    f1 = '0, BUTTON, ' + $
         '+ Symbol|Asterisk|Dot|Diamond|Triangle|Box|X Symbol,'+$
	 'EXCLUSIVE, SET_VALUE=' + string(selected_dataset.plot.psym - 1) + $
	 ', TAG=psym, LABEL_TOP=Symbol Style'

    ; We have to translate the field "color", which is a string, into
    ; an index into the list of all available colors.
    color_manager, COLOR_NAMES=color_names
    color_index = 0 > (where(selected_dataset.plot.color EQ color_names))[0]
    color_list  = string(color_names, F='(99(A,:,"|"))' )
 
    f2 = '0, BUTTON,' + color_list + ',EXCLUSIVE, SET_VALUE=' +$
 	  string(color_index) + ', TAG=color, LABEL_TOP=Color'

    f3 = '2, DROPLIST, Omit|Left|Center|Right, SET_VALUE=' + $
   	string((*st).legend_style) + ', LABEL_LEFT=Legend, TAG=legend_style'
   	    
    form = [form,b0,l0,f1,f2,f3]
    end
    
    1: $ ; IMAGE PROPERTIES
    begin
    b0 = '1, BASE,, ROW, FRAME'
    
    l0 = '0, LABEL, IMAGE:'
    
    form = [form,b0,l0,tv_form]
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

    f2 = '2, TEXT,' + escape_commas( (*st).contour_kwds ) + $
        ', TAG=contour_kwds, WIDTH=30, LABEL_LEFT=  Keywords for CONTOUR routine'
   	
    form = [form,b0,l0,f1,f2]
    end

    
    4: $ ; STATISTIC MAP PROPERTIES
    begin
    b0 = '1, BASE,, ROW, FRAME'

    l0 = '0, LABEL, STATISTIC MAP:'

    form = [form,b0,l0,tv_form]
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
  
  (*st).note = r.note
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
  
  if (roi_style EQ 0 OR roi_style EQ 3) then begin
    (*st).roi_params.zl = r.zl
    (*st).roi_params.zh = r.zh
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
    selected_dataset.binning.desired_delta_z = r.desired_delta_z
    selected_dataset.binning.xbin_location   = r.xbin_location
    selected_dataset.binning.ybin_location   = r.ybin_location
    selected_dataset.binning.zbin_location   = r.zbin_location
    
    if ((compare_struct(b,selected_dataset.binning))[0].ndiff GT 0) then begin
      selected_dataset.density.stale  = 1B
      selected_dataset.stat_map.stale = 1B
      
      if (*st).share_binning_params then begin
        (*st).datasets.binning.desired_delta_x    = r.desired_delta_x
        (*st).datasets.binning.desired_delta_y    = r.desired_delta_y
        (*st).datasets.binning.desired_delta_z    = r.desired_delta_z
        (*st).datasets.binning.xbin_location      = r.xbin_location
        (*st).datasets.binning.ybin_location      = r.ybin_location
        (*st).datasets.binning.zbin_location      = r.zbin_location
        (*st).datasets.density.stale              = 1B
        (*st).datasets.stat_map.stale             = 1B
      endif
    endif


    if (mode EQ 4) then begin
     ; Stat map parameters.
     s=selected_dataset.stat_map
     selected_dataset.stat_map.stat_code         = r.stat_code
     selected_dataset.stat_map.min_significance  = r.min_significance
     selected_dataset.stat_map.require_compatible_flux=r.require_compatible_flux
    
     if ((compare_struct(s,selected_dataset.stat_map))[0].ndiff GT 0) then begin
      selected_dataset.stat_map.stale = 1B
      
      if (*st).share_binning_params then begin
        (*st).datasets.stat_map.stat_code        = r.stat_code
        (*st).datasets.stat_map.min_significance = r.min_significance
        (*st).datasets.stat_map.require_compatible_flux=r.require_compatible_flux
        (*st).datasets.stat_map.stale        = 1B
      endif
     endif

    endif else begin
     ; Density parameters.
     d=selected_dataset.density
     selected_dataset.density.kernel              = r.kernel
     selected_dataset.density.kernel_z            = r.kernel_z
     selected_dataset.density.Epanechnikov_width  = 4 + 2*r.Epanechnikov_width
     selected_dataset.density.Epanechnikov_width_z= 4 + 2*r.Epanechnikov_width_z
     selected_dataset.density.Gaussian_sigma      = r.Gaussian_sigma
     selected_dataset.density.Gaussian_sigma_z    = r.Gaussian_sigma_z

     if ((compare_struct(d,selected_dataset.density))[0].ndiff GT 0) then begin
      selected_dataset.density.stale  = 1B
      
      if (*st).share_binning_params then begin
        (*st).datasets.density.kernel             = r.kernel
        (*st).datasets.density.kernel_z           = r.kernel_z
        (*st).datasets.density.Epanechnikov_width = 4 + 2*r.Epanechnikov_width
        (*st).datasets.density.Epanechnikov_width_z=4 + 2*r.Epanechnikov_width_z
        (*st).datasets.density.Gaussian_sigma     = r.Gaussian_sigma
        (*st).datasets.density.Gaussian_sigma_z   = r.Gaussian_sigma_z
        (*st).datasets.density.stale              = 1B
      endif
     endif
    endelse ; Density parameters
  endif ;(mode NE 0)
  
  if (mode EQ 1 OR mode EQ 4) then begin
    ; Handle tvform items.
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
  endif
  
  case mode of
   0: begin
      selected_dataset.plot.psym = r.psym+1
      ; We have to translate "result.color", which is an index into
      ; the corresponding color name (string).
      selected_dataset.plot.color = color_names[r.color]
     
      (*st).legend_style = r.legend_style
      end
      
   1: begin
      end
      
   2: begin
      (*st).surface_mode = r.surface_mode
      end     
       
   3: begin
      ; We have to translate "result.color", which is an index into
      ; the corresponding color name (string).
      selected_dataset.plot.color = color_names[r.color]
    
      (*st).contour_kwds = strtrim(r.contour_kwds,2)
      end   
      
   4: begin
      end     
  endcase
  
   ; Save the dataset structure we've modified.
   Save3dDataset, selected_dataset, st
   RedrawDataset3d, st
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
   RedrawDataset3d, st
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

   if (roi_mode NE 0) then RedrawDataset3d, st
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
  
   0: $ ; cube
    begin
    (*st).roi_params.xl = big_marker[0] < small_marker[0]
    (*st).roi_params.xh = big_marker[0] > small_marker[0]
    (*st).roi_params.yl = big_marker[1] < small_marker[1]
    (*st).roi_params.yh = big_marker[1] > small_marker[1]
    end
  
   3: $ ; T-range
    begin
    end
   endcase
   
   ; If ROI is active then statistics for all datasets are now stale.
   if (roi_mode NE 0) then (*st).datasets.stats_stale  = 1B
   
   ; If ROI filtering is active then all filtered datasets are now stale.
   if (roi_mode EQ 2) then begin
     (*st).datasets.filter_stale = 1B
     new_event = { ID:top_base, TOP:event.top, HANDLER:0L}
   endif

   if (roi_mode NE 0) then RedrawDataset3d, st
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

   if (roi_mode NE 0) then RedrawDataset3d, st
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

   if ((mode EQ 1) OR (mode EQ 4)) then begin
  
     ;; Convert the current tvlow/tvhigh values to scale_midpt/scale_width
     ;; values.
     selected_dataset.scale_stale = 1
     TvDataset3d, selected_dataset, st, xaxis, yaxis, /VERBOSE, $
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
       
       TvDataset3d, selected_dataset, st, xaxis, yaxis, /VERBOSE, $
     		    SAVED_STATS=saved_stats
     endrep until 0
  
     widget_control, draw_widget, /CLEAR_EVENTS
     widget_control, msg_id, /DESTROY, BAD_ID=bad_id
     
     ; Save the dataset structure we've modified.
     Save3dDataset, selected_dataset, st
     TvDataset3d, selected_dataset, st, xaxis, yaxis, /VERBOSE, $
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
  
       RedrawDataset3d, st
     endrep until 0
  
     widget_control, draw_widget, /CLEAR_EVENTS
     widget_control, msg_id, /DESTROY, BAD_ID=bad_id
     
   endif 
   device, /CURSOR_ORIGINAL
   end


;--------------------------------------------------------------------------
; Previous & Next Frame buttons
  (*st).prev_frm: $
   begin
   selected_dataset.frame_index = 0 > (selected_dataset.frame_index-1) < $
   				     (selected_dataset.density.zdim-1)

   ; Save the dataset structure we've modified.
   Save3dDataset, selected_dataset, st
   (*st).stale_titles = 1
   RedrawDataset3d, st
   end
   
  (*st).next_frm: $
   begin
   selected_dataset.frame_index = 0 > (selected_dataset.frame_index+1) < $
   				     (selected_dataset.density.zdim-1)

   ; Save the dataset structure we've modified.
   Save3dDataset, selected_dataset, st
   (*st).stale_titles = 1
   RedrawDataset3d, st
   end

  (*st).frm_num: $
   begin
   widget_control, (*st).frm_num, GET_VALUE=frm_num
   selected_dataset.frame_index = 0 > frm_num < $
   				     (selected_dataset.density.zdim-1)

   ; Save the dataset structure we've modified.
   Save3dDataset, selected_dataset, st
   (*st).stale_titles = 1
   RedrawDataset3d, st
   end

;--------------------------------------------------------------------------
;Animate button
  (*st).animate: $
   begin
   mode = widget_info( (*st).mode_list, /DROPLIST_SELECT )
   if ((mode NE 0) AND (mode NE 4)) then begin
     plot_window, (*st).pw_id, DRAW_WIDGET=draw_widget
  
     coordsystem_manager, draw_widget, /RESTORE
     
     ;; Set up the animation widget.
     if (selected_dataset.density.kernel_z NE 0) then begin
       convolution_margin_z  = (selected_dataset.density.Epanechnikov_width_z-2)/2
       first_frame = convolution_margin_z
       last_frame  = selected_dataset.density.zdim - 1 - convolution_margin_z
     endif else begin
       first_frame = 0
       last_frame  = selected_dataset.density.zdim-1
     endelse
     
     num_frames = last_frame - first_frame + 1
       
     xinteranimate, /CLOSE
     xinteranimate, /SHOWLOAD, SET=[!D.X_SIZE, !D.Y_SIZE, num_frames], $
         	    MPEG_QUALITY=100
     
     ;; Create a STOP button for user.
     location = get_screen_size()/2 - [150,0]
     num = string(num_frames, FORMAT='(I0)')
     stop_base = widget_base( TITLE='Generating '+num+' animation frames ...',$
	                     /COLUMN, XOFF=location(0), YOFF=location(1) )
;    dum=widget_text(stop_base, VALUE=prompt, $
;    		    XSIZE=max(strlen(prompt)), YSIZE=n_elements(prompt))
     stop_button = widget_button( stop_base, VALUE='STOP', XSIZ=300 )
     widget_control, stop_base, /REALIZE, /HOURGLASS
     
     ;; Generate each animation frame.
     frame_index = selected_dataset.frame_index
     for ii = 0, num_frames-1 do begin
       selected_dataset.frame_index = first_frame + ii
       
       ; Save the dataset structure we've modified.
       Save3dDataset, selected_dataset, st
       (*st).stale_titles = 1
       RedrawDataset3d, st
       xinteranimate, FRAME=ii, WINDOW=!D.WINDOW

       event = widget_event( stop_button, /NOWAIT, BAD_ID=bad )
       widget_control, /HOURGLASS

       if (event.id EQ stop_button) then begin
         xinteranimate, /CLOSE
         GOTO, ABORT
       endif
     endfor
     
     xinteranimate, 50
ABORT:     
     widget_control, stop_base, /DESTROY, BAD_ID=bad
     
     ;; Display the original frame.
     selected_dataset.frame_index = frame_index
     Save3dDataset, selected_dataset, st
     (*st).stale_titles = 1
     RedrawDataset3d, st
     endif
   end
   
;--------------------------------------------------------------------------
; Message label (generates timer events for dataset_3d_helper)
  (*st).msg_label: $
   begin
   
   ;; These timer events occur when background_processing_state is non-zero.
   ;; One or more datasets has a background computation in progress.
   ;; Here, we manage those computations:
   ;;  - Explicitly kill the helper process corresponding to a stat map
   ;;    that's marked as stale.
   ;;  - Perform a few seconds of background processing, giving priority to
   ;;    the selected dataset.
   ;;  - When a background computation completes mark the stat map as complete.
   ;;  - Determine if any further background processing is needed, i.e. if
   ;;    background_processing_state should remain enabled.
   
   ;; Look for non-null datasets with stale stat maps and active helper processes.
   kill_helper = ((*st).datasets.name NE '') AND $
   		 ((*st).datasets.stat_map.stale EQ 1) AND $
   		 ((*st).datasets.stat_map.helper_unit NE 0)
   
   ind = where(kill_helper, count)
   for ii = 0, count-1 do begin
     print, 'killing abandoned helper process'
     free_lun, (*st).datasets[ind[ii]].stat_map.helper_unit 
     (*st).datasets[ind[ii]].stat_map.helper_unit = 0
   endfor
   
   ;; First, assume there is no dataset to process.
   ds_index = -1

   ;; Look for all active helper processes that need some CPU time.
   active_helper = ((*st).datasets.name NE '') AND $
   		   ((*st).datasets.stat_map.stale EQ 3) 
   
   ;; Our low priority choice is the first active process.
   ind = where(active_helper, count)
   if (count GT 0) then ds_index = ind[0]

   ;; Our high priority choice is the selected dataset, if active.
   ind = where(active_helper AND ((*st).datasets.name EQ (*st).selected_name), count)
   if (count GT 0) then ds_index = ind[0]
   
   if (ds_index EQ -1) then begin
     ; If no datasets need processing then disable these TIMER events.
     (*st).background_processing_state = 0
   endif else begin
     ; Otherwise process the chosen active dataset.
     dataset = (*st).datasets[ds_index]
     s = dataset.stat_map
     ProcessKernelDescriptors, s, st
     
     ; If we've completed the map then mark the map as complete.
     ; We do not however close the unit because that would remove the
     ; kernel descriptor file, which we need to retain to lookup 
     ; kernel members in response to mouse clicks.
     ; We do not explicitly kill the helper process either -- the design 
     ; is for it to kill itself when finished.
     if (s.samples_completed EQ (s.xdim*s.ydim)) then s.stale = 0
     
     dataset.stat_map = s

     ; Redraw the display.
     dataset.tv_stale = 1
     Save3dDataset, dataset, st
     RedrawDataset3d, st
   endelse 
   end
   
;--------------------------------------------------------------------------
  else: print, 'unknown event in dataset_3d'
endcase

;; If necessary, set up the next timer event for background processing.
if ((*st).background_processing_state GT 0) then begin
  now = systime(1)
  if ((now - (*st).time_of_last_user_event) LT 10) then begin
    ; User is active, so slow down background processing.
    delay = 10
  endif else begin
    ; User seems idle, so if background processing seems to have plenty to
    ; do then don't pause much.
    delay = ((*st).background_processing_state EQ 2) ? 0.1 : 10
  endelse

;print, 'SETTING TIMER', delay 
  widget_control, (*st).msg_label, TIMER=delay
  if (Event.ID NE (*st).msg_label) then (*st).time_of_last_user_event = now
endif


if DestroyFlag then widget_control, (*st).parent, /DESTROY

return, new_event
end



;==========================================================================
;;; Event Handler Procedure
;==========================================================================
PRO Dataset3dEvent, Event
 
event = Dataset3dEventFn( Event )
return
end


;==========================================================================
;;; MAIN "dataset_3d" ROUTINE
;==========================================================================

PRO dataset_3d, top_base, x_data, y_data, z_data, _EXTRA=extra, $
		 TITLE=title, $
		 XTITLE=xtitle, YTITLE=ytitle, ZTITLE=ztitle, $
		 UNITY_ASPECT=unity_aspect, LEGEND_STYLE=legend_style, $
		 XWCS=xwcs, YWCS=ywcs, NAN_VALUES=nanval,$
		 DATASET_NAME=dataset_name, DESCRIPTION=description, $
		 XBIN=xbin, YBIN=ybin, TBIN=tbin, STAT_CODE=stat_code,$
		 HIDE_DATASET=hide_dataset, DISABLE_DELETE=disable_delete, $
		 DELETE=delete, ROI_MASK=mask, REDRAW=redraw, $
		 GET_DATASETS=get_datasets, $
     PLOT_WINDOW_OPTIONS=plot_window_options, PS_CONFIG=ps_config, PRINT=print_now

		 
;; If the widget ID of an existing dataset_3d was not passed, then create 
;; the widget.
create_flag = 1
if (n_elements(top_base) EQ 1) then begin
  if (widget_info( top_base, /VALID_ID )) then create_flag = 0
endif

if (create_flag) then top_base = CreateDataset3d( _EXTRA=extra )

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
;; But, we do want to apply some keywords to all dataset slots. 
if (create_flag AND (N_ELEMENTS(y_data) EQ 0)) then begin
  if (n_elements(stat_code) EQ 1) then begin
    widget_control, (*st).mode_list, SET_DROPLIST_SELECT=4
    (*st).datasets.stat_map.stat_code = stat_code
  endif
  
  return
endif

;;---------------------------------------------------------------------------
;; Return all the visible datasets if desired.
if arg_present(get_datasets) then begin
  indexes = where( ((*st).datasets.hidden_flag EQ 0) AND $
       		    (*st).datasets.name NE '', count )

  get_datasets = 0
  if (count GT 0) then get_datasets = (*st).datasets[indexes]
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
if (0 NE n_elements(ztitle))   then plot_window, (*st).pw_id, ZTITLE=ztitle
if keyword_set(unity_aspect)   then plot_window, (*st).pw_id, /FORCE_UNITY_ASP
if (0 NE n_elements(title))    then plot_window, (*st).pw_id, TITLE=title

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
    dataset_name = '3D dataset'
endif else begin
  ; Neither a name nor data were passed.  Assume the caller is refering to the last dataset.
  ind = where( (*st).datasets.name NE '', count )
  
  dataset_name = (count EQ 0) ? '3D dataset' : ((*st).datasets.name)[ind[count-1]]
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
  
    is_finite = finite(x_data) AND finite(y_data) AND finite(z_data)

    good_ind = where(is_finite, num_data_points, $
				COMPLEMENT=null_ind, NCOMPLEMENT=num_null)
    dataset.num_data_points = num_data_points > 1

    if            (num_null EQ 0)               then begin
      ; All the data are finite
      *dataset.x_data = x_data
      *dataset.y_data = y_data
      *dataset.z_data = z_data 
      
    endif else if (n_elements(nanval) GT 0) then begin
      ; Replace NaN/Inf with supplied values
      *dataset.x_data = x_data
      *dataset.y_data = y_data
      *dataset.z_data = z_data 

      (*dataset.x_data)[null_ind] = nanval[0]
      (*dataset.y_data)[null_ind] = nanval[1]
      (*dataset.z_data)[null_ind] = nanval[2]     
    
    endif else if (num_data_points GT 0) then begin
      ; Keep just the good data
      *dataset.x_data = x_data[good_ind]
      *dataset.y_data = y_data[good_ind]
      *dataset.z_data = z_data[good_ind] 
      
    endif else begin
      ; There are no good data points
      print, 'dataset_3d: WARNING! All data are Nan/Inf'
      *dataset.x_data = [0]
      *dataset.y_data = [0]
      *dataset.z_data = [0] 
    endelse
        
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
    dataset.plot.color		= 'white'
    dataset.plot.psym		= 3B
    dataset.scale_midpt         = 0.25
    dataset.scale_width         = 0.5
    dataset.tv_stale		= 1
        
    (*st).stale_titles = 1
  endif
  
  
;;---------------------------------------------------------------------------
;; We delete a dataset merely by setting its name to ''.
if keyword_set(delete) then begin
    dataset.name = ''
    *dataset.x_data = [0]
    *dataset.y_data = [0]
    *dataset.z_data = [0]
    dataset.num_data_points = 1
endif
  
  
;;---------------------------------------------------------------------------
;; Handle the keywords that modify a dataset.
if (0 NE n_elements(description))   then dataset.description = description
if (0 NE n_elements(hide_dataset))  then dataset.hidden_flag = hide_dataset 
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

if (n_elements(tbin) NE 0) then begin
  dataset.binning.desired_delta_z = abs(tbin)
  dataset.filter_stale = 1B
endif

if (n_elements(stat_code) EQ 1) then begin
  widget_control, (*st).mode_list, SET_DROPLIST_SELECT=4
  dataset.stat_map.stat_code = stat_code
endif


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
    if ((*st).legend_style EQ 0) && (dataset_index GT 0) then (*st).legend_style = 1
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
  ApplyDataset3dRoi, dataset, st
  mask = dataset.mask
  redraw_flag = 0
endif


;;---------------------------------------------------------------------------
;; Store the dataset structure we've been modifying.
Save3dDataset, dataset, st


;;---------------------------------------------------------------------------
;; The REDRAW keyword overrides redraw_flag calculated above.
if (n_elements(redraw) EQ 1) then redraw_flag = redraw
if (widget_info(top_base, /REALIZED) AND redraw_flag) then begin
  mode = widget_info( (*st).mode_list, /DROPLIST_SELECT )
  dum  = Dataset3dEventFn({handler:top_base, id:(*st).mode_list, index:mode})
endif

;;---------------------------------------------------------------------------
;; Print without user intervention, if directed.
if keyword_set(print_now) then begin
  (*st).print_now = 1
  event={ID:(*st).file_menu, TOP:top_base, HANDLER:top_base, VALUE:1}
  Dataset3dEvent, Event
endif

return
END





PRO burst_test, N, x, y, e

x=intarr(N)
y=x
e=x
d=random(N)+0.25
for ii=1,N-1 do e[ii]=e[ii-1] + round(d[ii]) + 1
x[0]=1
y[1]=1
return
end

PRO group_test1
bx=10*random(1000)
by=10*random(1000)
sx=5+random(300,/NORM)
sy=5+random(300,/NORM)
x=[bx,sx] & y=[by,sy] & z=replicate(1,n_elements(x))
dataset_3d,id3,x,y,z, XBIN=0.5,YBIN=0.5, STAT=10, DATA='sig>0'
dataset_3d,id3,x,y,z, XBIN=0.5,YBIN=0.5, STAT=10, DATA='sig=0'
dataset_2d,id2,x,y, XBIN=0.5,YBIN=0.5

return
end

PRO group_test2
N=200
bx=random(N)
by=random(N)
bz=random(N)
sxy=replicate(0.5,16)
sz=replicate(2,16)
x=[bx,sxy] & y=[by,sxy] & z=[bz,sz]
dataset_3d,id3,x,y,z, XBIN=0.1,YBIN=0.1, STAT=0, DATA='compatible flux kernel'
dataset_3d,id3,x,y,z, XBIN=0.1,YBIN=0.1, STAT=0, DATA='flat kernel'

return
end

PRO speed_test
N=2e6
bx=random(N)
by=random(N)
bz=random(N)
dataset_3d,id3,bx,by,bz, XBIN=0.001,YBIN=0.001, STAT=0, DATA='2e6'
return
end
