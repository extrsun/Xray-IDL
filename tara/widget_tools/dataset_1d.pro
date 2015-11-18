;+
;========================================================================
;;;
;;; Dataset_1D Widget: $Id: dataset_1d.pro 4378 2012-10-30 20:18:15Z psb6 $
;;;
;;; Copyright (C) 1998, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION:  
;;; This tool may be used to analyze the distribution of 1D datasets.
;;;
;========================================================================
;;; EVENTS GENERATED
;;; An event is forwarded to the client whenever the dataset is filtered.
;;;
;========================================================================
;;; INTERFACE TO CLIENT
;;;
;;; dataset_1d, top_base, x_data, $
;		 PARENT_WIDGET=parent, GROUP=group, BLOCK=block, $
;		 WIDGET_TITLE=widget_title,$
;		 XTITLE=xtitle, XWCS=xwcs, LEGEND_STYLE=legend_style, NAN_VALUES=nanval,$
;		 DENSITY_TITLE=density_title, $
;		 DISTRIBUTION_TITLE=distribution_title, $ 
;			   
;		 NORMALIZE_DENSITY=normalize_density, $
;		 NORM_ABSC=norm_absc, NORM_VAL=norm_val, $
;		 BINSIZE=binsize, BIN_LOCATION=bin_location,$
;			   
;		 DATASET_NAME=dataset_name, DESCRIPTION=description, $
;		 METRIC=metric, COLOR=color, LINESTYLE=linestyle, PSYM=psym, PLOTSYM=plotsym, $
;		 HIDE_DATASET=hide_dataset, DISABLE_DELETE=disable_delete, $
;		 DELETE=delete,  ROI_MASK=mask, REDRAW=redraw, $
;
;    PLOT_WINDOW_OPTIONS=plot_window_options, PS_CONFIG=ps_config, PRINT=print_now
;;;
;;; The parameters PARENT_WIDGET & GROUP are the usual ones associated 
;;; with widgets.  If dataset_1d is a top-level widget, then WIDGET_TITLE
;;; controls the title in the X-window frame.
;;;
;;; COLOR can have the values 'red', 'blue', 'green', or 'white'.
;;;
;;; LINESTYLE can have the values 6 (none), 0 (solid), 1 (dotted), 
;;; 2 (dashed), 3 (dash dot), 4 (dash dot dot dot), 5 (long dashes).
;;;
;;; PSYM can have the values 0 (none), 1 (+), 2 (*), 3 (.), 
;;; 4 (diamond), 5 (triangle), 6 (box), 7 (X), 10(histogram),
;;; 8 (defined by plotsym.pro; see below).
;;;
;;; PLOTSYM is a string containing the parameters passed to plotsym.pro (AstroLib).  
;;; The most simple PLOTSYM would be just a string containing a number from 0...8, e.g. 
;;; PLOTSYM='4'.  A more complex example is PLOTSYM='7, 1.3, THICK=2'
;;; Pass PSYM=8 when using PLOTSYM.

;;; The dataset_1d can work with multiple named datasets.
;;; Multiple datasets are passed with multiple calls to dataset_1d -- see
;;; example below.
;;; A dataset may be deleted by the caller using /DELETE.

;;; The keywords BINSIZE & BIN_LOCATION define the bin grid used to compute 
;;; density functions.

;;; The keywords NORM_ABSC & NORM_VAL describe a function 
;;; norm_val = f( norm_absc ) which is used to normalize the density functions
;;; computed by this tool.  For example, if the dataset you're analyzing
;;; consists of the distances from a bunch of 2-D datapoints to a reference
;;; point and you'd like this tool to compute radial surface brightness, 
;;; then the normalization function you want to pass is
;;;   norm_val = 2 * !PI * norm_absc  {The circumference of a circle.}
;;; Then, the "density function" computed by this tool at distance "R" is
;;;   (N / binsize) / (2 * !PI * R) = N / (binsize * 2 * !PI * R)
;;; where
;;;   N = (# datapoint distances between R-binsize/2 and R+binsize/2)
;;; This is the number of datapoints in an annulus divided by the area
;;; of the annulus, which is what you wanted!

;;; The state of this widget is stored as an anonymous structure in the 
;;; user value slot of the first child (the normal convention).


;==========================================================================
;;;                     CALLING OPTIONS

;;; From a non-widget program or the command line, analyze vector X. 
;;; Widget events will be processed when the command line returns.
;;; ** dataset_1d, id, X

;;; From a non-widget program analyze vector X but block further
;;; program execution until the dataset_1d widget is destroyed.
;;; ** dataset_1d, id, X, Y, /BLOCK
;;; ** xmanager

;;; From a widget program, create a top-level dataset_1d.
;;; ** dataset_1d, id, X, GROUP=creator_widget_id

;;; Create a dataset_1d as a child of another widget.
;;; ** dataset_1d, id, X, PARENT_WIDGET=base

;;; Delete a dataset from an existing dataset_1d.
;;; ** dataset_1d, id, /DELETE, DATASET_NAME=name

;;; Obtain a pointer to a byte array that shows which data points in the 
;;; specified dataset are retained by the user's filter.  If no filter
;;; is defined, then the pointer points to an undefined variable.
;;; ** dataset_1d, id, DATASET_NAME=name, ROI_MASK=mask
;==========================================================================
;-

;==========================================================================
;;; Create the widget.
;==========================================================================
FUNCTION CreateDataset1d, PARENT_WIDGET=parent, GROUP=group, BLOCK=block, $
			  WIDGET_TITLE=widget_title

;; Call color_manager to switch to DirectColor if available.
color_manager

;; Default values
if (0 EQ n_elements(group))          then group = 0
if (0 EQ n_elements(widget_title))   then widget_title = 'Dataset_1D'
  

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
			EVENT_FUNC='Dataset1dEventFn', $
			KILL_NOTIFY='Dataset1dCleanup', $
			/COLUMN, /SPACE, XPAD=0, YPAD=0 )

upper_base = widget_base( top_base, /ALIGN_LEFT, /ROW, $
			   SPACE=8, XPAD=0, YPAD=0 )
			   
 menu = [{ CW_PDMENU_S, flags:1, name:'File' }, $ 
	 { CW_PDMENU_S,   0,      'Print' }, $
	 { CW_PDMENU_S,   0,      'Save Selected Density (FITS)' }, $ 
	 { CW_PDMENU_S,   0,      'Save All Densities (FITS)' }, $ 
	 { CW_PDMENU_S,   0,      'Save Selected Distribution (FITS)' },$
	 { CW_PDMENU_S,   0,      'Save All Distributions (FITS)' },$
	 { CW_PDMENU_S,   0,      'Save 1-D Dataset (FITS)' }, $
	 { CW_PDMENU_S,   0,      'Load 1-D Dataset (FITS or ASCII)' }, $
	 { CW_PDMENU_S,   0,      'Delete Dataset' }, $
	 { CW_PDMENU_S,   2,      'Hide/Show Datasets' }]

 if this_is_top_level then begin
   menu[9].flags = 0
   menu = [menu, { CW_PDMENU_S,     2,      'Exit' }]
 endif 
 
 file_menu = cw_pdmenu(upper_base, menu, /RETURN_INDEX)
     
 edit_button    = widget_button( upper_base, VALUE='Edit' )
 
 roi_base = widget_base(upper_base, /ROW, /SPACE, XPAD=0, YPAD=0, /FRAME)
 
   roi_mode = widget_droplist( roi_base, VALUE=['None','Stats','Filter'], $
   				  TITLE='ROI:' )
   
   roi_define = widget_button( roi_base, VALUE='Use Markers' )
  
   roi_semantics = widget_droplist( roi_base, VALUE=['Include','Exclude'] )

 menu = [{ CW_PDMENU_S, flags:1, name:'Analysis' }, $ 
	 { CW_PDMENU_S,   0,   'Define Model' }, $ 
	 { CW_PDMENU_S,   0,   'Fit Selected Dataset' }, $ 
	 { CW_PDMENU_S,   0,   'Fit All Datasets' }, $ 
	 { CW_PDMENU_S,   0,   'Export Fit Function' },$ 
	 { CW_PDMENU_S,   0,   'Export Density Function' },$ 
	 { CW_PDMENU_S,   2,   'Distribution of Density Function Samples' }]
	 
 derived_menu = cw_pdmenu(upper_base, menu, /RETURN_INDEX)


middle_base = widget_base( top_base, /ALIGN_LEFT, /ROW, $
			   /SPACE, XPAD=0, YPAD=0 )

 dataset_list = widget_droplist( middle_base, VALUE='NULL', /DYNAMIC_RESIZE )
 
 modes = ['Scatter Plot', 'Density Function', 'Distribution Function']
 mode_list = widget_droplist( middle_base, VALUE=modes )
 widget_control, mode_list, SET_DROPLIST_SELECT=1

msg_label = widget_label( top_base, /DYNAMIC_RESIZE, VALUE=' ' )
    
plot_window, pw_id, PARENT=top_base



; Setup state structure.
dataset = { index:0B, name:'', description:'', metric:0.0, $
	    hidden_flag:0, disable_delete:0,$

	    x_data:ptr_new(), $

      norm_absc:ptr_new(), $
      norm_val :ptr_new(),$
	    
	    x_filtered_data:ptr_new(),  $
	    mask:ptr_new(), filter_stale:1B, $
	    stats:'', stats_stale:1B, $
	    
	    ; num_data_points, x_max, x_min refer to the filtered dataset!
	    num_data_points:0L, x_max:0.0, x_min:0.0, $

	    
	    plot: {color:'white', psym:3B, plotsym:'', line:0, show_errors:0, error_skip:4}, $
	    
	    binning:   {desired_delta_x:0.0,  $
	    		delta_x:0.0,  $
	    		xbin_location:0.0},$


	    		;;0=histogram, 1=Epanechnikov kernel, 2=Gaussian kernel
	    		;;Epanechnikov_width must be EVEN
	    		;;Gaussian sigma is in units of bins
	    density:   {kernel:0, Epanechnikov_width:4, Gaussian_sigma:1.0, $ 
	    		bandwidth_x:1.0, $
	    		subtitle:'', $
	    		x_min:0.0, x0:0.0, xdim:0L, $
	    		samples:ptr_new(), error:ptr_new(), stale:0B }, $

	    		
	    distribution: {samples:ptr_new(), stale:0B}, $
	    
	    fit: {samples:ptr_new(), stale:0B, name:'', number:0} $
	   } 		
	    
r2=[0.0,0.0]
wcs_object, wcs, /INIT
state = { parent:parent, $ 
	;IDs of widgets that generate events or need to be updated. 
	roi_mode:roi_mode, $
	roi_define:roi_define, roi_semantics:roi_semantics,$
	show_roi:1, $
	
	dataset_list:dataset_list, $
	file_menu:file_menu, edit_button:edit_button, $
	derived_menu:derived_menu, fit_setup:0L, $
	mode_list:mode_list, $
	msg_label:msg_label, pw_id:pw_id, $
	
	;Dataset structures
	selected_name:'', datasets: replicate( dataset, 30 ), $
	
	;ROI parameters
	roi_params: {xl:1., xh:0.}, $

	; Model information
	gauss:replicate({amplitude:0., mean:0., sigma:1., freeze_amp:1, $
			       freeze_mean:0, freeze_sigma:0}, 12), $
	poly: replicate({coeff:0., freeze_coeff:1, exponent:0.0, $
	      		       low:-1E10, high:1E10}, 12), $
	      	
	;Other state information.
	normalize_density:0, $
	legend_style:0, share_binning_params:1, $
	note:'', note_x:0.0, note_y:0.0, stale_titles:1, $
	
	density_title:'',      density_wcs:wcs, $
	distribution_title:'', distribution_wcs:wcs, $
	
	density_widget:0L, samples_widget:0L, ps_config:ptr_new(/ALLOC), print_now:0B }


	  
;; Allocate heap variables in dataset structures.
color_manager, COLOR_NAMES=color_names  & nc = n_elements(color_names)-1
line =[0,1,2,3,4,5]	& nl = n_elements(line)
for ii = 0, n_elements(state.datasets)-1 do begin
  state.datasets[ii].index                = ii
  state.datasets[ii].x_data               = ptr_new([0])
  state.datasets[ii].x_filtered_data      = ptr_new([0])
  state.datasets[ii].mask                 = ptr_new(/ALLOC)
  state.datasets[ii].density.samples      = ptr_new(/ALLOC)
  state.datasets[ii].density.error        = ptr_new(/ALLOC)
  state.datasets[ii].distribution.samples = ptr_new(/ALLOC)
  state.datasets[ii].fit.samples          = ptr_new(/ALLOC)
  
  state.datasets[ii].plot.color = color_names[ ii         mod nc]
  state.datasets[ii].plot.line  =       line [(ii  /  nc) mod nl]
endfor

state.gauss[0].amplitude    = 1
state.gauss[0].freeze_amp   = 0
state.poly [0].freeze_coeff = 0

;; Save state structure.
widget_control, top_base, SET_UVALUE=ptr_new(state, /NO_COPY)


;; If this is top-level widget, realize it and register.
if this_is_top_level then begin
  widget_control, parent, /REALIZE

  ; The fancy combination of JUST_REG and NO_BLOCK is necessary to get the
  ; widget to behave when called by either widget applications or normal
  ; programs (which block on tty reads).
  xmanager, 'dataset_1d', parent, GROUP_LEADER=group, $
	    JUST_REG=keyword_set(block), NO_BLOCK=(keyword_set(block) EQ 0), $
	    EVENT_HANDLER='PlotWindowTopbaseEventFn'
endif

return, top_base
END

;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO Dataset1dCleanup, top_base

widget_control, top_base, GET_UVALUE=st

;; Free the heap vars allocated locally.
ptr_free, (*st).datasets.x_data, (*st).datasets.x_filtered_data, $
          (*st).datasets.norm_absc, (*st).datasets.norm_val, $
          (*st).datasets.mask, (*st).datasets.density.samples, $
          (*st).datasets.density.error, (*st).datasets.distribution.samples, $
	  (*st).datasets.fit.samples, $
	  (*st).ps_config
ptr_free, st
return
end


;==========================================================================
;;; Save a dataset structure back to the widget state structure.
;==========================================================================
PRO Save1dDataset, dataset, st
(*st).datasets[ dataset.index ] = dataset
return
end


;==========================================================================
;;; Routine to filter a dataset or compute ROI statistics.
;;; The structure 'dataset' is modified!
;==========================================================================
PRO ApplyDataset1dRoi, dataset, st

;; First, see if the filter has already been applied.
if (dataset.filter_stale EQ 0 AND dataset.stats_stale EQ 0) then return

area = 0
	
;; If there is no filter defined, then just point to full dataset and
;; set the mask to all 1's.
roi_mode  = widget_info( (*st).roi_mode, /DROPLIST_SELECT )

if (roi_mode EQ 0 ) then begin
  x_roi_data = dataset.x_data
    
  mask = replicate( 1B, n_elements(*dataset.x_data) )
  
endif else begin

  ;; If we get here, then there is a ROI defined and we need to apply it.
  ;; Remember that the result  could be the empty set, represented by
  ;; x_roi_data pointing to an undefined heap var.
  roi    = (*st).roi_params
  x_data = dataset.x_data
  
  ;; Initialize the ROI if it has never been assigned by user or if it is
  ;; null.
  if (roi.xl GT roi.xh) then begin
    roi.xh = max(*x_data, MIN=x_min)
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
    x_roi_data = ptr_new(/ALLOC)
  endif else begin
    x_roi_data = ptr_new( (*x_data)[index], /NO_COPY)
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
    dataset.stats = string( num_points, f='(I0," datapoints")' )
    
    if (area GT 0) then begin  
      dataset.stats = dataset.stats + $
        string(area, num_points/float(area), $
	       f='(" (range=",G10.4,"; density=",G10.4,")")' )
    endif
    
    if ~ptr_valid( dataset.norm_absc ) then begin
      if (num_points GE 2) then begin
        x_median = median(*x_roi_data, /EVEN)
        xcen = (moment( *x_roi_data, SDEV=sdev, /DOUBLE ))[0]
      endif else begin
        x_median = 0 & xcen = 0 & sdev = 0
      endelse
      
      dataset.stats = dataset.stats + $
        string(x_median,xcen,sdev,f='("; median=",G0.5," mean=",G0.5,"; sample stdev=",G10.4)')
    endif
     
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
  	
  ;; Is filtering selected?
  if (widget_info( (*st).roi_mode, /DROPLIST_SELECT ) NE 2) then begin
    dataset.x_filtered_data  = dataset.x_data
    *dataset.mask = 0
    dum = temporary( *dataset.mask )
  endif else begin
    dataset.x_filtered_data = x_roi_data
    
    *dataset.mask = temporary( mask )
  endelse

  ;; Find the max and min of the filtered dataset.
  dataset.num_data_points = n_elements( *dataset.x_filtered_data )
  
  if (dataset.num_data_points EQ 0) then begin
    x_min = 0  &  x_max = 1 
  endif else begin
    x_max = max(*dataset.x_filtered_data, MIN=x_min)
  endelse  
  
  dataset.x_max           = x_max
  dataset.x_min           = x_min
  
  
  ;; Since we've changed the filtered dataset, mark all derived data structures
  ;; as stale.
  dataset.density.stale   = 1
  
endif ; (dataset.filter_stale EQ 1)

return
end

;==========================================================================
;;; Routine to define a binning region and bin up univariate data.
;;; x0 is the coordinate of the center of the first bin.
;==========================================================================
PRO Bin1dDataset, dataset, MARGIN=margin, $
		  x_min, x0, num_col, col, DESIRED_X_MIN=desired_x_min

if (n_elements(margin) NE 1) then margin = 0

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


;; If necessary, increase the sample interval (delta_x) requested 
;; until a reasonably sized array results.
maxdim = 1e6
repeat begin
  xdim = (dataset.x_max - dataset.x_min) / bin.delta_x

  done = 1
  if (xdim GT maxdim) then begin
    bin.delta_x = 2 * bin.delta_x
    done      = 0
  endif
endrep until (done)

dataset.binning = bin


;; Calculate the signed distance from the smallest datapoint to the bin corner
;; the user specified.
datamin_to_edge_distance_x = dataset.x_min - bin.xbin_location

;; Calculate the location of the bin corner which is <= the smallest 
;; datapoint.
x_min = floor( datamin_to_edge_distance_x / bin.delta_x ) * bin.delta_x $
        + bin.xbin_location

;; Add the desired margin to the left & bottom edges of the binning region.	
x_min = x_min - bin.delta_x * margin[0]
  	     
;; If a different value for x_min was requested, try to comply.
if (n_elements(desired_x_min) NE 0) then begin
  if (desired_x_min GT x_min) then message, 'desired_x_min too large'
  x_min = desired_x_min
endif

;; Next, discretize the data vectors.
col = floor( (*dataset.x_filtered_data - x_min) / bin.delta_x ) 

num_col = max(col)+1 
 
;; Add the desired margin to the right & top edges of the binning region.	
num_col = num_col + margin[0]

;; Compute x0 the coordinate of the center of the first bin.
x0 = x_min + bin.delta_x/2.0

return
end


;==========================================================================
;;; Routine to estimate a density function, sampled on a uniform 1-D grid.
;;; The structure 'dataset' is modified!
;;; The phase of the sample grid is controlled by the parameter 
;;; (*st).binning.xbin_location.
;;;
;;; The structure tag x0 refers to the data coordinate of the CENTER of the
;;; first bin.
;==========================================================================
PRO ComputeDataset1dDensity, dataset, st, DESIRED_X_MIN=desired_x_min

d = dataset.density

;; First, see if the density has already been calculated.
if (d.stale EQ 0) then return

d.stale = 0
dataset.distribution.stale = 1
dataset.fit.stale          = 1

;; Now, make sure there is some data left after filtering.
if (dataset.num_data_points EQ 0) then begin
  *d.samples = lonarr(4)
  *d.error   = lonarr(4)
  d.xdim = 4 
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
  
;; If a different value for x_min was requested, try to comply.
if (n_elements(desired_x_min) NE 0) then begin
  Bin1dDataset, dataset, MARGIN=convolution_margin,$
	        x_min, x0, num_col, col, DESIRED_X_MIN=desired_x_min
endif else begin
  Bin1dDataset, dataset, MARGIN=convolution_margin,$
	        x_min, x0, num_col, col
endelse
	      
bin    = dataset.binning
d.x_min= x_min
d.x0   = x0
d.xdim = num_col

msg = string(bin.delta_x,  $
  	f='("Computing histogram with binssize ",G10.4)' )
widget_control, (*st).msg_label, SET_VALUE=msg
  
  
;; Compute the histogram.
*d.samples = histogram( col, MIN=0L, MAX=num_col-1 )

;; Now, apply kernel smoothing if requested.
case (d.kernel) of
 0: $
  begin
  ;; HISTOGRAM
  d.subtitle = strcompress( string( bin.delta_x, f='("Binsize: ",G10.4)' ) )
  
  ;Estimate Poisson errors with Gehrels formula  
  *d.error = 1 + sqrt( *d.samples + 0.75)
  end

 1: $
  begin
  ;; EPANECHNIKOV SMOOTHING
  d.bandwidth_x = bin.delta_x * d.Epanechnikov_width/2

  d.subtitle = strcompress( string( d.bandwidth_x, $
  		   f='("Epanechnikov kernel bandwidth: ",G10.4)' ) )

  widget_control, (*st).msg_label, SET_VALUE='smoothing histogram'
  
  ;; The data themselves are now discretized using the sampling intervals
  ;; delta_x.  We want to discretize the kernel with the same
  ;; sampling intervals, then convolve the two to smooth.
    
  ; First, make a kernel array where each element is the distance from
  ; the center in array index units.
  kernel = shift( dist(kernel_dim, 1), kernel_dim/2)
  
  ; Now, scale these distances to the kernel coordinate system in which the
  ; kernel support is [-1,1].
  kernel = kernel * 2.0 / d.Epanechnikov_width
  
  ; Now, compute the kernel values, normalizing so that the sum is 1.0
  ; Kernel is zero for distances greater than 1.0
  kernel = (0.75) * (1 - kernel^2)
  kernel = kernel / total(kernel)

  ; Now, convolve with histogram.  
  ; With the histogram padded by convolution_margin, the /EDGE_WRAP below should produce the same result as /EDGE_ZERO.
  ; I don't recall why I didn't use /EDGE_ZERO for extra protection from wrapping, but I hesitate to change the code now (2012).
  *d.samples= convol( float(*d.samples), kernel, /EDGE_WRAP )
  *d.error  = 0
  end


 2: $
  begin
  ;; GAUSSIAN SMOOTHING
  d.bandwidth_x = bin.delta_x * d.Gaussian_sigma 

  d.subtitle = strcompress( string( d.bandwidth_x,  $
  	f='("Gaussian kernel bandwidth (1-sigma): ",G10.4)' ) )

  widget_control, (*st).msg_label, SET_VALUE='smoothing histogram'
  
  ;; The data themselves are now discretized using the sampling intervals
  ;; delta_x & delta_y.  We want to discretize the kernel with the same
  ;; sampling intervals, then convolve the two to smooth.
    
  ; First, make a kernel array where each element is the distance from
  ; the center in array index units.
  kernel = shift( dist(kernel_dim, 1), kernel_dim/2)

  ; Normalize those distances by the bandwidth parameter..
  kernel = kernel / d.Gaussian_sigma
      
  ; Now, compute the kernel values, normalizing so that the sum is 1.0
  kernel = exp( (kernel^2) / (-2.0) )
  kernel = kernel / total(kernel)

  ; Now, convolve with histogram.  
  ; With the histogram padded by convolution_margin, the /EDGE_WRAP below should produce the same result as /EDGE_ZERO.
  ; I don't recall why I didn't use /EDGE_ZERO for extra protection from wrapping, but I hesitate to change the code now (2012).
  *d.samples= convol( float(*d.samples), kernel, /EDGE_WRAP )
  *d.error  = 0
  end
endcase


;; Normalize the density and errors by either the binsize or by  
;; (binsize * num_data_points) to produce either a density of datapoints
;; (# per X-unit) or a probability density.
;; In either case, configure the density_wcs object so that it shows 
;; the actual number of datapoints in each bin.

case (*st).normalize_density of
  0: begin ;counts
     norm = 1.0
     end
     
  1: begin ;# per X-unit
     norm       = bin.delta_x
     *d.samples = *d.samples / norm
     *d.error   = *d.error   / norm
     end

  2: begin ;PDF
     norm       = (bin.delta_x * dataset.num_data_points)
     *d.samples = *d.samples / norm
     *d.error   = *d.error   / norm
     end
endcase

wcs_object, wcs, /INIT, CTYP='counts', CDLT=norm
(*st).density_wcs = wcs

;; If necessary, normalize density and errors by the supplied normalization
;; function.  Watch out for zeros in normalization.
if ptr_valid( dataset.norm_absc ) then begin
  bin_center = d.x0 + bin.delta_x * findgen(d.xdim)
  
  norm = interpol( *dataset.norm_val, *dataset.norm_absc,  bin_center )
  
  index = where( norm LE 0, count )
  if (count GT 0) then norm[index] = 1.0
   
  *d.samples = *d.samples / norm
  *d.error   = *d.error   / norm
  
  if (count GT 0) then begin
    (*d.samples)[index] = 0.0
    (*d.error  )[index] = 0.0
  endif
  
  print, count, f='("Normalizing density; ",I0," bins are infinite.")' 
  wcs_object, wcs, /INIT
  
  (*st).density_wcs = wcs
endif
	  
dataset.density = d

(*st).stale_titles = 1
return
end

;==========================================================================
;;; Routine to estimate a distribution function, which is just the integral
;;; of the density function.
;;; The structure 'dataset' is modified!
;==========================================================================
PRO ComputeDataset1dDistribution, dataset, st

;; Make sure the density has been computed.
ComputeDataset1dDensity, dataset, st

d     = dataset.density
distn = dataset.distribution

;; First, see if the distribution has already been calculated.
if (distn.stale EQ 0) then return

distn.stale = 0

; Integrate distribution & normalize so last value is 1.0.
integral = total( *d.samples, /CUMULATIVE )
*distn.samples = temporary(integral / integral[n_elements(integral)-1])

dataset.distribution = distn

return
end


;==========================================================================
;;; Routine to align multiple densities and extract overlapping region.
;==========================================================================
PRO AlignDatasets1d, st, datasets, DISTRIBUTIONS=distributions

num_datasets = n_elements(datasets)

;; Apply the ROI and compute the densities using a common bin size, but 
;; individual x_min values.
desired_delta_x = max( datasets.binning.delta_x )
for ii = 0, num_datasets-1 do begin 
  ds = datasets[ii]
  ApplyDataset1dRoi,       ds, st

  if ((ds.binning.delta_x NE desired_delta_x)) then begin
    ds.binning.desired_delta_x = desired_delta_x
    ds.density.stale           = 1
  endif
  
  ;; We always call Compute... because a density may have been stale when
  ;; we started this routine.
  ComputeDataset1dDensity, ds, st
  datasets[ii] = ds
  
  if (ds.binning.delta_x NE desired_delta_x) then message, 'internal error'
endfor


;; Now we choose a common value for x_min and recompute densities, saving
;; the results back in the global data structure.
desired_x_min = min( datasets.density.x_min )

;; If necessary, recompute the densities on a common grid.
for ii = 0, num_datasets-1 do begin 
  ds = datasets[ii]

  if ((ds.density.x_min NE desired_x_min)) then begin
    ds.density.stale   = 1
    ComputeDataset1dDensity, ds, st, DESIRED_X_MIN=desired_x_min
    datasets[ii] = ds
  endif
  
  if keyword_set(distributions) then ComputeDataset1dDistribution, ds, st
  Save1dDataset, ds, st
endfor
     
return
end


;==========================================================================
;;; Routine to update the widget's appearance
;==========================================================================
PRO RedrawDataset1d, st

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
ApplyDataset1dRoi, selected_dataset, st
Save1dDataset, selected_dataset, st


;--------------------------------------------------------------------------
;; Filter all the datasets we're going to display -- this calculates 
;; informationwhich may be needed to set axis ranges.
;; Compute the densities needed -- this computes the subtitle
;; we may need to assign.
;--------------------------------------------------------------------------
;; Apply the ROI to all the datasets that will be shown and compute densities,
;; saving the modified structures back to (*st).
shown = where( ((*st).datasets.name NE '') AND $
  	       ((*st).datasets.hidden_flag EQ 0), count )

if (count EQ 0) then shown = [selected_index]
  
for ii = 0, n_elements(shown)-1 do begin
    dataset = (*st).datasets[ shown[ii] ]
    ApplyDataset1dRoi,       dataset, st
    if (mode GT 0) then ComputeDataset1dDensity,      dataset, st
    if (mode EQ 2) then ComputeDataset1dDistribution, dataset, st
    Save1dDataset, dataset, st
endfor
    
plotsets = ((*st).datasets)[ shown ]
    

;--------------------------------------------------------------------------
;; Assign default axis ranges if necessary.
;--------------------------------------------------------------------------
plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis

set_xrange=0
set_yrange=0			  
if (xaxis.default_flag) then begin
    margin = max( plotsets.binning.delta_x )
    set_xrange = [min( plotsets.x_min )-margin, max( plotsets.x_max )+margin]
endif

if (yaxis.default_flag) then begin
 case mode of
  0: set_yrange = [0, max( plotsets.num_data_points)]

  1: begin
     max_y = 0.0
     for ii = 0, n_elements(plotsets)-1 do begin
       max_y = max( [max_y, max( *plotsets[ii].density.samples )] )
     endfor
     set_yrange = [0, max_y]
     end
     
  2: begin
     max_y = 0.0
     for ii = 0, n_elements(plotsets)-1 do begin
       max_y = max( [max_y, max( *plotsets[ii].distribution.samples )] )
     endfor
     set_yrange = [0, max_y]
     end
  endcase
endif

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
    wcs_object, wcs, /INIT
    if (n_elements(plotsets) EQ 1) then title=plotsets[0].description $
				   else title=''
    plot_window, (*st).pw_id, TITLE=title, SUBTITLE='', $
    			      YWCS=wcs, YTITLE='Datapoint Index'
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
    
    point_count = dataset.num_data_points 
    if (point_count GT 0) then begin
      oplot, *dataset.x_filtered_data, lindgen(point_count), $
      	     PSYM=p.psym, COLOR=plot_color
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
      
        ; Coyote Graphics requires color indexes to be INT, not LONG.
        al_legend, plotsets.description, PSYM=plotsets.plot.psym,  $
                /TOP,LEFT=(lstyle EQ 1),CEN=(lstyle EQ 2),RIGHT=(lstyle EQ 3),$
                COLORS=fix(colors), TEXTCOLORS=fix(colors), CHARSIZE=0.75*!P.CHARSIZE
  endif
  end
  
 ;-------------------------------------------------------------------------
 1: $ DENSITY FUNCTION
 ;-------------------------------------------------------------------------
  begin
  ; Draw the plot axes.
  if ((*st).stale_titles) then begin
    case 1 of
     ((*st).density_title NE ''): title = (*st).density_title 
     (n_elements(plotsets) EQ 1): title = plotsets[0].description
     else:			  title = ''
    endcase
    
    if (n_elements(plotsets) EQ 1) then subtit=plotsets[0].density.subtitle $
				   else subtit=''
		   
    case (*st).normalize_density of
     0: ytitle = '#'
     1: ytitle = 'Density (# per X-unit)'
     2: ytitle = 'Probability Density'
    endcase
    
    plot_window, (*st).pw_id, TITLE=title, SUBTITLE=subtit, $
    		 YWCS=(*st).density_wcs, YTITLE=ytitle
  endif

  plot_window, (*st).pw_id, /SHOW_AXES
  
  ;------------------------------------------------------------------------
  ; Plot each of the datasets.
  ;------------------------------------------------------------------------
  for ii = 0, n_elements(plotsets)-1 do begin
    ;; Density plot
    p   = plotsets[ii].plot
    bin = plotsets[ii].binning
    d   = plotsets[ii].density
    color_manager, p.color, plot_color
    if keyword_set(p.plotsym) then begin
      cmd = 'plotsym, ' + p.plotsym
      if NOT execute(cmd) then message, 'call to plotsym.pro failed', /CONTINUE
    endif
    
    ;; We need zero bins on both ends for the plot to look right.
    bin_center = (d.x0-bin.delta_x) + bin.delta_x * findgen(2+d.xdim)
    bin_value  = [0.,*d.samples,0.]
          
    if (p.show_errors AND n_elements(*d.error) GT 1) then begin
      error = [0.,*d.error,0.]
      oploterror, bin_center, bin_value, error, PSYM=p.psym, LINESTYLE=p.line, ERRSTYLE=0, COLOR=plot_color, ERRCOLOR=plot_color, NSKIP=p.error_skip, /NOHAT
    endif else begin
      oplot, bin_center, bin_value, PSYM=p.psym, LINESTYLE=p.line, COLOR=plot_color
    endelse
    
    ;; Plot the fit if present.
    if (plotsets[ii].fit.stale EQ 0) then $
      oplot, bin_center[1:*], *plotsets[ii].fit.samples, $
      	     LINE=p.line, COLOR=plot_color
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
        
        ; Coyote Graphics requires color indexes to be INT, not LONG.
        al_legend, plotsets.description, LINE=plotsets.plot.line,  $
                /TOP,LEFT=(lstyle EQ 1),CEN=(lstyle EQ 2),RIGHT=(lstyle EQ 3),$
                COLORS=fix(colors), TEXTCOLORS=fix(colors), CHARSIZE=0.75*!P.CHARSIZE
  endif
  end

 ;-------------------------------------------------------------------------
 2: $ DISTRIBUTION FUNCTION
 ;-------------------------------------------------------------------------
  begin
  ; Draw the plot axes.
  if ((*st).stale_titles) then begin
    case 1 of
     ((*st).distribution_title NE ''): title = (*st).distribution_title 
     (n_elements(plotsets) EQ 1):      title = plotsets[0].description
     else:			       title = ''
    endcase
    
    if (n_elements(plotsets) EQ 1) then subtit=plotsets[0].density.subtitle $
				   else subtit=''

    ytitle = 'Cumulative Distribution'
        
    plot_window, (*st).pw_id, TITLE=title, SUBTITLE=subtit, $
    		 YWCS=(*st).distribution_wcs, YTITLE=ytitle
  endif

  plot_window, (*st).pw_id, /SHOW_AXES
  
  ;------------------------------------------------------------------------
  ; Plot each of the datasets.
  ;------------------------------------------------------------------------
  for ii = 0, n_elements(plotsets)-1 do begin
    ;; Distribution plot
    p     = plotsets[ii].plot
    bin   = plotsets[ii].binning
    d     = plotsets[ii].density
    distn = plotsets[ii].distribution
    color_manager, p.color, plot_color
    
    bin_center = d.x0 + bin.delta_x * findgen(d.xdim)
    
    ;; We need to connect the first point to X-axis for the plot to look right.
    oplot,  [d.x0, bin_center], [0., *distn.samples], LINE=p.line, COLOR=plot_color

    ;; Integrate and plot the fit if present.
    if (plotsets[ii].fit.stale EQ 0) then begin
      model_dist = total( *plotsets[ii].fit.samples * bin.delta_x, /CUMULATIVE )

      oplot, bin_center, model_dist, LINE=p.line, COLOR=plot_color
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
      
        ; Coyote Graphics requires color indexes to be INT, not LONG.
        al_legend, plotsets.description, LINE=plotsets.plot.line,  $
                /TOP,LEFT=(lstyle EQ 1),CEN=(lstyle EQ 2),RIGHT=(lstyle EQ 3),$
                COLORS=fix(colors), TEXTCOLORS=fix(colors), CHARSIZE=0.75*!P.CHARSIZE
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
  
    xl=(*st).roi_params.xl   &  xh=(*st).roi_params.xh
    if (!Y.TYPE EQ 0) then begin
      yl=!Y.CRANGE(0)    &  yh=!Y.CRANGE(1)
    endif else begin
      yl=10^!Y.CRANGE(0) &  yh=10^!Y.CRANGE(1)
    endelse
    
    plots, [xl,xl], [yl,yh], /DATA, THICK=2, COLOR=blue
    plots, [xh,xh], [yl,yh], /DATA, THICK=2, COLOR=blue 
endif ; (show_roi EQ 1)


plot_window, (*st).pw_id, /SHOW_MARKERS, XAXIS=xaxis, YAXIS=yaxis

;; Finally, show the dataset stats in the message field.
widget_control, (*st).msg_label, SET_VALUE=selected_dataset.stats
return
end


;==========================================================================
;;; Event Handler Function
;==========================================================================
FUNCTION Dataset1dEventFn, Event

widget_control, /HOURGLASS
new_event   = 0
DestroyFlag = 0

;; Get the state structure. 
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st

selected_index   = (where( (*st).datasets.name EQ (*st).selected_name))(0)
selected_dataset = (*st).datasets[ selected_index ]

roi_mode  = widget_info( (*st).roi_mode, /DROPLIST_SELECT )

;; Process the event.
case Event.ID of

;--------------------------------------------------------------------------
; Plot window mouse events.
  (*st).pw_id: $
   begin
   if (Event.middle_button AND (selected_dataset.hidden_flag EQ 0) AND $
       (selected_dataset.num_data_points GT 0)) then begin
     mode = widget_info( (*st).mode_list, /DROPLIST_SELECT )
     d   = selected_dataset.density
     bin = selected_dataset.binning
     
     plot_window, (*st).pw_id, DRAW_WIDGET=draw_widget
       
     ; We need to locate the datapoint closest to the click.
     ; Find distance from click to each data point in the device coor system.
     coordsystem_manager, draw_widget, /RESTORE
     click = convert_coord( event.button_coord, /DATA, /TO_DEVICE )
     case (mode) of
      0: $
       begin
       x_data = *selected_dataset.x_filtered_data
       y_data = lindgen(selected_dataset.num_data_points)
       end
      1: $
       begin
       x_data = d.x0 + bin.delta_x * findgen(d.xdim)
       y_data = *d.samples
       end
      2: $
       begin
       x_data = d.x0 + bin.delta_x * findgen(d.xdim)
       y_data = *selected_dataset.distribution.samples
       end
     endcase 
            
     xy_dev = convert_coord( x_data, y_data, /DATA, /TO_DEVICE )
       
     distances = (xy_dev(0,*) - click[0])^2 + (xy_dev(1,*) - click[1])^2
       		 
     minDist = min(distances,Imin)
     x_pt    = x_data[Imin]
     y_pt    = y_data[Imin]
       
     case (mode) of
      0: $
       begin
       msg = string(imin,x_pt,f='("Datapoint #",I0," is ",G11.5)')
       end
      1: $
       begin
       msg = string( x_pt, y_pt, $
                     f='("Density sample at ",G11.5," = ",G11.5)')
       end
      2: $
       begin
       msg = string( x_pt, y_pt, $
                     f='("Distribution sample at ",G11.5," = ",G11.5)')
       end
     endcase 
            
     tvcrs, x_pt, y_pt, /DATA
     widget_control, (*st).msg_label, SET_VALUE=strcompress(msg)
   endif ;(Event.middle_button)
   
   if (event.redraw) then RedrawDataset1d, st
   end


;--------------------------------------------------------------------------
; Dataset droplist
  (*st).dataset_list: $
   begin
   indexes          = where( (*st).datasets.name NE '' )
   names            = (*st).datasets[indexes].name
   (*st).selected_name = names[ event.index ]

   (*st).stale_titles = 1
   RedrawDataset1d, st
   end


;--------------------------------------------------------------------------
; Mode droplist
  (*st).mode_list: $
   begin
   (*st).stale_titles = 1
   RedrawDataset1d, st
   end


;--------------------------------------------------------------------------
; Derived menu
  (*st).derived_menu: $
   begin
   case Event.value of
    ;----------------------------------------------------------------------
    1: $ ;Define Model
     begin 
     RedrawDataset1d, st

     if (widget_info((*st).fit_setup, /VALID_ID) EQ 0) then begin
     
     bleft = '1, BASE,, COLUMN, FRAME'     
     
     b0 = '1, BASE,, ROW'
        
     f1 = '0,BUTTON,MOUSE,TAG=mouse'

     f2 = '2, DROPLIST, 1|2|3|4|5|6|7|8|9|10|11|12, SET_VALUE=0,' + $
          'LABEL_LEFT=GAUSSIAN COMPONENT:, TAG=gauss_index'
          
     form = [bleft,b0,f1,f2]
     
     
     b0 = '1, BASE,, ROW'
     
     f1 = '0, FLOAT,' + string((*st).gauss[0].amplitude) + $
	 ', TAG=amplitude, LABEL_LEFT=amplitude, WIDTH=8'
   
     f2 = '2, DROPLIST, free|fixed, SET_VALUE=' + $
          string((*st).gauss[0].freeze_amp)+ ', TAG=freeze_amp'
 
     form = [form,b0,f1,f2]
     
     
     b0 = '1, BASE,, ROW'
     
     f1 = '0, FLOAT,' + string((*st).gauss[0].mean) + $
	 ', TAG=mean, LABEL_LEFT=     mean, WIDTH=8'
   
     f2 = '2, DROPLIST, free|fixed, SET_VALUE=' + $
          string((*st).gauss[0].freeze_mean)+ ', TAG=freeze_mean'
 
     form = [form,b0,f1,f2]
     

     b0 = '1, BASE,, ROW'
     
     f1 = '0, FLOAT,' + string((*st).gauss[0].sigma) + $
	 ', TAG=sigma, LABEL_LEFT=    sigma, WIDTH=8'
   
     f2 = '2, DROPLIST, free|fixed, SET_VALUE=' + $
          string((*st).gauss[0].freeze_sigma)+ ', TAG=freeze_sigma'
 
     form = [form,b0,f1,f2,'2,BASE']
     


     bright = '1, BASE,, COLUMN, FRAME'
     
     f2 = '0, DROPLIST, 1|2|3|4|5|6|7|8|9|10|11|12, SET_VALUE=0,' + $
          'LABEL_LEFT=POLYNOMIAL COMPONENT:, TAG=poly_index'
          
     form = [form,bright,f2]
     
          
     b0 = '1, BASE,, ROW'
     
     f1 = '0, FLOAT,' + string((*st).poly[0].coeff) + $
	 ', TAG=coeff, LABEL_LEFT=coefficient, WIDTH=8'
   
     f2 = '2, DROPLIST, free|fixed, SET_VALUE=' + $
          string((*st).poly[0].freeze_coeff)+ ', TAG=freeze_coeff'
 
     form = [form,b0,f1,f2]
     
     
     b0 = '1, BASE,, ROW'

     f1 = '0, FLOAT,' + string((*st).poly[0].exponent) + $
	  ', TAG=exponent, LABEL_LEFT=     exponent, WIDTH=5'
     
     l0 = '2, LABEL, (fixed)'

     form = [form,b0,f1,l0]
     

     b0 = '1, BASE,, ROW'
     
     f1 = '0, FLOAT,' + string((*st).poly[0].low) + $
	 ', TAG=low, LABEL_LEFT=range, WIDTH=12'
    
     f2 = '2, FLOAT,' + string((*st).poly[0].high) + $
	 ', TAG=high, WIDTH=12'
    
     form = [form,b0,f1,f2,'2,BASE']
     f3 = '0,BUTTON,Hide,QUIT,TAG=hide'
          
     widget_control, top_base, UPDATE=0
     (*st).fit_setup = cw_form(top_base, [form,f3] )
     widget_control, top_base, UPDATE=1
     endif
     
     if ((*st).normalize_density NE 1) then begin
      msg_id = -1L
      TimedMessage, msg_id, "Under EDIT, select density normalization '# per X-unit' to enable fitting.", GROUP=top_base, LIFE=15, POSITIONING_PARENT=(*st).fit_setup
     endif
     
     end
     
    ;----------------------------------------------------------------------
    4: $ ;Export Fit Function
     begin 
     if (selected_dataset.fit.stale EQ 0) then begin
       plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis
       d = selected_dataset.density
       bin_center = d.x0 + selected_dataset.binning.delta_x * findgen(d.xdim)
       
       id = (*st).density_widget
       function_1d, id, bin_center, *selected_dataset.fit.samples, $
      		  WIDGET_TITLE='Density Function', GROUP=top_base,$
      		  XTITLE=xaxis.title, YTITLE=yaxis.title, $
      		  DATASET_NAME=selected_dataset.fit.name, $
      		  LINE=selected_dataset.plot.line, $
      		  COLOR=selected_dataset.plot.color
       (*st).density_widget = id
     endif
     end

    ;----------------------------------------------------------------------
    5: $ ;Export Density Function
     begin 
     ;; Make sure the density is computed, then save the dataset 
     ;; structure, which may have been modified.
     ComputeDataset1dDensity, selected_dataset, st
     Save1dDataset, selected_dataset, st
     
     d = selected_dataset.density
     bin_center = d.x0 + selected_dataset.binning.delta_x * findgen(d.xdim)

     plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis
     id = (*st).density_widget
     if (n_elements(*d.error) GT 1) then begin
       function_1d, id, bin_center, *d.samples, ERR=*d.error,$
      		    WIDGET_TITLE='Density Function', GROUP=top_base,$
      		    XTITLE=xaxis.title, YTITLE=yaxis.title,$
       		    PSYM=10, LINE=selected_dataset.plot.line, $
       		    COLOR=selected_dataset.plot.color, $
       		    DATASET_NAME=selected_dataset.name, $
      		    DESCRIPTION=selected_dataset.description
     endif else begin
       function_1d, id, bin_center, *d.samples, $
      		    WIDGET_TITLE='Density Function', GROUP=top_base,$
      		    XTITLE=xaxis.title, YTITLE=yaxis.title,$
       		    PSYM=10, LINE=selected_dataset.plot.line, $
       		    COLOR=selected_dataset.plot.color, $
       		    DATASET_NAME=selected_dataset.name, $
      		    DESCRIPTION=selected_dataset.description
     endelse
     (*st).density_widget = id
     end

    ;----------------------------------------------------------------------
    6: $ ;Density Function Samples
     begin 
     ;; Make sure the density is computed, then save the dataset 
     ;; structure, which may have been modified.
     ComputeDataset1dDensity, selected_dataset, st
     Save1dDataset, selected_dataset, st
   
     d=selected_dataset.density
   
     id = (*st).samples_widget
     dataset_1d, id, *d.samples, GROUP=top_base,  $
     		 WIDGET_TITLE='1-D Density Function Samples',$
     		 DATASET_NAME=selected_dataset.name,$
		 XTITLE='1-D Density Function Value'
     (*st).samples_widget = id
		 
     txt=['The 1-D array of density estimates you have computed',$
          '(a regular histogram or smoothed histogram) have been formed',$
          'into a 1-D dataset and sent to a ''dataset_1d'' tool for analysis.']
     TimedMessage, msg_id, txt, GROUP=top_base, LIFE=15
     end

    ;----------------------------------------------------------------------
    else: $ ; Perform Fit, either on selected dataset or all of them.
     begin 
     ;; Collect the dataset(s) we're going to fit.
     if (event.value EQ 3) then begin
       ;; For all the datasets not hidden, apply the ROI & compute the 
       ;; distribution saving the modified structures back to (*st).
       shown = where( ((*st).datasets.name NE '') AND $
         	      ((*st).datasets.hidden_flag EQ 0), count )
       
       if (count EQ 0) then shown = [selected_index]
         
       for ii = 0, n_elements(shown)-1 do begin
         dataset = (*st).datasets[ shown[ii] ]
         ApplyDataset1dRoi,            dataset, st
         ComputeDataset1dDistribution, dataset, st
         Save1dDataset, dataset, st
       endfor
    
       datasets_to_fit = ((*st).datasets)[ shown ]
         
     endif else begin
       ; Make sure the distribution function is available.
       ApplyDataset1dRoi,            selected_dataset, st
       ComputeDataset1dDistribution, selected_dataset, st
       Save1dDataset, selected_dataset, st
       
       datasets_to_fit = [selected_dataset]
     endelse
     

     ;; Construct a parameter structure to pass to ml_gaussian().
     gindex = where( ((*st).gauss.amplitude  NE 0) OR $
       		     ((*st).gauss.freeze_amp NE 1), gcount )
       
     pindex = where( ((*st).poly.coeff        NE 0) OR $
       		     ((*st).poly.freeze_coeff NE 1), pcount )
       
     if (gcount EQ 0 AND pcount EQ 0) then begin
       report = ['', '---- YOU MUST HAVE AT LEAST ONE COMPONENT WHOSE',$
       		     'AMPLITUDE IS NON-ZERO! ----']
       TimedMessage, msg_id, report, TIT=title, GR=top_base, POS=top_base 
       print, report
       return, 0
     endif
     
     case 1 of
        (gcount GT 0 AND pcount EQ 0): $
         begin
         model = string(gcount, F='("MODEL: ",I0," gaussians")')
         par   = {gauss: (*st).gauss[gindex]}
         end
         
        (gcount EQ 0 AND pcount GT 0): $
         begin
         model = string(pcount, F='("MODEL: polynomial with ",I0," terms")')
         par   = {poly:  (*st).poly [pindex]}
         end
         
        (gcount GT 0 AND pcount GT 0): $
         begin
         fmt='("MODEL: ",I0," gaussians + polynomial with ",I0," terms")'
         model = string(gcount, pcount, FORMAT=fmt)
         par   = {gauss: (*st).gauss[gindex], $
         	         poly: (*st).poly [pindex]}
         end
     endcase
              
     prompt=[model,'',$
	       'The univariate data are being fit by a direct application',$
       	       'of the Maximum Likelihood Method (Chapter 10 of Bevington).',$
       	       'The IDL terminal window shows the results after each',$
       	       'iteration of the fit.','',$
       	       'If you want to perform a least-squares fit to a DENSITY',$
       	       'FUNCTION, then you need to export the density function to',$
       	       'a function_1d tool and fit it there.']
     
     ;; Fit each dataset to the model.
     n_datasets = n_elements(datasets_to_fit)
     results = replicate( par, n_datasets )
     
     for ii = 0, n_datasets-1 do begin
       dataset = datasets_to_fit[ii]
       title  = 'ERROR'
       if (dataset.num_data_points LE 0) then begin
         report = ['', '---- Dataset '+dataset.name+' has no data. ----']
       endif else begin
         d = dataset.density
         distribution = *dataset.distribution.samples
         
         lkh = ml_gaussian( PARAMETERS=par, PROMPT=prompt,$
                      			  DATA_PTR=dataset.x_filtered_data, $
                      			  X0=d.x0, DELTA_X=selected_dataset.binning.delta_x, $
                      			  DISTRIBUTION=distribution,$
                      			  REPORT=report, F_OF_X=*dataset.fit.samples, $
                      			  NUMFREE=numfree )

         results[ii] = par
         if (lkh NE 0) then begin
           case (*st).normalize_density of
             0: *dataset.fit.samples = *dataset.fit.samples * dataset.binning.delta_x
             1: 
             2: *dataset.fit.samples = *dataset.fit.samples / dataset.num_data_points           
           endcase
           
           dataset.fit.stale = 0
           dataset.fit.name  = string(dataset.fit.number, dataset.name,$
         			      F='("fit ",I0," to ",A)')
           dataset.fit.number= dataset.fit.number + 1
           Save1dDataset, dataset, st
         
           f1='("Fit (",I0," free parameters) to dataset {",A0,"}")'
           f2='("over the data range [",G10.4,", ",G10.4,"]")'
           txt=[string(numfree, dataset.name, FORMAT=f1), $
                string(dataset.x_min,dataset.x_max, FORMAT=f2)]
           report = [strcompress(txt),report]
           title  = dataset.fit.name
         endif ; (lkh NE 0)
       endelse ; num_data_points > 0
       
       TimedMessage, msg_id, report, TIT=title, GR=top_base, POS=top_base 
       msg_id = -1L
       tara_clipboard, POST=report
     endfor ;ii
     RedrawDataset1d, st

     if (n_datasets EQ 1) && widget_info((*st).fit_setup, /VALID_ID) then begin
       ; Write the fit parameters back into the GUI.
       case 1 of
        (gcount GT 0 AND pcount EQ 0): (*st).gauss[gindex] = par.gauss
         
        (gcount EQ 0 AND pcount GT 0): (*st).poly [pindex] = par.poly
         
        (gcount GT 0 AND pcount GT 0): $
         begin
         (*st).gauss[gindex] = par.gauss
         (*st).poly [pindex] = par.poly
         end
       endcase
       event = {ID:(*st).fit_setup, TOP:top_base, HANDLER:top_base, TAG:'REDRAW'}
       dum = Dataset1dEventFn(event)
       ;widget_control, top_base, SEND_EVENT=event
     endif ;(n_datasets EQ 1) 
              
     
     ;; Now, if we fit multiple datasets, plot the parameters.
     if (n_datasets GT 1) then begin
       function_1d, id, WIDGET_TITLE='Model Parameters',GROUP=top_base,$
			XTITLE='trend property value', YTITLE='parameter value'
       x = datasets_to_fit.metric
       
       if (gcount GT 0) then begin
         gauss = results.gauss
         if (size(gauss, /N_DIM) EQ 1) then begin
             function_1d, id, x, gauss.amplitude, DATASET_NAME="amplitude"
             function_1d, id, x, gauss.mean,      DATASET_NAME="mean"
             function_1d, id, x, gauss.sigma,     DATASET_NAME="sigma"
         endif else begin
           n_components = (size(gauss, /DIM))[0]
         			
           for ii=0,n_components-1 do begin
             function_1d, id, x, reform((gauss.amplitude)[ii,*]), $
             	DATASET_NAME=string(ii,F='("amplitude",I0)')
             function_1d, id, x, reform((gauss.mean)[ii,*]), $
             	DATASET_NAME=string(ii,F='("mean",I0)')
             function_1d, id, x, reform((gauss.sigma)[ii,*]), $
             	DATASET_NAME=string(ii,F='("sigma",I0)')
           endfor
         endelse
       endif ;(gcount GT 0)

       if (pcount GT 0) then begin
         poly = results.poly
         if (size(poly, /N_DIM) EQ 1) then begin
             function_1d, id, x, poly.coeff, DATASET_NAME="coeff"
	     endif else begin
           n_components = (size(poly, /DIM))[0]
         			
           for ii=0,n_components-1 do begin
             function_1d, id, x, reform((poly.coeff)[ii,*]), $
             	DATASET_NAME=string(ii,F='("coeff",I0)')
           endfor
         endelse
       endif ;(pcount GT 0) 
     endif ;(n_datasets GT 1)
     end ; Perform Fit (else part of case statement)
   endcase
  end

;--------------------------------------------------------------------------
; Fit setup form
  (*st).fit_setup: $
   begin
   widget_control, (*st).fit_setup, GET_VALUE=val
   redraw_form = 0
   case event.tag of
    'MOUSE': $
     begin
     ii = val.gauss_index
     prompt='Click on peak of gaussian'
     plot_window, (*st).pw_id, GET_MOUSE=peak, PROMPT=prompt
     
     prompt='Click on half-max point of gaussian; BEWARE OF LOG SCALING!!!'
     plot_window, (*st).pw_id, GET_MOUSE=half_max, PROMPT=prompt
     
     prompt='Click on wing of gaussian'
     plot_window, (*st).pw_id, GET_MOUSE=wing, PROMPT=prompt
     
     mean      = peak[0]
     sigma     = abs( mean - half_max[0] ) * 2.0 / 2.354
     amplitude = (peak[1]-wing[1]) * sigma * sqrt(2.0*!PI)
     
     if ((*st).gauss[ii].freeze_amp EQ 0) then $
         (*st).gauss[ii].amplitude = amplitude

     if ((*st).gauss[ii].freeze_mean EQ 0) then $
         (*st).gauss[ii].mean = mean

     if ((*st).gauss[ii].freeze_sigma EQ 0) then $
         (*st).gauss[ii].sigma = sigma
     redraw_form = 1
     end
    
    'GAUSS_INDEX': redraw_form = 1
    'POLY_INDEX' : redraw_form = 1
    'REDRAW'     : redraw_form = 1
    
    'HIDE': widget_control, (*st).fit_setup, /DESTROY
    
    else: $
     begin
     ii = val.gauss_index
     (*st).gauss[ii].amplitude    = val.amplitude
     (*st).gauss[ii].freeze_amp   = val.freeze_amp
     (*st).gauss[ii].mean         = val.mean
     (*st).gauss[ii].freeze_mean  = val.freeze_mean
     (*st).gauss[ii].sigma        = val.sigma
     (*st).gauss[ii].freeze_sigma = val.freeze_sigma
     
     ii = val.poly_index
     (*st).poly[ii].coeff        = val.coeff
     (*st).poly[ii].freeze_coeff = val.freeze_coeff
     (*st).poly[ii].exponent     = val.exponent
     (*st).poly[ii].low          = val.low
     (*st).poly[ii].high         = val.high
     end
   endcase
   
   if (redraw_form) then begin
     ;; Redraw the widgets.
     ii = val.gauss_index
     val.amplitude     = (*st).gauss[ii].amplitude  
     val.freeze_amp    = (*st).gauss[ii].freeze_amp 
     val.mean          = (*st).gauss[ii].mean        
     val.freeze_mean   = (*st).gauss[ii].freeze_mean 
     val.sigma         = (*st).gauss[ii].sigma        
     val.freeze_sigma  = (*st).gauss[ii].freeze_sigma 
     
     ii = val.poly_index
     val.coeff         = (*st).poly[ii].coeff        
     val.freeze_coeff  = (*st).poly[ii].freeze_coeff 
     val.exponent      = (*st).poly[ii].exponent        
     val.low           = (*st).poly[ii].low 
     val.high          = (*st).poly[ii].high        
     widget_control, (*st).fit_setup, SET_VALUE=val
   endif
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
       RedrawDataset1d, st
       device, /CLOSE
       color_manager,  /X_PSEUDO
  
       if (Filename EQ '') then begin
         print_file
         widget_control, (*st).msg_label, SET_VALUE='Printed plot'
       endif else begin
         widget_control, (*st).msg_label, $
         		 SET_VALUE='Wrote Postscript file ' + filename
       endelse
     endif 
     end
     
    2: $ ; SAVE SELECTED DENSITY
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
                  
         ;; Make sure the density is computed, then save the dataset 
         ;; structure, which may have been modified.
         ComputeDataset1dDensity, selected_dataset, st
         Save1dDataset, selected_dataset, st
           
         ; Make up a primary FITS header.
         get_date, date_today
         mkhdr, pheader, '', /EXTEND
         sxaddpar, pheader, "CREATOR", "$RCSfile$, Version $Revision: 4378 $"
	 sxaddpar, pheader, 'ORIGIN',   'Penn State University'
	 sxaddpar, pheader, "FITSVERS", "IDL Astronomy Users Library"
	 fdecomp, pathname, disk, item_path, item_name, item_qual
	 if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
	 sxaddpar, pheader, "FNFITS", item_name
	 sxaddpar, pheader, 'DATE',     date_today
	 writefits, pathname, no_data, pheader
	 
         sxaddpar, fits_header, "EXTNAME", 'DENSITY of '+selected_dataset.name
         sxaddpar, fits_header, "CREATOR", "$RCSfile$, Version $Revision: 4378 $"
	 sxaddpar, fits_header, 'ORIGIN',   'Penn State University'
	 sxaddpar, fits_header, 'DATE',     date_today

         bin = selected_dataset.binning
         d   = selected_dataset.density
         if (n_elements(*d.error) GT 1) then begin
           bin_table = replicate( {X:0.0, DENSITY:0.0, ERROR:0.0 }, d.xdim )
	 endif else begin
           bin_table = replicate( {X:0.0, DENSITY:0.0 }, d.xdim )
         endelse
	 
	 bin_table.X       = d.x0 + bin.delta_x * findgen(d.xdim)
         bin_table.DENSITY = *d.samples
         if (n_elements(*d.error) GT 1) then bin_table.ERROR = *d.error
         
         mwrfits, bin_table, pathname, fits_header            
	 msg='Wrote density to FITS file ' + pathname
         widget_control, (*st).msg_label, SET_VALUE=msg
       endelse ; (Error EQ 0)
     endif ; (pathname NE '')
     end


    3: $ ; SAVE ALL DENSITIES
     begin
     pathname = dialog_pickfile( GROUP=top_base, FILE='densities.fits', $
				 TITLE='Save All Densities (FITS)' )
     widget_control, /HOURGLASS
     
     if (pathname NE '') then begin
       ; Make sure we can write to this file.
       openw, Unit, pathname, /GET_LUN, ERROR=Error
       if (Error NE 0) then begin
         msg = 'ERROR opening file '+ pathname
         dummy=dialog_message(msg, DIALOG_PARENT=(*st).file_menu)
       endif else begin
         free_lun, Unit
         
         ;; Align all the non-hidden datasets.
         shown = where( ((*st).datasets.name NE '') AND $
         	        ((*st).datasets.hidden_flag EQ 0), count )
         
         if (count EQ 0) then shown = [selected_index]

         datasets = (*st).datasets[ shown ]
         AlignDatasets1d, st, datasets
           
         ;; Make up a primary FITS header.
         get_date, date_today
         mkhdr, pheader, '', /EXTEND
         sxaddpar, pheader, "CREATOR", "$RCSfile$, Version $Revision: 4378 $"
	 sxaddpar, pheader, 'ORIGIN',   'Penn State University'
	 sxaddpar, pheader, "FITSVERS", "IDL Astronomy Users Library"
	 fdecomp, pathname, disk, item_path, item_name, item_qual
	 if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
	 sxaddpar, pheader, "FNFITS", item_name
	 sxaddpar, pheader, 'DATE',     date_today
	 writefits, pathname, no_data, pheader
	 
         sxaddpar, fits_header, "EXTNAME", 'DENSITIES'
         sxaddpar, fits_header, "CREATOR", "$RCSfile$, Version $Revision: 4378 $"
	 sxaddpar, fits_header, 'ORIGIN',   'Penn State University'
	 sxaddpar, fits_header, 'DATE',     date_today

         ;; Build a structure to hold data.
         str = {X:0.0}
         for ii=0, n_elements(datasets)-1 do begin
           str = create_struct( str, 'Y_'+strcompress(datasets[ii].name,/REMOVE), 0.0 )
         endfor
         
         xdim      = max( datasets.density.xdim )
         bin_table = replicate( str, xdim )
         
         for ii=0, n_elements(datasets)-1 do begin
           d      = datasets[ii].density
           col    = fltarr(xdim)
           col[0] = *d.samples
           bin_table.(ii+1) = col
         endfor
	 
	 bin_table.X =  datasets[0].density.x0 + $
			datasets[0].binning.delta_x * findgen(xdim)
         
         ;; Write out table.
         mwrfits, bin_table, pathname, fits_header            
	 msg='Wrote densities to FITS file ' + pathname
         widget_control, (*st).msg_label, SET_VALUE=msg
         RedrawDataset1d, st
       endelse ; (Error EQ 0)
     endif ; (pathname NE '')
     end

     
    4: $ ; SAVE SELECTED DISTRIBUTION
     begin
     name = strcompress( selected_dataset.name + '.fits', /REMOVE_ALL )
     pathname = dialog_pickfile( GROUP=top_base, FILE=name(0), $
				 TITLE='Save Distribution (FITS)' )
     widget_control, /HOURGLASS
     
     if (pathname NE '') then begin
       ; Make sure we can write to this file.
       openw, Unit, pathname, /GET_LUN, ERROR=Error
       if (Error NE 0) then begin
         msg = 'ERROR opening file '+ pathname
         dummy=dialog_message(msg, DIALOG_PARENT=(*st).file_menu)
       endif else begin
         free_lun, Unit
                  
         ;; Make sure the Distribution is computed, then save the dataset 
         ;; structure, which may have been modified.
         ComputeDataset1dDensity,      selected_dataset, st
         ComputeDataset1dDistribution, selected_dataset, st
         Save1dDataset, selected_dataset, st
           
         ; Make up a primary FITS header.
         get_date, date_today
         mkhdr, pheader, '', /EXTEND
         sxaddpar, pheader, "CREATOR", "$RCSfile$, Version $Revision: 4378 $"
	 sxaddpar, pheader, 'ORIGIN',   'Penn State University'
	 sxaddpar, pheader, "FITSVERS", "IDL Astronomy Users Library"
	 fdecomp, pathname, disk, item_path, item_name, item_qual
	 if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
	 sxaddpar, pheader, "FNFITS", item_name
	 sxaddpar, pheader, 'DATE',     date_today
	 writefits, pathname, no_data, pheader
	 
         sxaddpar, fits_header, "EXTNAME", 'DISTRIBUTION'
         sxaddpar, fits_header, "CREATOR", "$RCSfile$, Version $Revision: 4378 $"
	 sxaddpar, fits_header, 'ORIGIN',   'Penn State University'
	 sxaddpar, fits_header, 'DATE',     date_today

         bin = selected_dataset.binning
         d   = selected_dataset.density
         bin_table = replicate( {X: 0.0, DISTBN: 0.0 }, d.xdim )
         		        
         bin_table.X       = d.x0 + bin.delta_x * findgen(d.xdim)
         bin_table.DISTBN  = *selected_dataset.distribution.samples
         
         mwrfits, bin_table, pathname, fits_header            
	 msg='Wrote distribution to FITS file ' + pathname
         widget_control, (*st).msg_label, SET_VALUE=msg
       endelse ; (Error EQ 0)
       
     endif ; (pathname NE '')
     end
     
    5: $ ; SAVE ALL DISTRIBUTIONS
     begin
     pathname = dialog_pickfile( GROUP=top_base, FILE='distributions.fits', $
				 TITLE='Save All Distributions (FITS)' )
     widget_control, /HOURGLASS
     
     if (pathname NE '') then begin
       ; Make sure we can write to this file.
       openw, Unit, pathname, /GET_LUN, ERROR=Error
       if (Error NE 0) then begin
         msg = 'ERROR opening file '+ pathname
         dummy=dialog_message(msg, DIALOG_PARENT=(*st).file_menu)
       endif else begin
         free_lun, Unit
         
         ;; Align all the non-hidden datasets.
         shown = where( ((*st).datasets.name NE '') AND $
         	        ((*st).datasets.hidden_flag EQ 0), count )
         
         if (count EQ 0) then shown = [selected_index]

         datasets = (*st).datasets[ shown ]
         AlignDatasets1d, st, datasets, /DISTRIBUTIONS
           
         ;; Make up a primary FITS header.
         get_date, date_today
         mkhdr, pheader, '', /EXTEND
         sxaddpar, pheader, "CREATOR", "$RCSfile$, Version $Revision: 4378 $"
	 sxaddpar, pheader, 'ORIGIN',   'Penn State University'
	 sxaddpar, pheader, "FITSVERS", "IDL Astronomy Users Library"
	 fdecomp, pathname, disk, item_path, item_name, item_qual
	 if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
	 sxaddpar, pheader, "FNFITS", item_name
	 sxaddpar, pheader, 'DATE',     date_today
	 writefits, pathname, no_data, pheader
	 
         sxaddpar, fits_header, "EXTNAME", 'DENSITIES'
         sxaddpar, fits_header, "CREATOR", "$RCSfile$, Version $Revision: 4378 $"
	 sxaddpar, fits_header, 'ORIGIN',   'Penn State University'
	 sxaddpar, fits_header, 'DATE',     date_today

         ;; Build a structure to hold data.
         str = {X:0.0}
         for ii=0, n_elements(datasets)-1 do begin
           str = create_struct( str, 'Y_'+strcompress(datasets[ii].name,/REMOVE), 0.0 )
         endfor
         
         xdim      = max( datasets.density.xdim )
         bin_table = replicate( str, xdim )
         
         for ii=0, n_elements(datasets)-1 do begin
           col    = replicate(max(*datasets[ii].distribution.samples), xdim)
           col[0] = *datasets[ii].distribution.samples
           bin_table.(ii+1) = col
         endfor
	 
	 bin_table.X =  datasets[0].density.x0 + $
			datasets[0].binning.delta_x * findgen(xdim)
         
         ;; Write out table.
         mwrfits, bin_table, pathname, fits_header            
	 msg='Wrote distributions to FITS file ' + pathname
         widget_control, (*st).msg_label, SET_VALUE=msg
         RedrawDataset1d, st
       endelse ; (Error EQ 0)
     endif ; (pathname NE '')
     end

     
    6: $ ; SAVE DATASET
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
         sxaddpar, pheader, "CREATOR", "$RCSfile$, Version $Revision: 4378 $"
	 sxaddpar, pheader, 'ORIGIN',   'Penn State University'
	 sxaddpar, pheader, "FITSVERS", "IDL Astronomy Users Library"
	 fdecomp, pathname, disk, item_path, item_name, item_qual
	 if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
	 sxaddpar, pheader, "FNFITS", item_name
	 sxaddpar, pheader, 'DATE',     date_today
	 writefits, pathname, no_data, pheader
	 
         sxaddpar, fits_header, "EXTNAME", selected_dataset.name
         sxaddpar, fits_header, "CREATOR", "$RCSfile$, Version $Revision: 4378 $"
	 sxaddpar, fits_header, 'ORIGIN',   'Penn State University'
	 sxaddpar, fits_header, 'DATE',     date_today
	 
	 plot_window, (*st).pw_id, XAXIS=xaxis, YAXIS=yaxis
	 sxaddpar, fits_header, 'XTITLE', xaxis.title  
	 

	 bin_table = replicate( {X: 0.0}, $
	   			  selected_dataset.num_data_points )

         bin_table.X        = *selected_dataset.x_filtered_data
         
         mwrfits, bin_table, pathname, fits_header            
	 msg='Wrote dataset to ' + pathname       
         widget_control, (*st).msg_label, SET_VALUE=msg
       endelse ; (Error EQ 0)
       
     endif ; (pathname NE '')
     end
          
    7: $ ; LOAD DATASET
     begin
     txt=['If you choose a FITS FILE, then its first extension must be',$
          'a binary table.  The first column of the table is assumed',$
          'to contain the univariate dataset.','',$
          'If you choose a ASCII FILE, then you will be presented with',$
          'the ASCII_TEMPLATE tool built into IDL.  Fill out the',$
   	  'three forms in this tool so that the first field extracted',$
   	  'contains the univariate dataset.']
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
       dataset_1d, top_base, table.(0), DATASET_NAME=item_name
     endif
     widget_control, msg_id, /DESTROY, BAD_ID=bad
     end

    8: $ ; DELETE DATASET
     begin
     ;; We do NOT let the user delete the last dataset.
     dum = where( (*st).datasets.name NE '', count )
     if ((count NE 1) AND (NOT selected_dataset.disable_delete)) then begin
       (*st).datasets[ selected_index ].name = ''
       
       ; Select the dataset just prior to the deleted one.
       (*st).selected_name = (*st).datasets[0 > (selected_index-1)].name
       (*st).stale_titles = 1
       RedrawDataset1d, st
     endif
     end

    9: $ ; HIDE/SHOW DATASETS
     begin
     ;; Find the non-null datasets and update the dataset_list droplist widget.
     index = where( (*st).datasets.name NE '', count )
     
     datasets = (*st).datasets[index]
     
     bgroup = string(datasets.name, F='("0,BUTTON,", 99(A,:,"|"))') + $
     	      ',TAG=bgroup'
     form = ['1,BASE,,COLUMN', bgroup, '2,BASE']

     ;; Run the form widget.
     title = '0,LABEL, Dataset Visibility'
     ok = '0, BUTTON, OK, QUIT'
 
     form_base = widget_base(TITLE='dataset_1d')
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
     for ii=0,count-1 do Save1dDataset, datasets[ii], st
          
     RedrawDataset1d, st
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
   f2 = '0, TEXT,' + escape_commas( str ) + $
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
   b0 = '1, BASE,, ROW, FRAME'

   l0 = '0, LABEL, ROI:'
   
   f2 = '0, DROPLIST, Hide ROI|Show ROI, SET_VALUE=' + $
   	string((*st).show_roi) + ', TAG=show_roi'
   	
   f3 = '0, FLOAT,' + string((*st).roi_params.xl) + $
	 ', TAG=xl, LABEL_LEFT=Min, WIDTH=8'
	 
   f4 = '2, FLOAT,' + string((*st).roi_params.xh) + $
	 ', TAG=xh, LABEL_LEFT=Max, WIDTH=8'

   form = [form,b0,l0,f2,f3,f4]
   
   
   mode = widget_info( (*st).mode_list, /DROPLIST_SELECT ) 

   if (mode NE 0) then begin
    b0 = '1, BASE,, COLUMN, FRAME'

    ; BINNING PROPERTIES
    b1 = '1, BASE,, COLUMN, FRAME'

    l0 = '0, LABEL, BINNING:, LEFT'

    f1a = '0, FLOAT,' + string(selected_dataset.binning.desired_delta_x) + $
	 ', TAG=desired_delta_x, LABEL_LEFT=Desired binsize: , WIDTH=8'

    l1 = '0, LABEL,Actual binsize:  ' + $
    	 string(selected_dataset.binning.delta_x)

    f2a = '2, FLOAT,' + string(selected_dataset.binning.xbin_location) + $
	 ', TAG=xbin_location, LABEL_LEFT=Location of any bin edge: ,WIDTH=8'
 
    form = [form,b0,b1,l0,f1a,l1,f2a]


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
   
    ; GLOBAL DENSITY PROPERTIES 
    b0 = '1, BASE,, ROW, FRAME'
          
    f1 = '2, DROPLIST, Counts|# per X-unit|PDF, SET_VALUE=' + $
    	  string((*st).normalize_density) + $
    	  ', LABEL_LEFT=Density Normalization:, TAG=normalize'
	  
    form = [form,b0,f1]     
   endif ;(mode NE 0)
   
   
   ;  PLOT PROPERTIES
    b0 = '1, BASE,, ROW, FRAME'
    
    l0 = '0, LABEL, PLOT PROPERTIES:'
    
    f1 = '0, BUTTON, ' + $
         '+ Symbol|Asterisk|Dot|Diamond|Triangle|Box|X Symbol|user-defined|Histogram,'+$
	 'EXCLUSIVE, SET_VALUE=' + string((selected_dataset.plot.psym - 1)<8) + $
	 ', TAG=psym, LABEL_TOP=Symbol Style'

    f1b= '0, TEXT,' + escape_commas( selected_dataset.plot.plotsym ) + $
	 ', TAG=plotsym, LABEL_TOP=plotsym params, WIDTH=15'

    f4 = '0, BUTTON, ' + $
         'Solid|Dotted|Dashed|Dash Dot|Dash Dot Dot|Long Dashes,'+$
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

    f6 = '2, INTEGER,' + string(selected_dataset.plot.error_skip) + $
	 ', TAG=error_skip, LABEL_LEFT=NSKIP, WIDTH=3'
	 
    f3 = '2, DROPLIST, Omit|Left|Center|Right, SET_VALUE=' + $
   	string((*st).legend_style) + ', LABEL_LEFT=Legend:, TAG=legend_style'
   	
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
  
  (*st).show_roi = r.show_roi
  rp = (*st).roi_params

  (*st).roi_params.xl = r.xl
  (*st).roi_params.xh = r.xh
  
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
    selected_dataset.binning.xbin_location   = r.xbin_location
    
    if ((compare_struct(b,selected_dataset.binning))[0].ndiff GT 0) then begin
      selected_dataset.density.stale  = 1B
      
      if (*st).share_binning_params then begin
        (*st).datasets.binning.desired_delta_x    = r.desired_delta_x
        (*st).datasets.binning.xbin_location      = r.xbin_location
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
    
     ; If normalization mode changed, ALL densities & distributions are stale.
     if ((*st).normalize_density NE r.normalize) then begin
       (*st).normalize_density             = r.normalize
       
       selected_dataset.density.stale      = 1B
       selected_dataset.distribution.stale = 1B
       (*st).datasets.density.stale        = 1B
       (*st).datasets.distribution.stale   = 1B
     endif
  endif ; (mode NE 0)
  
   r.psym = (r.psym EQ 8) ? 10 : r.psym+1
   
   selected_dataset.plot.psym = r.psym
   selected_dataset.plot.plotsym = r.plotsym
   selected_dataset.plot.line = r.line
      
   ; We have to translate "result.color", which is an index into
   ; the corresponding color name (string).
   selected_dataset.plot.color = color_names[r.color]

   (*st).legend_style = r.legend_style
   selected_dataset.plot.show_errors  = r.show_errors
   selected_dataset.plot.error_skip   = r.error_skip > 1
  
   ; Save the dataset structure we've modified.
   Save1dDataset, selected_dataset, st
   RedrawDataset1d, st
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
   RedrawDataset1d, st
   end


;--------------------------------------------------------------------------
; Filter Define button
  (*st).roi_define: $
   begin
   ; Use the plot_window's markers to define a ROI.
   plot_window, (*st).pw_id, BIG_MARKER=big_marker, SMALL_MARKER=small_marker

    (*st).roi_params.xl = big_marker[0] < small_marker[0]
    (*st).roi_params.xh = big_marker[0] > small_marker[0]
   
   ; If ROI is active then statistics for all datasets are now stale.
   if (roi_mode NE 0) then (*st).datasets.stats_stale  = 1B
   
   ; If ROI filtering is active then all filtered datasets are now stale.
   if (roi_mode EQ 2) then begin
     (*st).datasets.filter_stale = 1B
     new_event = { ID:top_base, TOP:event.top, HANDLER:0L}
   endif

   if (roi_mode NE 0) then RedrawDataset1d, st
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

   if (roi_mode NE 0) then RedrawDataset1d, st
   end


  else: print, 'unknown event in dataset_1d'
endcase

widget_control, (*st).fit_setup, SENSITIVE=((*st).normalize_density EQ 1), BAD_ID=bad_id

if DestroyFlag then widget_control, (*st).parent, /DESTROY

return, new_event
end



;==========================================================================
;;; Event Handler Procedure
;==========================================================================
PRO Dataset1dEvent, Event
 
event = Dataset1dEventFn( Event )
return
end


;==========================================================================
;;; MAIN "dataset_1d" ROUTINE
;==========================================================================

PRO dataset_1d, top_base, x_data, _EXTRA=extra, $
		 TITLE=title, $
		 XTITLE=xtitle, XWCS=xwcs, LEGEND_STYLE=legend_style, NAN_VALUES=nanval,$
		 DENSITY_TITLE=density_title, $
		 DISTRIBUTION_TITLE=distribution_title, $ 
		 NORMALIZE_DENSITY=normalize_density, $
		 NORM_ABSC=norm_absc, NORM_VAL=norm_val, $
		 BINSIZE=binsize, BIN_LOCATION=bin_location,$
		 DATASET_NAME=dataset_name, DESCRIPTION=description, $
		 METRIC=metric, COLOR=color, LINESTYLE=linestyle, PSYM=psym, PLOTSYM=plotsym, $
		 HIDE_DATASET=hide_dataset, DISABLE_DELETE=disable_delete, $
		 DELETE=delete, ROI_MASK=mask, REDRAW=redraw, DENSITY_WIDGET=density_widget, $ 
     PLOT_WINDOW_OPTIONS=plot_window_options, PS_CONFIG=ps_config, PRINT=print_now
		 
;; If the widget ID of an existing dataset_1d was not passed, then create 
;; the widget.
create_flag = 1
if (n_elements(top_base) EQ 1) then begin
  if (widget_info( top_base, /VALID_ID )) then create_flag = 0
endif

if (create_flag) then top_base = CreateDataset1d( _STRICT_EXTRA=extra )

;;---------------------------------------------------------------------------
;; Get the state structure.
widget_control, top_base, GET_UVALUE=st
redraw_flag = 0

;;---------------------------------------------------------------------------
;; Handle title keywords by passing on to plot_window.
if (0 NE n_elements(xwcs))     then plot_window, (*st).pw_id, XWCS=xwcs

param_name = (routine_names(x_data, ARG_NAME=(-1)))[0]

; On the first call use the parameter name (if available) to name the axis
; if XTITLE not supplied.
if create_flag then begin
  if (0 EQ n_elements(xtitle)) then xtitle = keyword_set(param_name) ? param_name : 'X'
endif

if (0 NE n_elements(xtitle))   then plot_window, (*st).pw_id, XTITLE=xtitle
if (0 NE n_elements(title))    then begin
  print, '!!!!!!!!!!!!!!!!!!!'
  print, 'Obsolete keyword TITLE used in dataset_1d call; use DENSITY_TITLE or DISTRIBUTION_TITLE'
  print, '!!!!!!!!!!!!!!!!!!!'
endif

if (0 NE n_elements(density_title))      then begin
  (*st).density_title=density_title
  (*st).stale_titles = 1
  widget_control, (*st).mode_list, SET_DROPLIST_SELECT=1
endif

if (0 NE n_elements(distribution_title)) then begin
  (*st).distribution_title=distribution_title
  (*st).stale_titles = 1
  widget_control, (*st).mode_list, SET_DROPLIST_SELECT=2
endif

if (0 NE n_elements(normalize_density)) then begin
  ; If normalization mode changed, ALL densities & distributions are stale.
  (*st).normalize_density = normalize_density
  
  (*st).datasets.density.stale        = 1B
  (*st).datasets.distribution.stale   = 1B
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
  dataset_name = keyword_set(param_name) ? param_name :'1D dataset'
endif else begin
  ; Neither a name nor data were passed.  Assume the caller is refering to the last dataset.
  ind = where( (*st).datasets.name NE '', count )
  
  dataset_name = (count EQ 0) ? '1D dataset' : ((*st).datasets.name)[ind[count-1]]
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
  ;; Store new X data values if supplied.

  if (N_ELEMENTS(x_data) NE 0) then begin

    is_finite = finite(x_data)

    good_ind = where(is_finite, num_data_points, $
				COMPLEMENT=null_ind, NCOMPLEMENT=num_null)
    dataset.num_data_points = num_data_points > 1

    if            (num_null EQ 0)               then begin
      ; All the data are finite
      *dataset.x_data = x_data
      
    endif else if (n_elements(nanval) GT 0) then begin
      ; Replace NaN/Inf with supplied values
      *dataset.x_data = x_data

      (*dataset.x_data)[null_ind] = nanval[0]
  
    
    endif else if (num_data_points GT 0) then begin
      ; Keep just the good data
      *dataset.x_data = x_data[good_ind]
      
    endif else begin
      ; There are no good data points
      print, 'dataset_1d: WARNING! All data are Nan/Inf'
      *dataset.x_data = [0]
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
  dataset.metric     		  = dataset_index
  dataset.hidden_flag     	= 0
  dataset.disable_delete 	= 0
  dataset.plot.psym		    = 10
  dataset.plot.plotsym		  = ''
  
  (*st).stale_titles = 1
endif


;;---------------------------------------------------------------------------
;; We delete a dataset merely by setting its name to ''.
if keyword_set(delete) then begin
    dataset.name = ''
    *dataset.x_data = [0]
    dataset.num_data_points = 1
endif
  
;;---------------------------------------------------------------------------
;; Handle some more keywords that modify a dataset.
if keyword_set(norm_absc)           then dataset.norm_absc = ptr_new(norm_absc)
if keyword_set(norm_val)            then dataset.norm_val  = ptr_new(norm_val)
if (0 NE n_elements(color))         then dataset.plot.color  = color
if (0 NE n_elements(linestyle))     then dataset.plot.line   = linestyle
if (0 NE n_elements(psym))           then dataset.plot.psym       = psym
if (0 NE n_elements(plotsym))        then dataset.plot.plotsym    = plotsym
if (0 NE n_elements(description))   then dataset.description = description
if (0 NE n_elements(metric))        then dataset.metric      = metric
if (0 NE n_elements(disable_delete)) then $
    dataset.disable_delete = disable_delete 

if (n_elements(binsize) NE 0) then begin
  dataset.binning.desired_delta_x = abs(binsize)
  dataset.filter_stale = 1B
endif

if (n_elements(bin_location) NE 0) then begin
  dataset.binning.xbin_location = bin_location
  dataset.filter_stale = 1B
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
  ApplyDataset1dRoi, dataset, st
  mask = dataset.mask
  redraw_flag = 0
endif

density_widget = (*st).density_widget

;;---------------------------------------------------------------------------
;; Store the dataset structure we've been modifying.
Save1dDataset, dataset, st


;;---------------------------------------------------------------------------
;; The REDRAW keyword overrides redraw_flag calculated above.
if (n_elements(redraw) EQ 1) then redraw_flag = redraw
if (widget_info(top_base, /REALIZED) AND redraw_flag) then RedrawDataset1d, st


;;---------------------------------------------------------------------------
;; Print without user intervention, if directed.
if keyword_set(print_now) then begin
  (*st).print_now = 1
  event={ID:(*st).file_menu, TOP:top_base, HANDLER:top_base, VALUE:1}
  Dataset1dEvent, Event
endif

return
END
