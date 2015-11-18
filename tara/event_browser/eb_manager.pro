;+
;==========================================================================
;;;
;;; FILE NAME:    $Id: eb_manager.pro 3442 2009-06-17 12:51:02Z patb $
;;;
;;; DESCRIPTION:  Manager Widget for X-ray Event Browser application
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        See the document "X-ray Event Browser Software Design
;;;               Specification"
;;;
;-
;==========================================================================
PRO CreateManagerWidget, recover_flag, block_flag, GENERIC=generic, _EXTRA=extra

qualcode_mode = 2

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.
sz = get_screen_size()

top_base = widget_base( TITLE='Event Browser Manager', $
			/COLUMN, XOFFSET=sz[0]-430 )

 status_label = widget_label( top_base, /ALIGN_CENTER, /DYNAMIC_RESIZE, $
			VALUE='no working dataset exists' )

 dataset_name = cw_field(top_base, /ROW, /STRING, /RETURN_EVENTS, XSIZE=28,$
			 TITLE='Working Dataset Name', VALUE='WDS')

 txt = 'Retain plots for old working datasets'
 retain_button = cw_bgroup( top_base, [txt], /NONEXCLUSIVE, SET_VALUE=0 )

 button_base = widget_base( top_base, COLUMN=4, /ALIGN_CENTER, SPACE=2,/BASE_ALIGN_CENTER )

 if keyword_set(generic) then begin
  button_base = widget_base( top_base, ROW=3, /ALIGN_CENTER, SPACE=2,/BASE_ALIGN_CENTER )
  gen_fn_1d    = widget_button( button_base, VALUE='Y=f(x)' )
  gen_ds_1n    = widget_button( button_base, VALUE='Univariate Distribution' )
  gen_ds_2d    = widget_button( button_base, VALUE='Bivariate Distribution' )
  gen_trend_1d = widget_button( button_base, VALUE='Trend-1D' )
  gen_ds_3d    = widget_button( button_base, VALUE='Trivariate Distribution' )
  gen_stat_map = widget_button( button_base, VALUE='Statistic Map' )
 
  spectral  = 0L
  spatial   = 0L
  timestamp = 0L
  grade     = 0L
  hardware  = 0L
  stat_maps = 0L
  bivariate = 0L
  trend_1d  = 0L
  obsolete  = 0L
  qualcode  = 0L
 endif else begin
  button_base = widget_base( top_base, COLUMN=4, /ALIGN_CENTER, SPACE=2,/BASE_ALIGN_CENTER )

  spectral = widget_button( button_base, VALUE='Spectrum (eV)' )

  menu = [{ CW_PDMENU_S, flags:3, name:'Positions' }, $
	  { CW_PDMENU_S,       0,      'Image' }, $
	  { CW_PDMENU_S,       0,      'Image Animation' }, $
	  { CW_PDMENU_S,       0,      'Distribution along X' }, $
	  { CW_PDMENU_S,       0,      'Distribution along Y' }, $
	  { CW_PDMENU_S,       0,      'Dispersion Distances' }, $
	  { CW_PDMENU_S,       0,      'Fit X dither' }, $
	  { CW_PDMENU_S,       2,      'Fit Y dither' } ]

  spatial = cw_pdmenu( button_base, menu, /RETURN_NAME )

  timestamp = widget_button( button_base, VALUE='Light Curve')

  menu = [{ CW_PDMENU_S, flags:3, name:'Grades' }, $
	  { CW_PDMENU_S,       0,      'ASCA Grade Filter'}, $
	  { CW_PDMENU_S,       0,      'ACIS Grade Filter'}, $
	  { CW_PDMENU_S,       0,      'ASCA Distribution (working dataset)'},$ 
	  { CW_PDMENU_S,       2,      'ACIS Distribution (working dataset)'} ]

  grade = cw_pdmenu( button_base, menu, /RETURN_NAME )

  menu = [{ CW_PDMENU_S, flags:3, name:'Detector' }, $
	  { CW_PDMENU_S,       0,      'CCD/AMP Filter' }, $
	  { CW_PDMENU_S,       0,      'Row/Column Filter' }, $
	  { CW_PDMENU_S,       2,      'Event Island Values'} ]

  hardware = cw_pdmenu( button_base, menu, /RETURN_NAME )

  menu = [{ CW_PDMENU_S, flags:3, name:'Statistic Maps' }, $
	  { CW_PDMENU_S,       0,      'Median Energy' }, $
	  { CW_PDMENU_S,       0,      'Density' }, $
	  { CW_PDMENU_S,       0,      'Burst Length' }, $
	  { CW_PDMENU_S,       2,      'Bias Error Estimate'} ]

  stat_maps = cw_pdmenu( button_base, menu, /RETURN_NAME )

  menu = [{ CW_PDMENU_S, flags:3, name:'Bivariate' }, $
	  { CW_PDMENU_S,       0,      'Exposure#  versus Energy' }, $
	  { CW_PDMENU_S,       0,      'Time       versus Energy' }, $
	  { CW_PDMENU_S,       0,      'x_position versus Energy' }, $
	  { CW_PDMENU_S,       2,      'y_position versus Energy'} ]

  bivariate = cw_pdmenu( button_base, menu, /RETURN_NAME )

  trend_1d = widget_button( button_base, VALUE='Trend-1D' )
 

  obsolete = 0L
; menu = [{ CW_PDMENU_S, flags:3, name:'Obsolete' } ]
; obsolete = cw_pdmenu( button_base, menu, /RETURN_NAME )
  
  menu = ["accept only high-quality events", "accept only low-quality events", $
          'ignore Quality Code']
  qualcode = widget_droplist(top_base, VALUE=menu)
  widget_control, qualcode, SET_DROPLIST_SEL=qualcode_mode


  gen_fn_1d    = 0L
  gen_ds_1n    = 0L
  gen_ds_2d    = 0L
  gen_trend_1d = 0L
  gen_ds_3d    = 0L
  gen_stat_map = 0L
 endelse

 bottom_base = widget_base( top_base, ROW=1, /ALIGN_RIGHT )

  apply_button = widget_button( bottom_base, VALUE='APPLY FILTERS' )

  menu = [ $
	 { CW_PDMENU_S, flags:3, name:'Save Working Dataset' }, $
	 { CW_PDMENU_S,       0,      'as FITS event list' }, $ 
	 { CW_PDMENU_S,       2,      'as FITS spectrum for XSPEC' } ]

  save_menu = cw_pdmenu( bottom_base, menu, /RETURN_NAME )

  about_button = widget_button( bottom_base, VALUE='About EB' )

  destroy_button = widget_button( bottom_base, VALUE='QUIT' )

;; "Declare" structures to represent filter and display widgets.
dummy = {EB_WIDGET, id: 0L, realized:0B, filter:0B, description: '' }


;; Create a top-level Data Source Widget.
domain_dataset_widget = CreateDomainDataset(top_base, recover_flag, $
					    GENERIC=keyword_set(generic),$
					    _EXTRA=extra)


;; Store the widget state.

state = { $
	; Data Source Widget
	domain_dataset_widget:domain_dataset_widget, $  

	; Lists of filter and display widgets.
	widget_list : replicate({EB_WIDGET},  25), $


	;IDs of widgets that generate events or have to be updated.
	top_base:top_base, status_label:status_label, $
	dataset_name:dataset_name, retain_button:retain_button,$

	grade:grade, trend_1d:trend_1d, spectral:spectral, timestamp:timestamp, $
	spatial:spatial, stat_maps:stat_maps, hardware:hardware, $
	bivariate:bivariate, obsolete:obsolete, $
	qualcode:qualcode, qualcode_mode:qualcode_mode, $
	
	gen_fn_1d:gen_fn_1d, gen_ds_1n:gen_ds_1n, gen_ds_2d:gen_ds_2d, $
	gen_trend_1d:gen_trend_1d, gen_ds_3d:gen_ds_3d, $
	gen_stat_map:gen_stat_map, $

	apply_button:apply_button, save_menu:save_menu, $
	destroy_button:destroy_button, about_button:about_button,$

	;Other state info
	domain_set_revised: 1, working_dataset_available: 0, $
	generic:keyword_set(generic), manager_widget_handle:handle_create() $
	}

;; Disable/enable the Apply Filters button and Save button based on the
;; recover flag.
widget_control, save_menu,  SENSITIVE=0

if (NOT keyword_set(recover_flag)) then begin
  widget_control, apply_button, SENSITIVE=0
endif 

widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

;; Realize the widget and register with manager.
widget_control, top_base, /REALIZE

xmanager, 'filter & display manager', top_base, NO_BLOCK=(block_flag EQ 0), $
	  EVENT_HANDLER='ManagerWidgetEvent', /JUST_REG

return
end


;==========================================================================
PRO eb_custom_filter, mask
mask = 0  &  dum = temporary(mask)
return
end


;==========================================================================
PRO ManagerWidgetEvent, event

destroy_flag   = 0

;; Get the widget state structure.
top_base = event.handler
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY
widget_control, /HOURGLASS

;; We need to detect when the flag working_dataset_available changes, so
;; let's make a copy of it here and then look at it again at the end.
working_dataset_available = state.working_dataset_available


;; Make sure we know which widgets are still around; retrieve the working
;; dataset name.
UpdateRealizedFlags, state, OPEN_SLOT=open_slot
widget_control, state.dataset_name, GET_VALUE=dataset_name
dataset_name = dataset_name[0]

;; Handle the event; branch first based on which widget generated it.
case event.id of

  ;------------------------------------------------------------------------
  state.top_base: $
    begin
    case event.name of
      ;--------------------------------------------------------------------
      'domain_set_revised': $
	begin
	;; Record that the DDS has been changed and the WDS is stale.
	state.domain_set_revised        = 1
	state.working_dataset_available = 0


	;; Try to name the WDS automatically.
	dataset_name = fxpar( GetPrimaryKywds(), 'OBS_ID', COUNT=count )
	if (count EQ 0) then $
	  dataset_name = fxpar( GetTableKywds(), 'OBS_ID', COUNT=count )

	if (count EQ 0) AND (event.filename NE '') then begin
          fdecomp, event.filename, disk, path, name, qual
	  if ('' NE qual) then name = name+ '.' +qual
	  dataset_name = name
	  count = 1
	endif

	if (count NE 0) then begin
          widget_control, state.dataset_name, SET_VALUE=dataset_name
          
          ;; If desired, destroy the WDS plots in all widgets.
          ;; We SHOULD do this by simulating an event from the dataset_name
          ;; widget, BUT the cw_field widget does not like having events
          ;; "sent" to it.
          widget_control, state.retain_button, GET_VALUE=retain
          
          if (retain[0] EQ 0) then begin
            indexes = where( state.widget_list.realized, count )
            for ii=0, count-1 do begin
              id = state.widget_list(indexes(ii)).id
            
              new_event = { WORKING_SET_DELETE, ID:id, TOP:id, HANDLER:0L, $
		            NAME: 'working_set_delete' }
 
              widget_control, id, SEND_EVENT=new_event
            endfor
          endif
	endif


	;; If requested, generate an event for the apply_button so that the 
	;; working dataset will be automatically recomputed (just as if the 
	;; user had pushed the button).
	if (event.COMPUTE_WDS) then begin
	  id = state.apply_button
	  widget_control, id, SEND_EVENT={ID:id, TOP:state.top_base, HANDLER:0L}
	endif
	end

      ;--------------------------------------------------------------------
      'filter_revised': $
	begin

	;; Update the description of the filter that generated the event.
	indexes = where( state.widget_list.id EQ event.filter_id, count )

	state.widget_list(indexes(0)).description = event.description

	state.working_dataset_available = 0
	end

      ;--------------------------------------------------------------------
      else: message, 'event not handled'
    endcase
    end

    
  ;------------------------------------------------------------------------
  state.dataset_name: $
    begin
    indexes = where( state.widget_list.realized, count )

    ;; If desired, destroy the WDS plots in all widgets.
    widget_control, state.retain_button, GET_VALUE=retain
    
    ;; Generate a working_set_fresh event for each widget.
    for ii=0, count-1 do begin
      id = state.widget_list(indexes(ii)).id

      if (retain[0] EQ 0) then begin
        new_event = { WORKING_SET_DELETE, ID:id, TOP:id, HANDLER:0L, $
		      NAME: 'working_set_delete' }

        widget_control, id, SEND_EVENT=new_event
      endif 
      
      new_event = { WORKING_SET_FRESH, ID:id, TOP:id, HANDLER:0L, $
		    NAME: 'working_set_fresh', DATASET_NAME:dataset_name }

      widget_control, id, SEND_EVENT=new_event
    endfor
    end


  ;------------------------------------------------------------------------
  state.retain_button: $
    begin
    end


  ;------------------------------------------------------------------------
  state.grade: $
    begin
    case event.value of
     'ACIS Distribution (working dataset)': $
      begin
      state.widget_list(open_slot).id = $
       eb_dataset_1d( state.top_base, dataset_name, $
				  'instrument_grade', GROUP=state.top_base )
      state.widget_list(open_slot).filter = 1
      end

     'ASCA Distribution (working dataset)': $
      begin
      state.widget_list(open_slot).id = $
       grade_distribution( state.top_base, dataset_name, $
			       GROUP=state.top_base )
      state.widget_list(open_slot).filter = 0
      end

     'ASCA Grade Filter': $
      begin
      state.widget_list(open_slot).id = $
       asca_grade_filter( state.top_base, GROUP=state.top_base )
      state.widget_list(open_slot).filter = 1
      state.working_dataset_available = 0
      end

     'ACIS Grade Filter': $
      begin
      state.widget_list(open_slot).id = $
       acis_grade_filter( state.top_base, GROUP=state.top_base )
      state.widget_list(open_slot).filter = 1
      state.working_dataset_available = 0
      end
    endcase
    end

  ;------------------------------------------------------------------------
  state.trend_1d: $
    begin
    state.widget_list(open_slot).id = $
       eb_trend_1d( state.top_base, dataset_name, 'asca_grade', 'energy', GROUP=state.top_base )
    state.widget_list(open_slot).filter = 0
    end

  ;------------------------------------------------------------------------
  state.spectral: $
    begin
    state.widget_list(open_slot).id = $
       eb_dataset_1d( state.top_base, dataset_name, $
				  'energy', GROUP=state.top_base )
    state.widget_list(open_slot).filter = 1
    end

  ;------------------------------------------------------------------------
  state.timestamp: $
    begin
    state.widget_list(open_slot).id = $
       eb_dataset_1d( state.top_base, dataset_name, $
				  'timestamp', GROUP=state.top_base )
    state.widget_list(open_slot).filter = 1
    end

  ;------------------------------------------------------------------------
  state.obsolete: $
    begin
    case event.value of
    endcase
    end

  ;------------------------------------------------------------------------
  state.spatial: $
    begin
    case event.value of
     'Image': $
      begin
      state.widget_list(open_slot).id = $
       eb_dataset_2d( state.top_base, $
		dataset_name, 'x_position', 'y_position', GROUP=state.top_base )
      state.widget_list(open_slot).filter = 1
      end

     'Image Animation': $
      begin
      state.widget_list(open_slot).id = $
       eb_dataset_3d( state.top_base, dataset_name, GROUP=state.top_base, $
                      'x_position', 'y_position', 'timestamp' )
      state.widget_list(open_slot).filter = 0
      end

     'Distribution along X': $
      begin
      state.widget_list(open_slot).id = $
       eb_dataset_1d( state.top_base, dataset_name, $
				  'x_position', GROUP=state.top_base )
      state.widget_list(open_slot).filter = 1
      end

     'Distribution along Y': $
      begin
      state.widget_list(open_slot).id = $
       eb_dataset_1d( state.top_base, dataset_name, $
				  'y_position', GROUP=state.top_base )
      state.widget_list(open_slot).filter = 1
      end
      
     'Dispersion Distances': $
      begin
      state.widget_list(open_slot).id = $
       diffraction_display( state.top_base, $
		       dataset_name, GROUP=state.top_base, $
		       HANDLE=state.manager_widget_handle )
      state.widget_list(open_slot).filter = 0
      end
      
      'Fit X dither': fit_dither, /XPOSITION
      'Fit Y dither': fit_dither, /YPOSITION
    endcase
    end


  ;------------------------------------------------------------------------
  state.hardware: $
    begin
    case event.value of
     'CCD/AMP Filter': $
      begin
      state.widget_list(open_slot).id = $
	ccd_filter( state.top_base, GROUP=state.top_base )
      state.widget_list(open_slot).filter = 1
      state.working_dataset_available = 0
      end

     'Row/Column Filter': $
      begin
      state.widget_list(open_slot).id = $
	rowcolumn_filter( state.top_base, state.manager_widget_handle, $
			       GROUP=state.top_base )
      state.widget_list(open_slot).filter = 1
      state.working_dataset_available = 0
      end
      
     'Event Island Values': $
      begin
      state.widget_list(open_slot).id = $
                     eb_island_display( state.top_base, dataset_name, $
					GROUP=state.top_base )
      state.widget_list(open_slot).filter = 0
      end
    endcase
    end

  ;------------------------------------------------------------------------
  state.stat_maps: $
    begin
    case event.value of
     'Median Energy': $
      begin
      state.widget_list(open_slot).id = $
       eb_dataset_3d( state.top_base, dataset_name, GROUP=state.top_base,$
       		      'x_position', 'y_position', 'energy', STAT_CODE=2 )
      state.widget_list(open_slot).filter = 0
      end

     'Density': $
      begin
      state.widget_list(open_slot).id = $
       eb_dataset_3d( state.top_base, dataset_name, GROUP=state.top_base,$
       		      'x_position', 'y_position', 'exposure', STAT_CODE=10 )
      state.widget_list(open_slot).filter = 0
      end

     'Burst Length': $
      begin
      state.widget_list(open_slot).id = $
       eb_dataset_3d( state.top_base, dataset_name, GROUP=state.top_base,$
       		      'x_position', 'y_position', 'exposure', STAT_CODE=9 )
      state.widget_list(open_slot).filter = 0
      end

     'Bias Error Estimate': $
      begin
      state.widget_list(open_slot).id = $
                     eb_island_display( state.top_base, dataset_name, $
					GROUP=state.top_base, /MAP )
      state.widget_list(open_slot).filter = 0
      end
    endcase
    end

  ;------------------------------------------------------------------------
  state.bivariate: $
    begin
    case event.value of
     'Exposure#  versus Energy': $
      begin
      state.widget_list(open_slot).id = $
       eb_dataset_2d( state.top_base, $
		dataset_name, 'exposure', 'energy', GROUP=state.top_base )
      state.widget_list(open_slot).filter = 1
      end

     'Time       versus Energy': $
      begin
      state.widget_list(open_slot).id = $
       eb_dataset_2d( state.top_base, $
		dataset_name, 'timestamp', 'energy', GROUP=state.top_base )
      state.widget_list(open_slot).filter = 1
      end

     'x_position versus Energy': $
      begin
      state.widget_list(open_slot).id = $
       eb_dataset_2d( state.top_base, $
		dataset_name, 'x_position', 'energy', GROUP=state.top_base )
      state.widget_list(open_slot).filter = 1
      end

     'y_position versus Energy': $
      begin
      state.widget_list(open_slot).id = $
       eb_dataset_2d( state.top_base, $
		dataset_name, 'y_position', 'energy', GROUP=state.top_base )
      state.widget_list(open_slot).filter = 1
      end

    endcase
    end


  ;------------------------------------------------------------------------
  state.gen_fn_1d: $
    begin
    state.widget_list(open_slot).id = $
       eb_function_1d( state.top_base, /GENERIC,$
		dataset_name, '', '', GROUP=state.top_base )
    state.widget_list(open_slot).filter = 1
    end


  ;------------------------------------------------------------------------
  state.gen_ds_1n: $
    begin
    state.widget_list(open_slot).id = $
       eb_dataset_1d( state.top_base, /GENERIC, dataset_name, $
				  '', GROUP=state.top_base )
    state.widget_list(open_slot).filter = 1
    end


  ;------------------------------------------------------------------------
  state.gen_ds_2d: $
    begin
    state.widget_list(open_slot).id = $
       eb_dataset_2d( state.top_base, /GENERIC,$
		dataset_name, '', '', GROUP=state.top_base )
    state.widget_list(open_slot).filter = 1
    end


  ;------------------------------------------------------------------------
  state.gen_trend_1d: $
    begin
    state.widget_list(open_slot).id = $
       eb_trend_1d( state.top_base, /GENERIC, dataset_name, '', '', GROUP=state.top_base)
    state.widget_list(open_slot).filter = 1
    end


  ;------------------------------------------------------------------------
  state.gen_ds_3d: $
    begin
    state.widget_list(open_slot).id = $
       eb_dataset_3d( state.top_base, /GENERIC, dataset_name, $
       		      GROUP=state.top_base, '', '', '' )        
    state.widget_list(open_slot).filter = 1
    end


  ;------------------------------------------------------------------------
  state.gen_stat_map: $
    begin
    state.widget_list(open_slot).id = $
       eb_dataset_3d( state.top_base, /GENERIC, dataset_name, $
       		      GROUP=state.top_base, '', '', '', STAT_CODE=2 )        
    state.widget_list(open_slot).filter = 1
    end


  ;------------------------------------------------------------------------
  state.qualcode: $
    begin
    state.qualcode_mode = event.index
    state.working_dataset_available = 0
    end

  ;------------------------------------------------------------------------
  state.apply_button: $
    begin
    ;; First, make sure all the realized filter widgets are notified if the
    ;; domain dataset has been revised.
    if (state.domain_set_revised) then begin
      state.domain_set_revised = 0
      
      indexes = where( state.widget_list.realized, count )
      
      for ii=0, count-1 do begin
	  wid = state.widget_list(indexes(ii))

	  new_event = { ID:wid.id, TOP:wid.id, HANDLER:0L, $
		        NAME: 'domain_set_revised' }

	  if (wid.filter) then widget_control, wid.id, SEND_EVENT=new_event
       endfor
     endif
     
     ;; WE MUST MUST MUST MAKE SURE THAT THE DOMAIN_SET_REVISED EVENTS WE SENT 
     ;; TO THE FILTER WIDGETS (ABOVE) HAVE ALREADY BEEN PROCESSED BEFORE WE
     ;; START ASKING THOSE SAME FILTER WIDGETS FOR FILTER MASKS (BELOW)!!!!
     ;; OTHERWISE THE FILTER WIDGETS WILL COMPUTE MASKS USING AN OLD
     ;; DOMAIN DATASET.
     ;;
     ;; BUT FIRST WE MUST CLEAR ANY PENDING EVENTS THAT ARE HANDLED BY THIS
     ;; ROUTINE BECAUSE THE widget_event() CALL BELOW WOULD CAUSE THIS 
     ;; EVENT HANDLER TO BE CALLED AGAIN (RECURSIVELY) LEADING TO AN 
     ;; UNDEFINED STATE VARIABLE.
;print, 'calling widget_event() so filters can get new DDS'     
     widget_control, top_base, /CLEAR_EVENT
     dum = widget_event(/NOWAIT)
     widget_control, /HOURGLASS
;print, 'widget_event() RETURNED'     


    ;; Count how many normal filter widgets are defined.
    indexes = where( state.widget_list.realized AND $
    		     state.widget_list.filter, count )
    line_count=3+count
    
    ;; Make a status widget.
    txt = strarr( line_count )
    TimedMessage, msg_widget, txt, GROUP=top_base, $
		  TITLE='FILTERING DOMAIN DATASET...', POS=state.apply_button
    print, 'FILTERING DOMAIN DATASET...'
    
    ;; See if a cutom mask function is defined.
    eb_custom_filter, custom_mask
    custom_mask_available = (n_elements(custom_mask) GT 0) 
    if custom_mask_available then txt[0] = '>> Custom Filter'
    
    ;; Apply any qualcode filter defined.
    qual_mask_available = 0
    
    if GetProperty( 'qualcode', DDS=qualcode ) then begin
      if (state.qualcode_mode EQ 0) then begin
        qual_mask = (0 EQ *qualcode)
        qual_mask_available = 1
        txt[1] = '>> Quality Code'
      endif else if (state.qualcode_mode EQ 1) then begin
        qual_mask = (0 NE *qualcode)
        qual_mask_available = 1
        txt[1] = '>> Quality Code'
      endif
    endif
    
    ;; Create a composite mask from whatever masks we have so far.
    case 1 of
     (custom_mask_available AND qual_mask_available): $
       composite_mask = custom_mask AND qual_mask
      
     (custom_mask_available): $
       composite_mask = temporary(custom_mask)
       
     (qual_mask_available): $
       composite_mask = temporary(qual_mask)
       
     else: $
       composite_mask = replicate( 1B, CountXrayData() > 1 )
    endcase
    
          
    ;; Obtain a filter mask from each filter widget and compute a 
    ;; composite mask.
    for ii=0, count-1 do begin
      wid = state.widget_list(indexes(ii))

      widget_control, wid.id, GET_VALUE=mask

      if (n_elements(mask) GT 1) then begin
          composite_mask = temporary(composite_mask) AND mask
          txt[ii+2] = '>> ' + wid.description
          TimedMessage, msg_widget, txt
      endif
    endfor
    txt[line_count-1] = 'DONE'
    TimedMessage, msg_widget, txt, LIFE=10

    SetWdsMask, composite_mask
    state.working_dataset_available = 1
    end

  ;------------------------------------------------------------------------
  state.save_menu: $
    begin
    case event.value of
      'as FITS event list': $
        SaveWorkingDataset, dataset_name, state.save_menu, /EVENT_LIST

      'as FITS spectrum for XSPEC': $
        SaveWorkingDataset, dataset_name, state.save_menu, /SPECTRUM
    endcase
    end

  ;------------------------------------------------------------------------
  state.destroy_button: $
    begin
      result=dialog_message("Confirm:  Quit Event Browser?",/CANCEL, $
			    DIALOG_PARENT=state.destroy_button)
      if (result EQ 'OK') then destroy_flag = 1
    end

  ;------------------------------------------------------------------------
  state.about_button: $
    begin
    str=['Event Browser, version $Revision: 3442 $','',$
         'Event Browser is designed to visualize and analyze data from X-ray',$
	 'detectors stored as FITS event lists (see OGIP Memo OGIP/94-003).',$
	 'Source code and a manual are available at',$
	 '  http://www.astro.psu.edu/xray/docs/','',$
	 'Event Browser was written by Patrick Broos & Scott Koch at',$
	 'The Pennsylvania State University under a NASA contract to',$
	 'support calibration of the AXAF CCD Imaging Spectrometer (ACIS).',$
	 'Please direct questions and comments to patb@astro.psu.edu.']

      result = dialog_message(str, /INFORMATION) 
    end

  ;------------------------------------------------------------------------
  else: message, 'event not handled'
endcase


;; If the working dataset status has CHANGED, then enable/disable the apply 
;; and save buttons, change the status label, and send appropriate events
;; to the filter and display widgets.
if (working_dataset_available NE state.working_dataset_available) then begin
  widget_control, state.dataset_name, GET_VALUE=dataset_name
  dataset_name = dataset_name[0]

  UpdateRealizedFlags, state

  if (state.working_dataset_available) then begin
    widget_control, state.apply_button, SENSITIVE=0
    widget_control, state.save_menu,    SENSITIVE=1

    domain_size  = CountXrayData() > 1
    working_size = CountXrayData( /WORKING_DATASET )

    fmt='("working dataset ready (",I0," xray events, ",F5.1,"%)")'
    pcnt = (100.0*working_size)/domain_size
    status = string( working_size, pcnt, f=fmt)


    ;; Generate working_set_fresh events for each realized widget.

    indexes = where( state.widget_list.realized, count )

    for ii=0, count-1 do begin
      id = state.widget_list(indexes(ii)).id

      new_event = { WORKING_SET_FRESH, ID:id, TOP:id, HANDLER:0L, $
		    NAME: 'working_set_fresh', DATASET_NAME:dataset_name }

      widget_control, id, SEND_EVENT=new_event
    endfor

  endif else begin
    widget_control, top_base, /SHOW
    widget_control, state.apply_button, SENSITIVE=1
    widget_control, state.save_menu,    SENSITIVE=0

    status ='filters modified; working dataset invalid'

    ;; Generate working_set_stale events for each realized widget.

    indexes = where( state.widget_list.realized, count )

    for ii=0, count-1 do begin
      id = state.widget_list(indexes(ii)).id

      new_event = { WORKING_SET_STALE, ID:id, TOP:id, HANDLER:0L, $
	            NAME: 'working_set_stale' }

      widget_control, id, SEND_EVENT=new_event
    endfor
  endelse

  widget_control, state.status_label, SET_VALUE=status

endif

;; Free all Event Browser handles
if (destroy_flag) then handle_free, state.manager_widget_handle

;; Save the state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY


;; Destroy the widget if desired.
if (destroy_flag) then widget_control, top_base, /DESTROY


return
end


;==========================================================================
;;; Update the "realized" flag in the filter_widget and display_widget lists.
;;; The keyword parameter OPEN_SLOT returns the index of an empty slot in
;;; the list, or -1 if the list is full.

PRO UpdateRealizedFlags, state, OPEN_SLOT=open_slot

open_slot = -1
list_full = 1
for ii=0, N_ELEMENTS(state.widget_list)-1 do begin

  realized = widget_info( state.widget_list(ii).id, /VALID_ID )

  state.widget_list(ii).realized = realized

  if (NOT realized) then begin
    list_full = 0
    open_slot = ii
    state.widget_list(open_slot).description = ''
  endif
endfor

return
end


;==========================================================================
;;; Save the working dataset in the selected format.
;==========================================================================
PRO SaveWorkingDataset, dataset_name, parent, $
			EVENT_LIST=event_list, SPECTRUM=spectrum

if (CountXrayData(/WORKING_DATASET) EQ 0) then return

;; Let the user choose the filename.
if (keyword_set( event_list )) then begin
  savetype = "FITS events"
  fn = dataset_name + '.evt'
  fn = strcompress( fn, /REMOVE_ALL )
  pathname = dialog_pickfile( TITLE="Save Event List (FITS format)", $
			      FILE=fn(0) )
endif else begin
  savetype = "FITS spectrum"
  fn = dataset_name + '.pha'
  fn = strcompress( fn, /REMOVE_ALL )
  pathname = dialog_pickfile( TITLE="Save Spectrum (XSPEC/OGIP format)", $
			      FILE=fn(0) )
endelse

if (pathname EQ '') then return

widget_control, /HOURGLASS

;; Add some Primary HDU keywords.  
AddPrimaryKywd, "HISTORY", BEFORE='FITSVERS', $
		   "written by Event Browser, version $Revision: 3442 $, " + systime()


case ( savetype ) of
  "FITS events": $
    begin
    ;; Let the user choose which event properties to write out.
    fits_names    = GetPropertyNames( /FITS )
    derived_names = GetPropertyNames( /DERIVED )
    num_fits      = n_elements(fits_names)
    num_derived   = n_elements(derived_names)
    
    ; Make up the FITS property form.
    labels = strarr(num_fits)
    for ii = 0, num_fits-1 do begin
      dum = GetProperty(fits_names[ii], prop_ptr)
      labels[ii] = (*prop_ptr).col_name
    endfor

    bgroup = string(labels, F='("0,BUTTON,", 99(A,:,"|"))') + ',TAG=fits_bgroup'
    form1 = ['1,BASE,,COLUMN', $
             '0,LABEL,FITS Properties', $
             bgroup, $
             '2,BASE']
                     
    ; Make up the derived property form.
    labels = strarr(num_derived)
    for ii = 0, num_derived-1 do begin
      if GetProperty(derived_names[ii], prop_ptr, /DEF_ONLY) then begin
        labels[ii] = string( (*prop_ptr).name,(*prop_ptr).col_name, $
      		             F='(A," (",A,")")' )
      endif 
    endfor
    
    index = where( labels NE '', num_derived )

    if (num_derived GT 0) then begin
      derived_names = derived_names[index]
      labels        = labels[index]
      
      bgroup = string(labels, F='("0,BUTTON,", 99(A,:,"|"))') + $
      			',TAG=derived_bgroup'
      form2 = ['1,BASE,,COLUMN', $
               '0,LABEL,Derived Properties (column name)', $
               bgroup, $
               '2,BASE']
    endif else form2 = '0,BASE,,' 
            
    ok = '0, BUTTON, OK, QUIT'

    top_base = widget_base(TITLE='Choose Contents of FITS File')
    form     = cw_form(top_base, [form1,form2,ok])
    widget_control, top_base, /REALIZE
    widget_control, form, GET_VALUE=st
    st.fits_bgroup = 1
    widget_control, form, SET_VALUE=st
    
    repeat begin
      event = widget_event(form)
    endrep until event.quit
    
    widget_control, form, GET_VALUE=st
    widget_control, top_base, /DESTROY
    
    
    ;; Extract the property names the user wants to keep.
    names = ''
    index = where(st.fits_bgroup, num_fits)
    if (num_fits GT 0) then names = [names, fits_names[index]]
    
    if (num_derived GT 0) then begin
      index = where(st.derived_bgroup, num_derived)
      if (num_derived GT 0) then names = [names, derived_names[index]]
    endif
    
    if ((num_fits+num_derived) EQ 0) then return
    
    
    TimedMessage, msg, ['Writing event list ' + pathname, $
                    "This may take several minutes for large datasets ..."], $
                    TITLE="Saving Working Dataset ..." 
    
    WriteEvents, names[1:*], pathname, ERROR=error
    if keyword_set(error) then dum=dialog_message(error,DIALOG_PAR=parent,/ERR)

    widget_control, msg, /DESTROY, BAD_ID=bad_id
    end
 
  "FITS spectrum": $
    begin
    ;; Write the primary HDU.
    pheader = GetPrimaryKywds()
    fxhmake, pheader, /EXTEND

    fdecomp, pathname, disk, item_path, item_name, item_qual
    if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
    fxaddpar, pheader, "FNFITS", item_name

    writefits, pathname, 0, pheader
    
    prop_flags  = [GetProperty('energy', /DEF_ONLY), $
    		   GetProperty('PI',     /DEF_ONLY), $
    		   GetProperty('pha',    /DEF_ONLY)]
    prop_labels = ['"energy" property (nominal event energy in eV)',$
    		   '"PI" FITS column (computed outside Event Browser)',$
    		   '"pha" property (do not use with multiple amplifiers)']
    		   
    index = where(prop_flags, count)
    if (count EQ 0) then begin
      msg='Cannot compute a spectrum from the event properties currently defined.
      dum=dialog_message(msg,DIALOG_PAR=parent,/INF)
      return
    endif
    
    text=['A spectrum following the OGIP standard is defined in terms of',$
          'pulse height or energy "channels" numbered from 1 to the value',$
          'of the keyword DETCHANS.  The specific energy-related quantity',$
          'that is binned up\, the bin size\, and the location of bin #1',$
          'are not specified by the standard\, but are instead conventions',$
          'adopted by each instrument''s user community.  You must specify',$
          'this information below so that the spectral file created here',$
          'is compatible with any response matrix files you are using.']
    
    f0 = string(prop_labels[index], F='("0,DROPLIST,", 99(A,:,"|"))') + $
    	 ', SET_VALUE=0, TAG=prop'
    f1 = '0, DROPLIST, PI|PHA, SET_VALUE=0, LABEL_LEFT=CHANTYPE keyword,' + $
         'TAG=chantype'
    	 
    f2 = '0, FLOAT, 1.0, LABEL_LEFT=Left edge of channel #1,TAG=min_amplitude'
    f3 = '0, FLOAT, 1.0,    LABEL_LEFT=Channel width, TAG=binsize'
    f4 = '0, INTEGER, 1024, LABEL_LEFT=DETCHANS keyword, TAG=num_channels'
    f5 = '0, INTEGER, 1,    LABEL_LEFT=EXPOSURE keyword, TAG=exposure'
    f6 = '2, BUTTON, OK, QUIT, TAG=ok'

    result = cw_form(['0,LABEL,'+text+',LEFT',f0,f1,f2,f3,f4,f5,f6],$
    		     /COLUMN,TITLE='Spectrum Parameters')

    case (index[result.prop]) of
     0: prop_name = 'energy'
     1: prop_name = 'PI'
     2: prop_name = 'pha'
    endcase
    dum = GetProperty(prop_name, WDS_DATA=wds_data)
    
    min_amplitude = result.min_amplitude
    binsize       = result.binsize
    num_channels  = result.num_channels 
    
    bin_table = replicate( { CHANNEL: 0, COUNTS: 0L }, num_channels )
    bin_table.CHANNEL = 1 + indgen( num_channels ) 

    TimedMessage, msg, 'Writing spectrum ' + pathname, $
                           TITLE="Saving Working Dataset ..." 

    counts = histogram( wds_data, MIN=min_amplitude, BINSIZE=binsize )

    if (n_elements(counts) GE num_channels) then begin
	  bin_table.COUNTS = counts(0:num_channels-1)
    endif else begin
	  temp    = lonarr( num_channels )
	  temp(0) = counts
	  bin_table.COUNTS = temp
    endelse


    ;; Write the FITS table.
    fxaddpar, theader, 'EXTNAME',  'SPECTRUM'

    fxaddpar, theader, 'TELESCOP',  'UNKNOWN'
    fxaddpar, theader, 'INSTRUME',  'UNKNOWN'
    fxaddpar, theader, 'FILTER',    'none'
    fxaddpar, theader, 'EXPOSURE',  result.exposure
    fxaddpar, theader, 'AREASCAL',  1.0
    fxaddpar, theader, 'BACKFILE',  'none'
    fxaddpar, theader, 'BACKSCAL',  1.0
    fxaddpar, theader, 'CORRFILE',  'none'
    fxaddpar, theader, 'CORRSCAL',  1.0
    fxaddpar, theader, 'RESPFILE',  'none'
    fxaddpar, theader, 'ANCRFILE',  'none'
    fxaddpar, theader, 'XFLT0001',  'none'
    fxaddpar, theader, 'HDUCLASS',  'OGIP'
    fxaddpar, theader, 'HDUCLAS1',  'SPECTRUM'
    fxaddpar, theader, 'HDUVERS1',  '1.1.0'
    fxaddpar, theader, 'HDUCLAS2',  '        '
    fxaddpar, theader, 'HDUCLAS3',  'COUNT'
    fxaddpar, theader, 'CHANTYPE',  result.chantype ? 'PHA' : 'PI'

    fxaddpar, theader, 'POISSERR', 'T' 
    fxaddpar, theader, 'SYS_ERR',  0  
    fxaddpar, theader, 'QUALITY',  0  
    fxaddpar, theader, 'GROUPING', 0  
    fxaddpar, theader, 'DETCHANS', num_channels  

    comment = 'Property name binned into spectrum: ' + prop_name
    fxaddpar, theader, 'HISTORY', comment

    comment = string( min_amplitude, f='("Left edge of channel #1: ",G10.4)' )
    fxaddpar, theader, 'HISTORY', comment

    comment = string( binsize, f='("Channel width: ", G10.4)' )
    fxaddpar, theader, 'HISTORY', comment

    mwrfits, bin_table, pathname, theader
    print, pathname, " written"
    widget_control, msg, /DESTROY, BAD_ID=bad_id
    end

    else:   print, "Invalid selection.  Save aborted."
endcase
return
end




;==========================================================================
;; We need a procedure with the same name as this file to keep 
;; RESOLVE_ROUTINE happy.
PRO eb_manager
return
end
