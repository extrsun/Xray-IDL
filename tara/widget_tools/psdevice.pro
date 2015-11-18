;+
;========================================================================
;;;
;;; FILE NAME:    $Id: psdevice.pro 4076 2011-10-31 14:14:25Z psb6 $
;;;
;;; DESCRIPTION:  PostScript device configuration tool
;;;
;;;               This widget allows users to configure their output device
;;;               to postcript.  This graphical interface should allow users
;;;               to do so with a minimum of effort.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        
;;;

;;; Calling Sequence:

;;;		PsDevice,filename, success

;;; Two required arguments, filename and success, are OUTPUT
;;; variables which indicate the name of the selected output file for the
;;; PostScript device and whether the configuration
;;; of the PS device was successful.  If the user "Cancel"s the operation or
;;; if an error occurs, then success is set to 0; otherwise, success is set
;;; to 1.  If the user decides to print to the printer, then the filename
;;; returned is a blank string, ''.  Clients should check for this and spawn
;;; an appropriate print command after closing the PostScript device.

;;; A note about color.  The color radio buttons simply toggle the COLOR
;;; keyword to the DEVICE command.  They do NOT change color tables.  It is
;;; up to the caller to select a color table before calling psdevice.  See
;;; the IDL intrinsic command XLOADCT for more information.
;;;
;;; The routine color_manager is used to configure the device -- see the
;;; remarks about color tables in that program.

;;; Here is an example of its use:
;;;
;;;          s = findgen(100)
;;;          s = sin(s)
;;;          ;open the PostScript device
;;;          PsDevice,filename,success
;;;          IF(success EQ 1) THEN BEGIN
;;;            plot, s
;;;            ; close the PostScript device
;;;            device,/close
;;;            ; If a null filename is returned, the user indicated that
;;;            ; printer output is desired.
;;;            IF(filename EQ '') THEN BEGIN
;;;              spawn, 'lpr -r idl.ps'
;;;            ENDIF
;;;            ; select a new device
;;;            color_manager, /X_PSEUDO
;;;          ENDIF

;;; Always check the return status, success.  Notice that the PostScript
;;; device must be closed by the user.  Be sure to open another device
;;; after closing the PostScript device.

;;; The input/output parameter CONFIG is a structure that specifies one or more initial values for the fields in the dialog box, and returns the full tool configuration in place when the user dismissed the tool.

;;; The inputs 	XMARGIN, YMARGIN, ASPECT_RATIO, and COLOR are alternate ways to specify a few aspects of the tool's initial configuration.

;-
;==========================================================================
; This is the event handler function for the PsDevice widget
PRO PsDeviceEvent, Event

destroy_flag = 0

; Get the state structure.
top_base = Event.handler
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY

;; Process the event.
case Event.ID of
;--------------------------------------------------------------------------
  state.fileorlpr_buttons: begin
    (*state.config).printtoprinter = Event.value

    ; Sensitize the correct buttons
    if ((*state.config).printtoprinter EQ 0) then begin
      widget_control, state.filename_field, SET_VALUE=(*state.config).filename
      widget_control, state.filename_button, /SENSITIVE
    endif else begin
      widget_control, state.filename_field, SET_VALUE=''
      widget_control, state.filename_button, SENSITIVE=0
    endelse
  end
;--------------------------------------------------------------------------
  state.cancel_button: begin
    destroy_flag = 1
    (*state.config).success = 0
  end
;--------------------------------------------------------------------------
  state.filename_button: begin
    fdecomp, (*state.config).filename, disk, dir, dummy, qual

    fullname = dialog_pickfile( GROUP=top_base, PATH=dir, FILE=(*state.config).filename, TITLE='Select a PostScript filename' )
    if (fullname NE '') then begin
      (*state.config).filename = fullname
      widget_control, state.filename_field, SET_VALUE=fullname
    endif
  end
;--------------------------------------------------------------------------
  state.configure_button: begin
    if (*state.config).printtoprinter then filename = '~/idl.ps' $
    				      else filename = (*state.config).filename
    
    ; Read and save the geometry of the page.
    aspect_mode  = widget_info( state.aspect_mode, /DROPLIST_SELECT )
    widget_control, state.aspect_ratio, GET_VALUE=aspect_ratio
    widget_control, state.xsize_field, GET_VALUE=xsize
    widget_control, state.ysize_field, GET_VALUE=ysize
    
    (*state.config).aspect_ratio = aspect_ratio
    (*state.config).aspect_mode  = aspect_mode
    (*state.config).xsize   = xsize
    (*state.config).ysize   = ysize
    
    encapsulated = 1
    
;   spawn, '/bin/rm -f idl.ps >& /dev/null'
    ; Make sure we can write to this file.
    openw, Unit, filename, /GET_LUN, ERROR=Error
    if (Error NE 0) then begin
      print, 'ERROR opening file '+ filename
    endif else begin
      free_lun, Unit
      ; Change to PS device and set to requested size.
      color_manager, PS_GREY=    ((*state.config).color_mode EQ 0), $
		     PS_NEG_GREY=((*state.config).color_mode EQ 1), $
		     PS_PSEUDO=  ((*state.config).color_mode EQ 2), $
		     FILENAME=filename, ENCAPSULATED=encapsulated, PREVIEW=0, $
		     LANDSCAPE=(*state.config).landscape_t, $
		     BITS_PER_PIXEL=(*state.config).bits_per_pixel, $
		     XSIZE=xsize, YSIZE=ysize, $
		     INCHES=(*state.config).inches_t

      ; Handle request for fixed aspect mode.
      if (aspect_mode EQ 0) then begin
        ; Adjust size of PS "plot region" so that the "plot data window" has the
        ; desired aspect ratio.
        ; First, compute the margin lengths in device units.
        margin_x = !D.X_CH_SIZE * total( (*state.config).xmargin )
        margin_y = !D.Y_CH_SIZE * total( (*state.config).ymargin )
        
        ; Estimate the current dimensions of the plot data window.
        data_window_x = (!D.X_SIZE - margin_x) > 1
        data_window_y = (!D.Y_SIZE - margin_y) > 1
        
        ; Reduce one of the dimensions of the plot region so that we'll get a
        ; plot data window of the desired shape.  Note that we can't specify
        ; the plot data window dimensions directly -- we specify the plot region
        ; size (plot data window plus margins).
        if ((data_window_y/data_window_x) GE aspect_ratio) then begin
          desired_dev_ysize = (data_window_x*aspect_ratio) + margin_y
          ysize             = ysize * (desired_dev_ysize/!D.Y_SIZE)
        endif else begin
          desired_dev_xsize = (data_window_y/aspect_ratio) + margin_x
          xsize             = xsize * (desired_dev_xsize/!D.X_SIZE)
        endelse
      endif
          
      if encapsulated then begin
        page_long_dim  = xsize > ysize
        page_short_dim = xsize < ysize
      endif else begin
        page_long_dim  = 11 
        page_short_dim = 8.5 
      endelse
      
      if ~(*state.config).inches_t then begin
        page_long_dim  *= 2.54
        page_short_dim *= 2.54
      endif
      
      ; Center the "plot region" on the page.
      if (*state.config).landscape_t then begin
        xoffset = (page_short_dim - ysize) / 2.
        yoffset = page_long_dim - ((page_long_dim - xsize) / 2.)
      endif else begin
        xoffset = (page_short_dim - xsize) / 2.
        yoffset = (page_long_dim  - ysize) / 2.
      endelse
      
      color_manager, XOFFSET=xoffset, YOFFSET=yoffset, $
                XSIZE=xsize, YSIZE=ysize, $
                INCHES=(*state.config).inches_t
  
      destroy_flag = 1
    endelse ; file was writable
  end

;--------------------------------------------------------------------------
  state.preset1_button :begin
    ; Configure several items.
    (*state.config).landscape_t                                =0
    widget_control, state.orientation_mode, SET_DROPLIST_SELECT=0
    
    (*state.config).inches_t                             =1
    widget_control, state.units_mode, SET_DROPLIST_SELECT=1
    
    widget_control, state.xsize_field, SET_VALUE=6.5
    widget_control, state.ysize_field, SET_VALUE=9.0
  end

;--------------------------------------------------------------------------
  state.preset2_button :begin
    ; Configure several items.
    (*state.config).landscape_t                                =0
    widget_control, state.orientation_mode, SET_DROPLIST_SELECT=0
    
    (*state.config).inches_t                             =1
    widget_control, state.units_mode, SET_DROPLIST_SELECT=1
    
    widget_control, state.xsize_field, SET_VALUE=4.5
    widget_control, state.ysize_field, SET_VALUE=9.0
  end

;--------------------------------------------------------------------------
  state.preset3_button :begin
    ; Configure several items.
    (*state.config).landscape_t                                =1
    widget_control, state.orientation_mode, SET_DROPLIST_SELECT=1
    
    (*state.config).inches_t                             =1
    widget_control, state.units_mode, SET_DROPLIST_SELECT=1
    
    widget_control, state.xsize_field, SET_VALUE=9.0
    widget_control, state.ysize_field, SET_VALUE=6.5
  end

;--------------------------------------------------------------------------
  state.orientation_mode: begin
    ; When the orientation button is pressed, fill in some default values
    (*state.config).landscape_t = Event.index
    
    (*state.config).inches_t  = 1
    widget_control, state.units_mode, SET_DROPLIST_SELECT=(*state.config).inches_t
    
    if((*state.config).landscape_t) then begin
;      xoffset = 1.0
 ;     yoffset = 1.0
      xsize = 9.0
      ysize = 6.5
    endif else begin
;      xoffset = 1.0
;      yoffset = 3.0
      xsize = 6.5
      ysize = 9.0
    endelse
    widget_control, state.xsize_field, SET_VALUE=xsize
    widget_control, state.ysize_field, SET_VALUE=ysize
  end
;--------------------------------------------------------------------------
  state.units_mode: begin
    if ((*state.config).inches_t NE Event.index) then begin
      widget_control, state.xsize_field, GET_VALUE=xsize
      widget_control, state.ysize_field, GET_VALUE=ysize
      
      if (*state.config).inches_t then k = 2.54 else k = 1/2.54
      
      widget_control, state.xsize_field, SET_VALUE=xsize * k
      widget_control, state.ysize_field, SET_VALUE=ysize * k
      (*state.config).inches_t = Event.index
    endif
  end
;--------------------------------------------------------------------------
  state.bits_buttons: begin
    case Event.value of
      0: begin
        (*state.config).bits_per_pixel = 1
      end

      1: begin
        (*state.config).bits_per_pixel = 2
      end

      2: begin
        (*state.config).bits_per_pixel = 4
      end

      3: begin
        (*state.config).bits_per_pixel = 8
      end

      else: message, "Invalid callback value found for bits per pixel"
    endcase
  end
;--------------------------------------------------------------------------
  state.color_buttons: begin
    (*state.config).color_mode = event.value
  end

;--------------------------------------------------------------------------
  state.aspect_ratio: begin
  end
;--------------------------------------------------------------------------
  state.aspect_mode: begin
  end
;--------------------------------------------------------------------------
  state.xsize_field: begin
  end
;--------------------------------------------------------------------------
  state.ysize_field: begin
  end
;--------------------------------------------------------------------------
   else: message, 'unknown event in PsDeviceEvent'
endcase

; Save the state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY


; Destroy the widget if desired.
if (destroy_flag) then widget_control, top_base, /DESTROY

RETURN
END

;==========================================================================

PRO PsDevice, parent, filename, success, CONFIG=config, BATCH_MODE=batch_mode, $
	XMARGIN=xmargin, YMARGIN=ymargin, ASPECT_RATIO=asp_r, COLOR=color

if(NOT keyword_set(xmargin)) then xmargin = 0.0
if(NOT keyword_set(ymargin)) then ymargin = 0.0

if (n_elements(color) EQ 0) then begin
  ; Color is the default
  color_mode=2                                                           
endif else color_mode = keyword_set(color) ? 2 : 1


; Establish defaults for all device parameters.
config_default = {color_mode:color_mode, printtoprinter:0B, filename:'idl.ps', $
                  aspect_ratio:1.0, aspect_mode:0, xmargin:0.0, ymargin:0.0, $
                  xsize:6.5, ysize:9.0, inches_t:1B, $
                  landscape_t:0B, bits_per_pixel:8, $
                  success:0B}

; Override defaults with any configuration tags supplied by caller.  	  
if keyword_set(config) then struct_assign, config, config_default, /NOZERO, /VERBOSE
                                                                     
config = config_default

config.success = 1
if keyword_set(asp_r) then config.aspect_ratio=asp_r
if (n_elements(xmargin) EQ 0) then config.xmargin = xmargin
if (n_elements(ymargin) EQ 0) then config.ymargin = ymargin

config_ptr = ptr_new( config )


top_base = widget_base( TITLE="Configure the PostScript Device", $
			GROUP=parent, /MODAL,$
			/COLUMN, XOFFSET=420, YOFFSET=100, XPAD=1, YPAD=1 )
			

  filename_base = widget_base( top_base, /FRAME, /COLUMN)
    fileorlpr_buttons = cw_bgroup( filename_base, ['A File','A Printer'], $
				   /ROW, SPACE=1, XPAD=1, YPAD=1, $
				   /RETURN_INDEX, /NO_RELEASE, /EXCLUSIVE, $
				   LABEL_LEFT='Print To:' )
      filename_field = widget_label(filename_base, $
      				    /DYNAMIC_RESIZE, VALUE=config.filename)
      
      filename_button = widget_button(filename_base, VALUE="Change Filename")
  widget_control, fileorlpr_buttons, set_value=config.printtoprinter
  if config.printtoprinter then begin
      widget_control, filename_button, SENSITIVE=0
      widget_control, filename_field, SET_VALUE=''
  endif

  presets_base = widget_base( top_base, /FRAME, /ROW)
    dum = widget_label(presets_base, VALUE='Presets:')
    temp_base = widget_base(presets_base, /COLUMN)
    preset1_button = widget_button(temp_base, VALUE="Portrait,  full page")
    preset2_button = widget_button(temp_base, VALUE="Portrait,  half page")
    preset3_button = widget_button(temp_base, VALUE="Landscape")


  dims_base = widget_base( top_base, /FRAME, /COLUMN, /BASE_ALIGN_CENTER )

    orientation_mode = widget_droplist( dims_base, VALUE=['Portrait','Landscape'] )
      widget_control, orientation_mode, SET_DROPLIST_SELECT=config.landscape_t
      
    size_base = widget_base( dims_base, /ROW, /ALIGN_LEFT)
      xsize_field = cw_field(size_base, TITLE='Max Width', /FLOATING, $
			      XSIZE=7, VALUE=config.xsize, /RETURN_EVENTS )
      ysize_field = cw_field(size_base, TITLE='Max Height', /FLOATING, $
			      XSIZE=7, VALUE=config.ysize, /RETURN_EVENTS )

    units_mode = widget_droplist( dims_base, VALUE=['Centimeters','Inches'] )
      widget_control, units_mode, SET_DROPLIST_SELECT=config.inches_t
      
    aspect_base = widget_base( dims_base, /ROW, /ALIGN_LEFT)
      aspect_ratio = cw_field(aspect_base, TITLE='Aspect Ratio', /FLOATING, $
			      XSIZE=8, VALUE=config.aspect_ratio, /RETURN_EVENTS )
      aspect_mode = widget_droplist( aspect_base, VALUE=['Fixed','Free'] )
      widget_control, aspect_mode, SET_DROPLIST_SELECT=config.aspect_mode

  bits_base = widget_base( top_base, /FRAME, /ROW)
    bits_buttons = cw_bgroup( bits_base, ['1','2','4','8'], $
			      /ROW, SPACE=1, XPAD=1, YPAD=1, $
			      /RETURN_INDEX, /EXCLUSIVE, $
			      LABEL_LEFT='Bits Per Pixel:' )
    b = config.bits_per_pixel 
    widget_control, bits_buttons, set_value=3*(b EQ 8) + 2*(b EQ 4) + 1*(b EQ 2)


  color_base = widget_base( top_base, /FRAME, /ROW)
    color_buttons = cw_bgroup( color_base, $
		['Grayscale (positive)','Grayscale (negative)','Color'], $
			        /COLUMN, SPACE=1, XPAD=1, YPAD=1, $
			        /RETURN_INDEX, /EXCLUSIVE, $
			        LABEL_LEFT='Colors:  ' )
    widget_control, color_buttons, set_value=config.color_mode


  selection_base = widget_base( top_base, /FRAME, /ROW)
      configure_button = widget_button(selection_base, $
				     VALUE="OK")
      cancel_button = widget_button(selection_base, VALUE="Cancel")

; Note that many of these widgets only generate events when a RETURN
; is pressed after editing.  We can't trust that a user does this, so when
; we need one of these values, we MUST read the fields directly.
state = { $ ;Widgets 
	  fileorlpr_buttons:fileorlpr_buttons, $
	  filename_button:filename_button, $
	  filename_field:filename_field, $
	  cancel_button:cancel_button, $
	  configure_button:configure_button, $
          preset1_button:preset1_button, $
          preset2_button:preset2_button, $
          preset3_button:preset3_button, $
	  orientation_mode:orientation_mode, $
	  units_mode:units_mode, $
	  bits_buttons:bits_buttons, $
	  color_buttons:color_buttons, $
	  aspect_ratio:aspect_ratio, $
	  aspect_mode:aspect_mode, $
	  xsize_field:xsize_field, $
	  ysize_field:ysize_field, $
	  
	  config:config_ptr }

;; Store the state structure in the user value field of the top-level widget.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

;; Realize the widget.
widget_control, top_base, /REALIZE

if keyword_set(batch_mode) then begin
  ; Skip interaction with the user
  event={ID:configure_button, TOP:top_base, HANDLER:top_base}
  PsDeviceEvent, event
endif   
  
;; Register with manager.
if (widget_info( top_base, /VALID_ID )) then $
  xmanager, 'Configure a PostScript Output Device', top_base, $
	  EVENT_HANDLER='PsDeviceEvent', /NO_BLOCK

config = temporary( *config_ptr )
ptr_free, config_ptr
if (config.printtoprinter) then filename = '' $
			   else filename = config.filename
success  = config.success
RETURN
END
;==========================================================================
