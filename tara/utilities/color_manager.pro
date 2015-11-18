;+
;========================================================================
;;;
;;; FILE NAME:    $Id: color_manager.pro 4412 2012-12-24 20:32:10Z psb6 $
;;;
;;; DESCRIPTION:  Utility for managing the color table so that named colors
;;;               can co-exist with a set of colors used to scale images.
;;;
;;;		  This routine should REPLACE all calls to SET_PLOT & DEVICE
;;;		  to allow us to conveniently deal with the fact that color
;;;		  PostScript uses the color table maintained by this routine
;;;		  (which may have less than 256 entries) while B&W PostScript 
;;;		  works with a full 256 grey levels.  This routine keeps
;;;		  track of the current device and the color status of the
;;;		  PS device and returns the correct value of NCOLORS.
;;;
;;;               The physical color table is divided into two sections. 
;;;               The lower section contains the set of colors used to
;;;               scale images -- this section can be modified by the user
;;;               with LOADCT/XLOADCT.  The upper section always contains
;;;               the colors RED, GREEN, BLUE, WHITE, & BLACK for use in
;;;               drawing plots & annotations.
;;;
;;;		  Any extra keywords are passed onto the device procedure.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        USAGE EXAMPLE:
;;;
;;;               load a color table into the lower section
;;;               color_manager, /X_PSEUDO, NCOLORS=n_pseudo
;;;               xloadct, NCOLORS=n_pseudo
;;;
;;;               scale & display an image
;;;               color_manager, NCOLORS=n_pseudo
;;;               tv, bytscl( image, TOP=n_pseudo-1 )

;;;               annotate the image in RED
;;;               color_manager, RED=red
;;;               xyouts, .5, .5, /NORMAL, 'WOW!', COLOR=red
;;;
;;;		  set device to PS and set some device options
;;;		  color_manager, /PS_TRUE, BITS_PER_PIXEL=8
;-
;==========================================================================
PRO color_manager, color_name, color_index, X_PSEUDO=x_pseudo, X_TRUE=x_true, $
		   PS_GREY=ps_grey, PS_NEG_GREY=ps_neg_grey, $
		   PS_PSEUDO=ps_pseudo, PS_TRUE=ps_true, $
		   NUM_RESERVED=num_reserved, $
		   
		   UTILITY_DIR=utility_dir, DECOMPOSED=decomposed, $
		   NEGATIVE_IMAGES=negative_images, NCOLORS=ncolors, $
		   COLOR_NAMES=color_names, $
                   RED=red, GREEN=green, BLUE=blue, WHITE=white, BLACK=black,$
                   STATUS=status, $
		   _EXTRA=extra
		   
COMMON color_manager, util_dir_name, $
        x_server_connected, x_is_decomposed, $
	n_reserve, reserved_colors, i_black, i_white, i_red, i_green, i_blue,$
	r_pseudo, g_pseudo, b_pseudo,$
 	xpseudo, xtrue, psgrey, psneggrey, pspseudo, pstrue, model
	
creator_string = "TARA color manager, version"+strmid("$Date: 2012-12-24 15:32:10 -0500 (Mon, 24 Dec 2012) $", 6, 11)

status = 0

;; Do some stuff once per IDL session.
if (n_elements(model) EQ 0) then begin
; print, 'Initializing the '+creator_string
  ; Locate the utilities directory.
  result = routine_info( 'color_manager', /SOURCE )
  fdecomp, result.PATH, disk, util_dir_name, name, qual

  ; Create and destroy a window to get the X-server to allocate a color table.
  set_plot, 'X'
  !P.FONT = -1   ; "vector drawn" font
  device, TRUE_COLOR=24, DECOMPOSED=1
  win_num = !D.WINDOW
  
  x_server_connected = 1
  catch, status
  if (status NE 0) then begin
    print, !ERROR_STATE.MSG
    print, 'color_manager: Could not open X window.'
    x_server_connected = 0
    x_is_decomposed    = 0
  endif else window, /FREE, /PIX, XSIZE=20, YSIZE=20
  catch, /CANCEL
  
  if x_server_connected then begin
    ; Since the Ultra has such small pixels, let's set the font larger.
    device,font='8x13'
  
    ; See if the X display uses decomposed color.
    device, GET_DECOMPOSED=x_is_decomposed
  
    wdelete
    if (win_num NE -1) then begin
      wset, win_num
      print, 'WARNING! Since the IDL session had already created a graphics window when'
      print, 'the first TARA tool was run, there may be color problems in the TARA tools.'
    endif
  endif
  
  ; Establish user and reserve color tables.
  ; We need white at the top of the table because !P.COLOR defaults to the
  ; top of the table.
  rc = replicate({name:'', r:0, g:0, b:0}, 10)
  
  rc.name = ['gray','yellow','light blue','purple','orange','blue','green','red','black','white']
  rc.r    = [   90,     217,           0,     255,     255,    30,      0,  255,      0,   255 ]
  rc.g    = [   90,     166,         255,       0,     128,   144,    255,    0,      0,   255 ]
  rc.b    = [   90,      33,         255,     255,       0,   255,      0,    0,      0,   255 ]
  
  if (NOT keyword_set(num_reserved)) then num_reserved = n_elements(rc)
  n_reserve = 5 > num_reserved < n_elements(rc)
  
  reserved_colors = rc[n_elements(rc) - n_reserve:*]
  print, 'Named colors are: ', ['white', reverse((reserved_colors.name)[0:n_reserve-3]), 'black']

  
  i_white=n_reserve-1
  i_black=n_reserve-2
  i_red  =n_reserve-3
  i_green=n_reserve-4
  i_blue =n_reserve-5  

  ;; Create the color models.
  ; The pseudo-color X visual has colors at the bottom of the table
  ; loaded by the user, plus n_reserve colors at the top of the table. 
  ncolors  = !D.TABLE_SIZE - n_reserve
  if (ncolors LT 2) then message, 'NOT ENOUGH COLORS AVAILABLE TO RUN IDL!!'
  r_pseudo = (lindgen(ncolors) * 255) / (ncolors-1)
  g_pseudo = r_pseudo
  b_pseudo = r_pseudo
  
  xpseudo = ptr_new({ ncolors:ncolors, negative_images:0, decomposed:0, $
  		      reserved_indexes: ncolors + indgen(n_reserve) })
  
  ; When the true color model is requested we have to handle 8-bit and 
  ; 24-bit hardware in different ways.
  if (x_is_decomposed) then begin
    ; In the TrueColor X visual, color "indexes" are actually 
    ; 24bit RGB triplets.
    index24bit =  reserved_colors.r + 256L * $
		 (reserved_colors.g + 256L * reserved_colors.b)

    xtrue = ptr_new({ ncolors:256, negative_images:0, decomposed:1, $
  		      reserved_indexes: index24bit })
;   print, 'TrueColor X visual is available'

    ; Don't have a record of why this was needed ...
;    window, 31, TITLE='DO NOT DESTROY', XSIZE=200, YSIZE=10, XPOS=500, YPOS=30
  endif else begin
    ; If X_TRUE is requested, but the hardware does not support decomposed
    ; color, then we run a model just like xpseudo.
    xtrue = xpseudo
    print, 'TrueColor X visual is NOT available'
  endelse
  
  ; For monochrome PostScript all colors except black get index 0 which makes
  ; a black line on paper.  The color "black" gets index 255 which puts no  
  ; ink on the paper.
  ; PS_GREY & PS_NEG_GREY differ only in the flag negative_images.
  reserved_indexes          = replicate(0,n_reserve)
  reserved_indexes[i_black] = 255
  psgrey    = ptr_new({ ncolors:256, negative_images:0, decomposed:0, $
  		        reserved_indexes: reserved_indexes })
  
  psneggrey = ptr_new( *psgrey )
  (*psneggrey).negative_images = 1
  
  ; The pseudo-color PostScript is just like the xpseudo model, except that
  ; black & white have to be reversed since paper is white.
  pspseudo  = ptr_new( *xpseudo )

  temp                                  = (*pspseudo).reserved_indexes[i_white]
  (*pspseudo).reserved_indexes[i_white] = (*pspseudo).reserved_indexes[i_black]
  (*pspseudo).reserved_indexes[i_black] = temp
  
  ; The PS_TRUE model puts ramps in the bottom section of the table, and 
  ; puts the reserved colors at the top.
  ; Again, black & white have to be reversed since paper is white.
  ncolors   = 256 - n_reserve
  pstrue    = ptr_new({ ncolors:ncolors, negative_images:0, decomposed:0, $
  		        reserved_indexes: ncolors + indgen(n_reserve) })

  temp                                = (*pstrue).reserved_indexes[i_white]
  (*pstrue).reserved_indexes[i_white] = (*pstrue).reserved_indexes[i_black]
  (*pstrue).reserved_indexes[i_black] = temp
  
  
  ;; If no model requested initialize us to the X_PSEUDO model.
  if ~(keyword_set(x_pseudo)     || keyword_set(x_true)      || $
	  keyword_set(ps_grey)   || keyword_set(ps_neg_grey) || $
	  keyword_set(ps_pseudo) || keyword_set(ps_true)) then begin $
    x_pseudo  = x_server_connected
    ps_pseudo = ~x_pseudo
  endif

  ;; We have to set "model" var to something to keep code below happy.
  model = xpseudo
endif

;-------------------------------------------------------------------------

;; Handle model changes.
;; We could save time by skipping model change code if the current model is
;; already the one requested, but we'll go ahead and execute change anyway
;; as a recovery mechanism in case user or other program messes with device
;; or color tables.

if keyword_set(x_pseudo) && x_server_connected then begin
  ; If current model is X_PSEUDO the user may have modified color table
  ; so we should save it before writing to it.
  if (model EQ xpseudo) then tvlct, r_pseudo, g_pseudo, b_pseudo, /GET

  model = xpseudo
  set_plot, 'x'
  !P.FONT = -1   ; "vector drawn" font
  device, DECOMPOSED=0
  ; Load both user and reserve sections of the table.
  tvlct, r_pseudo,          g_pseudo,          b_pseudo,          0
  tvlct, reserved_colors.r, reserved_colors.g, reserved_colors.b, $
  	 (*model).ncolors

endif else if keyword_set(x_true) && x_server_connected then begin
  ; If current model is X_PSEUDO the user may have modified color table
  ; so we should save it before writing to it.
  if (model EQ xpseudo) then tvlct, r_pseudo, g_pseudo, b_pseudo, /GET

  model = xtrue
  set_plot, 'x'
  !P.FONT = -1   ; "vector drawn" font
  if (x_is_decomposed) then begin
    ; Load a full ramp; named colors are specified directly as long ints.
    device, DECOMPOSED=1
    ramp = bindgen(256)
    tvlct, ramp, ramp, ramp, 0
  endif else begin
    ; We have 8-bit hardware and will be using color_quan routine to fill
    ; in user section of the table.  Just load reserve section here.
    tvlct, reserved_colors.r, reserved_colors.g, reserved_colors.b, $
  	   (*model).ncolors
  endelse

endif else if keyword_set(ps_grey) then begin
  ; If current model is X_PSEUDO the user may have modified color table
  ; so we should save it before writing to it.
  if (model EQ xpseudo) then tvlct, r_pseudo, g_pseudo, b_pseudo, /GET

  model = psgrey
  set_plot, 'ps'
  !P.FONT = 0   ; "hardware" font 
  device, COLOR=0
  ; Mono PS device doesn't use tables so don't bother loading any. 

endif else if keyword_set(ps_neg_grey) then begin
  ; If current model is X_PSEUDO the user may have modified color table
  ; so we should save it before writing to it.
  if (model EQ xpseudo) then tvlct, r_pseudo, g_pseudo, b_pseudo, /GET

  model = psneggrey
  set_plot, 'ps'
  !P.FONT = 0   ; "hardware" font 
  device, COLOR=0
  ; Mono PS device doesn't use tables so don't bother loading any. 

endif else if keyword_set(ps_pseudo) then begin
  ; If current model is X_PSEUDO the user may have modified color table
  ; so we should save it before writing to it.
  if (model EQ xpseudo) then tvlct, r_pseudo, g_pseudo, b_pseudo, /GET

  model = pspseudo
  set_plot, 'ps'
  !P.FONT = 0   ; "hardware" font 
  device, COLOR=1
  ; Load both user and reserve sections of the table.
  tvlct, r_pseudo,          g_pseudo,          b_pseudo,          0
  tvlct, reserved_colors.r, reserved_colors.g, reserved_colors.b, $
  	 (*model).ncolors

endif else if keyword_set(ps_true) then begin
  ; If current model is X_PSEUDO the user may have modified color table
  ; so we should save it before writing to it.
  if (model EQ xpseudo) then tvlct, r_pseudo, g_pseudo, b_pseudo, /GET

  model = pstrue
  set_plot, 'ps'
  !P.FONT = 0   ; "hardware" font 
  device, COLOR=1, BITS_PER_PIXEL=8
  ; Load a partial ramp and the reserve table.
  ncolors = (*model).ncolors
  ramp = (lindgen(ncolors) * 255) / (ncolors-1)
  tvlct, ramp, ramp, ramp, 0
  tvlct, reserved_colors.r, reserved_colors.g, reserved_colors.b, ncolors
endif


utility_dir = util_dir_name

;; Handle any DEVICE keywords passed in _EXTRA.
if (0 NE n_elements(extra)) then device, _STRICT_EXTRA=extra
  
ncolors         = (*model).ncolors
negative_images = (*model).negative_images
decomposed      = (*model).decomposed

; Coyote Graphics requires color indexes to be INT, not LONG.
if arg_present(red)     then red   = fix(((*model).reserved_indexes)[i_red]  )
if arg_present(green)   then green = fix(((*model).reserved_indexes)[i_green])
if arg_present(blue)    then blue  = fix(((*model).reserved_indexes)[i_blue] )
if arg_present(white)   then white = fix(((*model).reserved_indexes)[i_white])
if arg_present(black)   then black = fix(((*model).reserved_indexes)[i_black])

if arg_present(color_names) then begin
  ;; The list of color names we report to clients should, for their 
  ;; convenience, begin with 'white'.  We include 'black' so that axes
  ;; can be made to disappear.
  
  color_names = ['white', reverse((reserved_colors.name)[0:n_reserve-3]), 'black']
endif
if keyword_set(color_name) then begin
  index = where(reserved_colors.name EQ color_name, count)
  if (count NE 1) then begin
    print, 'Invalid color name: ' + color_name + '; substituting white.'
    index = [i_white]
  endif
  
  ; Coyote Graphics requires color indexes to be INT, not LONG.
  color_index = fix( ((*model).reserved_indexes)[index[0]] )
endif

return
end

