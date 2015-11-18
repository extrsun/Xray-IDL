;+
;========================================================================
;;;
;;; FILE NAME:    $Id: fits_viewer.pro 1970 2004-02-29 19:30:14Z patb $
;;;
;;; DESCRIPTION:  Routine to view FITS images.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1994, Pennsylvania State University
;;;
;;; NOTES:        
;;;
;-
;==========================================================================
;;; Widget Event Handler Procedure
;==========================================================================
FUNCTION FitsViewerEvent, event

widget_control, /HOURGLASS
new_event   = 0

;; Get the state structure.
top_base = event.handler
widget_control, top_base, GET_UVALUE=st


;; Process the event.

DestroyFlag = 0
case event.ID of

;--------------------------------------------------------------------------
  (*st).choose_button: $
   begin
   ;; Select a file
   pathname = dialog_pickfile( GROUP=top_base, /MUST_EXIST, $
			       TITLE='Choose FITS file to view.',$
			       PATH=(*st).path, FILTER=(*st).filter )
   if (pathname NE '') then begin
     fdecomp, pathname, disk, dir, name, qual
     if ('' NE qual)  then name = name+ '.' +qual

     (*st).filter = '*.' + qual
     (*st).path   = dir

     ;; Select a FITS extension
     exten = choose_fits_extension(pathname)

     ;; Read data
     widget_control, /HOURGLASS

     if (*st).ds9 then begin
       spawn, 'xpaaccess ds9', status
       if (status[0] EQ 'no') then spawn, 'ds9 &'
     endif

     print, 'reading FITS file...'
     Image = readfits(pathname, Header, EXTEN_NO=exten)
     
     ;; Remove NaN's.
     index = where(0 EQ finite(Image), count)
     if (count GT 0) then Image[index] = 0


     if (size(Image, /N_DIM) NE 2) then begin
       print, 'NO IMAGE FOUND IN PRIMARY HDU!'
       image = indgen(10,10)
     endif

     widget_control, top_base, TLB_SET_TITLE=pathname

     expand_pixels = (*st).expand_pixels
     
     ;; Extract important keywords.
;     units = ' (' + strtrim( fxpar( header, 'BUNIT', COUNT=count ), 2 ) + ')'
;     if (count EQ 0) then units = ' (DN)'
;     ztit = 'Pixel Value' + units


     ;; Read the WCS keywords.
     wcs_object, xwcs, /INIT
     wcs_object, ywcs, /INIT     
     
     ctype1 = strtrim( fxpar( header, 'CTYPE1', COUNT=count ), 2 ) 
     if (count EQ 1) then wcs_object, xwcs, CTYP=ctype1 
     
     ctype2 = strtrim( fxpar( header, 'CTYPE2', COUNT=count ), 2 ) 
     if (count EQ 1) then wcs_object, ywcs, CTYP=ctype2 

     crpix1 = fxpar( header, 'CRPIX1', COUNT=count )
     if (count EQ 1) then wcs_object, xwcs, CRPX=crpix1

     crpix2 = fxpar( header, 'CRPIX2', COUNT=count )
     if (count EQ 1) then wcs_object, ywcs, CRPX=crpix2

     crval1 = fxpar( header, 'CRVAL1', COUNT=count )
     if (count EQ 1) then wcs_object, xwcs, CRVL=crval1

     crval2 = fxpar( header, 'CRVAL2', COUNT=count )
     if (count EQ 1) then wcs_object, ywcs, CRVL=crval2

     cdelt1 = fxpar( header, 'CDELT1', COUNT=count )
     if (count EQ 1) then wcs_object, xwcs, CDLT=cdelt1

     cdelt2 = fxpar( header, 'CDELT2', COUNT=count )
     if (count EQ 1) then wcs_object, ywcs, CDLT=cdelt2
     
     wcs_object, xwcs, /GET, CTYP=ctype1
     wcs_object, ywcs, /GET, CTYP=ctype2
     
     if ((ctype1 EQ 'RA---TAN') AND (ctype2 EQ 'DEC--TAN')) then begin
       ;; If a {RA---TAN, DEC--TAN} WCS system is found, then pass it on to 
       ;; function_2d via XWCS & YWCS
       function_2d, (*st).function_2d, Image, DATASET_NAME=name, X0=1, Y0=1,$
		  XWCS=xwcs, YWCS=ywcs, $
		  EXPAND_PIXELS=expand_pixels, $
		  XTITLE='FITS pixel number', YTITLE='FITS pixel number'
       
     endif else if ((ctype1 NE '') AND (ctype2 NE '')) then begin
       ;; If any other WCS system is found, then pass the assumed linear mapping
       ;; to function_2d via X0,Y0,DELTA_X,DELTA_Y so that the plot axis will
       ;; be labelled with the world coordinates.
       wcs_object, xwcs, /GET, CTYP=xtit, CRVL=crvl, CRPX=crpx, CDLT=delta_x
       x0 = crvl - (crpx-1)*delta_x

       wcs_object, ywcs, /GET, CTYP=ytit, CRVL=crvl, CRPX=crpx, CDLT=delta_y
       y0 = crvl - (crpx-1)*delta_y

       function_2d, (*st).function_2d, Image, DATASET_NAME=name, $
                  X0=x0, DELTA_X=delta_x, Y0=y0, DELTA_Y=delta_y, $
		  EXPAND_PIXELS=expand_pixels, $
		  XTITLE=xtit, YTITLE=ytit

     endif else begin
       ;; If there was no WCS, then display image with default labels.
       function_2d, (*st).function_2d, Image, DATASET_NAME=name, $
		  EXPAND_PIXELS=expand_pixels
     endelse

     if (widget_info( (*st).header, /VALID_ID )) then $
      widget_control, (*st).header, SET_VALUE=header


     if (*st).ds9 then begin
       xdim = fxpar(header, 'NAXIS1')
       ydim = fxpar(header, 'NAXIS2')
       
       ;; Show the image in ds9 too, with labels.
       region_file = filepath('ds9.' + getenv('USER'), /TMP)
       openw, lun, region_file, /GET_LUN
       printf, lun, "# Region file format: DS9 version 3.0"
       
       printf, lun, 0.5+xdim/2., 0.5+1.04*ydim, name, $
         	 F='(%"image;text(%f,%f) # color=black text={%s}")'

       if keyword_set(xtit) then $
         printf, lun, 0.5+xdim/2., 0.5-0.04*ydim, xtit, $
         	 F='(%"image;text(%f,%f) # color=black text={%s}")'

       if keyword_set(ytit) then $
         printf, lun, 0.5-0.04*xdim, 0.5+ydim/2., ytit, $
         	 F='(%"image;text(%f,%f) # color=black text={%s} textangle=90")'
       free_lun, lun
       file_chmod, region_file, /A_WRITE
       
       repeat begin
         spawn, 'xpaaccess ds9', status
         if (status[0] NE 'no') then break
         print, 'waiting for ds9 to come up...'
         wait,3
       endrep until (0)
  
       cmd1 = string(pathname, exten, F='(%"xpaset -p ds9 file \"%s[%d]\";")')
       cmd2 = string(region_file,     F='(%"xpaset -p ds9 regions load %s;")')
       cmd3 = 'xpaset -p ds9 zoom to fit;'
       cmd4 = 'xpaset -p ds9 zoom 0.9'
       spawn, cmd1+cmd2+cmd3+cmd4
     endif
   endif
   end

;--------------------------------------------------------------------------
  (*st).done_button: DestroyFlag = 1


;--------------------------------------------------------------------------
  ; Ignore events from function_2d
  (*st).function_2d: 

;--------------------------------------------------------------------------
 else: begin
       print, 'unknown event in fits_viewer'
       help,/ST,Event
       end
endcase


; DONE button
if (DestroyFlag) then begin
  widget_control, (*st).parent, /DESTROY
  if (*st).ds9 then spawn, 'xpaset -p ds9 exit'
endif

return, new_event
end


;==========================================================================
;;; Widget Creation Routine
;==========================================================================
PRO fits_viewer, BLOCK=block, EXPAND_PIXELS=expand_pixels, DS9=ds9


parent = plot_window_topbase(TITLE='Fits Image Viewer', $
			     XOFFSET=420, YOFFSET=0)

top_base = widget_base(parent, /BASE_ALIGN_RIGHT, /COLUMN, $
		       EVENT_FUNC='FitsViewerEvent', /SPACE, /XPAD, /YPAD)

; dum_base = widget_base( top_base )

 function_2d, function_2d, PARENT=top_base

 button_base = widget_base( top_base, /ROW, /SPACE, /XPAD, /YPAD )

 choose_button = widget_button( button_base, VALUE='Choose another FITS file' )

 done_button = widget_button( button_base, VALUE='Dismiss' )

header_base = widget_base(TITLE='Fits Header', GROUP=top_base, $
			   XOFFSET=0, YOFFSET=0, /COLUMN, /SPACE, /XPAD, /YPAD)

 header = widget_text( header_base, /SCROLL, XSIZE=80, YSIZE=20 )


state = { parent:parent, function_2d:function_2d, header:header, $
	  path:'./', filter:'*', $
	  expand_pixels:keyword_set(expand_pixels), ds9:keyword_set(ds9),$
	  choose_button:choose_button, done_button:done_button }

; Save state structure.
widget_control, top_base, SET_UVALUE=ptr_new(state, /NO_COPY)

widget_control, header_base, /REALIZE
widget_control, parent, /REALIZE

dum = FitsViewerEvent({ID:choose_button, TOP:top_base, HANDLER:top_base})


; The fancy combination of JUST_REG and NO_BLOCK is necessary to get the
; widget to behave when called by either widget applications or normal
; programs (which block on tty reads).
xmanager, 'fits_viewer', parent, EVENT_HANDLER='PlotWindowTopbaseEventFn', $
	  JUST_REG=keyword_set(block), NO_BLOCK=(keyword_set(block) EQ 0)
return
end

