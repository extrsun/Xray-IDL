;+
;========================================================================
;;;
;;; FILE NAME:    $Id: choose_fits_extension.pro 683 1998-10-07 09:51:22Z tsk $
;;;
;;; DESCRIPTION:  choose_fits_extension()
;;;
;;;               A function which scans through a FITS file and extracts
;;;               the name of each FITS HDU.  If the HDU is not named, then
;;;               it attempts to extract the CCDID keyword.  A list is
;;;               compiled, and a modal dialog widget titled "Select a FITS
;;;               extension:" is presented to the caller.  The function returns
;;;               a zero-based index of the selected HDU.
;;;
;;; AUTHOR:       Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1998, Pennsylvania State University
;;;
;;; NOTES:
;;;               An annoying warning is printed to the caller's screen
;;;               when HeadFits() goes past the End Of File.  I see no
;;;               way to get rid of that.
;;;
;-
;==========================================================================

FUNCTION choose_fits_extension, filename

;;; Loop through the input file and extract all extension names.
extname = strarr(1)
numext = 0
done = 0
while ( NOT done) do begin
	header = HeadFits(filename,EXTEN=numext)

;;;	HeadFits() returns -1 when an error has occurred (like going past EOF)
	if ( n_elements(header) EQ 1) then begin
		done = 1
	endif else begin

;;;		If this is the primary extension, then no name is present.
;;;		Assign it a sensible name.
		if (numext EQ 0) then begin
			extname(0) = "Primary Array"
		endif else begin

;;;			Extract the name of the FITS HDU from the header.  If
;;;			one is not present, then extract the value of the
;;;                     CCDID keyword.  If that is not found, then assign the
;;;                     FITS HDU a sensible name.
			name = SXPAR( header, 'EXTNAME', COUNT=matches )
			if ( matches NE 0 ) then begin
				extname = [extname, "HDU NAME:  " + $
						    strtrim(name,2)]
			endif else begin
				name = SXPAR( header, 'CCDID', COUNT=matches )
				if ( matches NE 0 ) then begin
					extname = [extname, "CCDID:     " + $
						strtrim(string(name),2)]
				endif else begin
					extname = [extname, "Untitled HDU"]
				endelse
			endelse
		endelse
		numext = numext + 1
	endelse
endwhile

;;; Now we've compiled a list of FITS HDUs and have stored their names
;;; or CCDID keyword values.  Bring up a modal dialog which allows the
;;; user to choose which HDU she wants to see.  Oh yeah - don't prompt
;;; the user when there's only one HDU.

choice = 0
if ( numext GT 1 ) then begin
	desc = ['0, BUTTON, ' + extname(0) + ', LEFT, QUIT']
	for ii=1, numext-1 do begin
		desc = [desc, '0, BUTTON, ' + extname(ii) + ', LEFT, QUIT']
	endfor
	result = CW_FORM(desc, /COLUMN, TITLE='Select a FITS extension:')
	for ii=0, n_tags(result)-1 do begin
		if (result.(ii) EQ 1) then choice = ii
	endfor
endif

return, choice
END
