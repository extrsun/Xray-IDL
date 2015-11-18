pro tbinfo,h,tbcol,width,idltype,numval,tunit,tnull,tform,ttype
;+
; NAME:
;	TBINFO
; PURPOSE:
;	Procedure to return information on the specified field 
;	in a FITS binary table.
;
; CALLING SEQUENCE:
;	tbinfo, h, tbcol, width, idltype, numval, tunit, tnull, tform, ttype
; INPUTS:
;	h - FITS table header
;
; OUTPUTS:
;	tbcol - starting column position in bytes
;	width - width of the field in bytes
;	idltype - idltype of field.
;		7 - string, 4- real*4, 3-integer*4, 5-real*8
;	tunit - string unit numbers
;	tnull - null value for the field
;	tform - format for the field
;	ttype - field name
;
; SIDE EFFECTS:
;	If there are difficulties interpreting the table then !ERR is set 
;	to -1
; HISTORY:
;	D. Lindler  July, 1987
;	W. Thompson, Feb. 1992, added support for 'B', 'C', and 'M' formats.
;-
;----------------------------------------------------------------------------
 On_error,2

; get number of fields

 tfields = sxpar( h, 'TFIELDS')
 if !err lt 0 then $
	message,'Invalid FITS header. keyword TFIELDS is missing'

 if tfields eq 0 then begin	;ANY fields in table
		!ERR = -1
		return
 endif

 idltype = intarr(tfields) & numval = lonarr(tfields) & tbcol = lonarr(tfields)
 width = intarr(tfields)  
 ttype = sxpar(h,'TTYPE*')

 if !ERR LT 0 then $
	message,'Invalid FITS table header -- keyword TTYPE not present'
 if !ERR NE Tfields then $
          message,'Warning - Number of fields ('+strtrim(!ERR,2) + $ 
         ') does not equal TFIELDS (' + strtrim(tfields,2) + ')',/CON

 ttype = strtrim(ttype,2)

 tform = sxpar(h,'tform*')			; column format
 if !ERR lt 0 then $
	message,'Invalid FITS table header -- keyword TFORM not present'

 tform =  strupcase(strtrim(tform,2))
;						; physical units
 tunit = sxpar(h,'tunit*')
 if !ERR LT 0 then tunit = ''

 tnull = sxpar(h,'tnull*')			;null data value
 if !err lt 0 then tnull = ''

; determine idl data type from format

 len = strlen(tform)
 for i = 0,n_elements(tform)-1 do begin
	numval(i) = strmid(tform(i),0,len(i)-1)
 	tform(i) =  strmid(tform(i),len(i)-1,1)
	case strupcase(tform(i)) of
	'A' : begin 
              idltype(i) = 7 & width(i) = 1
              end
        'I' : begin
              idltype(i) = 2 & width(i) = 2
              end
	'J' : begin
              idltype(i) = 3 & width(i) = 4
              end
	'E' : begin
              idltype(i) = 4 & width(i) = 4
              end
	'D' : begin
              idltype(i) = 5 & width(i) = 8
              end
        'L' : begin
              idltype(i) = 1 & width(i) = 1
              end
	'B' : begin
	      idltype(i) = 1 & width(i) = 1
	      end
	'C' : begin
	      idltype(i) = 6 & width(i) = 8
	      end

;  Treat bit arrays as byte arrays with 1/8 the number of elements.

        'X' : begin
              idltype(i) = 1
	      numval(i) = long((numval(i)+7)/8)
	      width(i) = 1
              end

;  Treat double-complex arrays as double-precision arrays with twice the number
;  of elements.

	'M' : begin
	      idltype(i) = 5
	      numval(i) = numval(i)*2
	      width(i) = 8
	      end
	else: message,'Invalid format specification for keyword ' + $
			'TFORM'+ strtrim(i+1,2)
 end

 if i ge 1 then tbcol(i) = tbcol(i-1) + width(i-1)*numval(i-1)

 endfor

 return
 end
