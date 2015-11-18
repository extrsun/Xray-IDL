function tbget,h,tab,field,rows,nulls,CONTINUE=continue
;+
; NAME:
;   TBGET 
; PURPOSE:
;    Function to return value(s) from specified column in a binary
;    FITS table
; CALLING SEQUENCE
;	values = tbget( h, tab, field, [ rows, nulls ] )
; INPUTS:
;	h - Binary FITS header returned by TBREAD
;	tab - Binary FITS table array returned by TBREAD
;	field - field name or number
; OPTIONAL INPUTS:
;	rows -  scalar or vector giving row number(s)
;		Row numbers start at 0.  If not supplied or set to
;		-1 then values for all rows are returned
; OUTPUTS:
;	the values for the row are returned as the function value.
;	Null values are set to 0 or blanks for strings.
; OPTIONAL OUTPUT:
;	nulls - null value flag of same length as the returned data.
;		It is set to 1 at null value positions and 0 elsewhere.
;		If supplied then the optional input, rows, must also
;		be supplied.
; HISTORY:
;       Written  W. Landsman        February, 1991
;-
;------------------------------------------------------------------
On_error,2
common tb_common,tbcol,width,idltype,numval,tunit,tnull,tform,ttype

if N_params() LT 3 then begin
    print, 'Syntax -  values = tbget( h, tab, field, [ rows, nulls ] )'
    return, -1
endif

!ERR = 0 			;no error yet
;
; get size of table
;
tbsize,h,tab,ncols,nrows,tfields
;
; get characteristics of specified field
;
if (not keyword_set(CONTINUE)) or (N_elements(tbcol) eq 0) then $
    tbinfo,h,tbcol,width,idltype,numval,tunit,tnull,tform,ttype
;
case datatype(field) of
'STR': begin
      i = where( ttype EQ strupcase(field), Nfound)
      if Nfound EQ 0 then $ 
         message,'Field '+strupcase(field) + ' not found in header'
      end
'UND':message,'First parameter must be field name or number'
ELSE: begin
      i = field-1
      if (i LT 0 ) or (i GT tfields) then $
            message,'Field number must be between 1 and ' +strtrim(tfields,2)
      end
endcase
i = i(0)
;
; if rows not supplied then return all rows
;
if N_params() lt 4 then rows=-1
;
; determine if scalar supplied
;
row = rows
s =size(row) & ndim = s(0)   
if ndim EQ 0 then begin		;scalar?
        if row LT 0 then begin	; -1 get all rows
		ndim = 1
		row = lindgen(nrows)
	   end else begin
		row = lonarr(1)+row
	end
end
nrow = N_elements(row)
;
; check for valid row numbers
;
if (min(row) LT 0) or (max(row) GT (nrows-1)) then $
	message,'Row numbers must be between 0 and '+ (nrows-1)

;
; get column
;
if ndim EQ 0 then begin					;scalar?
	d = tab(tbcol(i):tbcol(i)+numval(i)*width(i)-1,row(0))
    end else begin					;vector
	d = tab(tbcol(i):tbcol(i)+numval(i)*width(i)-1,row)
end
n = N_elements(d)
;
; convert data to the correct type
;
case idltype(i) of
1:  begin
    temp = byte(d,0,numval(i),nrow)
    if tform(i) EQ 'L' then begin
       d = strarr(numval(i),nrow)
       for j = 0,numval(i)*nrow-1 do d(j) = string(temp(j))
    endif
    end
2:  begin
    byteorder,d,/NtoHS
    d = fix(d,0,numval(i),nrow)
    end
3:  begin
    byteorder,d,/NtoHL
    d = long(d,0,numval(i),nrow)
    end
4:  begin
    d = float(d,0,numval(i),nrow)
    ieee_to_host, d
    end
5:  begin
    d = double(d,0,numval(i),nrow)
    ieee_to_host, d
    end
endcase
;
; extract correct rows if vector supplied
;
return, reform(d)
end
