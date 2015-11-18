;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
; NAME:
;   	TAB_TO_STRUCT
;
; PURPOSE:
;   	Procedure to get info into structure from SDAS binary table 
;       (e.g., like the tables created by IRAF &/or PROS)
;
; CALLING SEQUENCE:
;   	TAB_TO_STRUCT, file, HDR, TSTRUCT, dir=dir
;
; INPUTS:
;   	FILE - root file name: file + '.tab' assumed
;
; OPTIONAL INPUTS:
;       DIR - directory name of table file
;
; OUTPUTS:
;	HDR	=	IDL string array with IRAF style hdr info 
;                       from table
;	TSTRUCT -	IDL structure with table info
;
;
; PROCEDURE:
;  Uses IDL Astronomy Library routines to read in the PROS file
;    information, uses CREATE_STRUCT to define the IDL structure
;    by writing and executing a temprorary '.pro' file.
; RESTRICTIONS:
;  May have trouble with tables having unusual column names:
;     e.g., "RA', 'DEC', 'MAG' will work, but
;     '0.22' will not because it will be an invalid IDL structure tag name
;  Also will bomb if a DIFFERENT structure of the same name has defined 
;     previously.
;
; SUBROUTINES CALLED:
;  TAB_COL   (IDL Astronomical Users' Library)
;  CREATE_STRUCT
;
; MODIFICATION HISTORY:
;	version 1  R. Stern Dec 1991
;       Modified 26 Feb 1992 for Rosat IDL Library (GAR)
;       Modified 18 Sep 1992 to call newer version of CREATE_STRUCT and to
;         work more like FITS_TO_STRUCT (GAR)
;         Name of procedure also changed from TAB_TO_STRUC
;-
;----------------------------------------------------------------------------
pro tab_to_struct,file,hdr,tstruct,dir=dir
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' TAB_TO_STRUCT, file, HDR, TSTRUCT, dir=dir'
  retall
endif
if not keyword_set(dir) then dir = ''	
;	
fdecomp,file,disk,dir2,name,ext,ver
if (ext eq '') then ext = '.tab' else ext = '.'+ext
if (disk ne '') then dir = disk+dir2            ;if on different disk
tab_read,dir+name+ext,tcb,tab,hdr	        ;read table
tab_size,tcb,nrows,ncols		        ;get its size
;
; --- below are outputs from "TAB_COL" procedure
;
;       offset - column offset bytes
;        width - column width in bytes
;        datatype - column data type:
;                6 - real*4
;                7 - real*8
;                4 - integer*4
;                1 - boolean
;                2 - character string
;        name - column name
;        units - column units
;        format - format code
;
t_names = strarr(ncols)
t_formats = strarr (ncols)
for i=1,ncols do begin
  tab_col, tcb, i, off, width, dtype, colname, units, format
  t_names(i-1) = colname
  case dtype of
    1: t_formats(i-1) = 'A'
    2: t_formats(i-1) = 'A'
    4: t_formats(i-1) = 'I'
    6: if strpos(format,'f') ne -1 then t_formats(i-1) = 'F' else $
       t_formats(i-1) = 'E'
    7: if strpos(format,'f') ne -1 then t_formats(i-1) = 'F' else $
       t_formats(i-1) = 'E'
  else: stop, 'Invalid format'
  endcase
endfor
ttype = strmid (file, strlen(file)-5, 5)	; allow five chars	
mpos = strpos (ttype,'_')
;
tnew = strmid(ttype,mpos+1, strlen(ttype)-(mpos+1) )
;create_struct,struct,tnew+'_table',tnew+'_tab',t_names,t_formats,ch=0
strname = tnew+'_tab'
create_struct,struct,strname,t_names,t_formats,dimen=1,ch=0
; 
; ---- now put values into structure
;
case nrows of
  1:  begin
      tstruct=struct
      for i=0,ncols-1 do tstruct(0).(i) = tab_val(tcb,tab,i+1,0)
      end
;			
  else: begin
        tstruct = replicate (struct, nrows) 
        for i=0,ncols-1 do tstruct.(i) = tab_val(tcb,tab,i+1)
	end
endcase
;
return
end        ;pro tab_to_struct
