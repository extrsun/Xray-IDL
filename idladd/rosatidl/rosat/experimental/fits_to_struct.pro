;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
; NAME:
;       FITS_TO_STRUCT
;
; PURPOSE:
; 	Read data from extensions in a FITS file and store in a structure
;            variable
;
; CALLING SEQUENCE:
;	FITS_TO_STRUCT, file, HDR, STRUCT, exten=exten, strname=strname, 
;                       dir=dir
;
; INPUTS:
;	file   -  name of FITS file
;       exten  - extension of file to be read
;
; OPTIONAL INPUT PARAMETERS:
;	dir   -  directory where file is located (if not current directory)
;       strname - name of output structure. If not supplied, the value of
;                 the EXTNAME keyword in the header will be used.
;
; OUTPUTS:
;       hdr     -  FITS table header (string array)
;	struct -   IDL structure with info from file 
;
; NOTES:
;  A temporary .pro file is created to define structure in the default
;		directory, so writing privileges are required.
;
;  The previous problem with fields 27 & 28 in the SASS source list file
;     appears to be due to a disagreement in what the value of tnull should
;     be. FTGET sets tnull to '************** ' (14 *s followed by a blank), 
;     but the null values in fields 27 & 28 are '***************' (15 *s).
;     This has temporarily been fixed by a new version of ftget
;     in the experimental library.
;
; RESTRICTIONS:
;
; SUBROUTINES CALLED:
;  READFITS  (IDL Astronomical Users' Library)
;  ZPARCHECK       "                "
;  SXOPEN          "                "
;  SXHREAD         "                "
;  SXPAR           "                "
;  SXREAD          "                "
;  REMCHAR         "                "
;  GETTOK          "                "
;  CREATE_STRUCT
;  FITS_GET
;  FTGET    (IDL Astronomical Users' Library)
;  FTSIZE          "                "
;  FTINFO          "                "
;  ADSTRING        "                "
;  RADEC           "                "
;  NINT            "                "
;  TBGET           "                "
;  TBSIZE          "                "
;  TBINFO          "                "
;
; MODIFICATION HISTORY:
;  Version 1.0 RAS January 1992
;  Modified 05 Jun 1992 for Rosat IDL Library (GAR). 
;-
; ==================================================================
pro fits_to_struct,file,hdr,struct,exten=exten,dir=dir,strname=strname
;
; procedure to read Data in FITS extension and return IDL structure
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' FITS_TO_STRUCT, file, HDR, STRUCT, exten=exten, dir=dir (nullstring),'
  print,'                 strname=strname (default = EXTNAME in header)'
  retall
endif
if (n_elements(exten) eq 0) then begin
  print,' Table extension not defined. Returning.'
  retall
endif
if (n_elements(strname) eq 0) then strname = ''
if (n_elements(dir) eq 0) then dir = ''
;
tab = readfits(dir+file,hdr,ext=exten,/sil)        ;read the FITS file
;
check = 1
nfields = sxpar(hdr,'tfields')
if strtrim(sxpar(hdr,'XTENSION'),2) eq 'TABLE' then check = 0
if strtrim(sxpar(hdr,'XTENSION'),2) eq 'BINTABLE' then check = 0
if strtrim(sxpar(hdr,'XTENSION'),2) eq 'A3DTABLE' then check = 0
if (check ne 0) then begin
  print,'WARNING - Header is not for a FITS ASCII or Binary Table',/INF
  retall
endif
if (!ERR EQ -1) then begin
  print,'ERROR - FITS Header does not include required TFIELDS keyword'
  retall
endif
;
; search header for name of extension - suppiled in EXTNAME
;
if (strname eq '') then begin
  strname = strtrim(sxpar(hdr,'EXTNAME'),2)      ;find extension name
  remchar,strname,'.'                            ;remove bad characters
endif
if (strname eq '') then begin        ;check again to make sure
  print,' No value for EXTNAME given in header. You must supply a name'
  print,'    for the structure (strname). Returning.'
  retall
endif
;
tbcol = intarr(nfields)
tform = strarr(nfields) & tunit = tform & ttype =tform
tbcal = fltarr(nfields) + 1.
;
nhdr = n_elements(hdr)
again = 1
;
itype = min( where(strpos(hdr,'TTYPE') ge 0))     ;first TTYPE line
if (itype lt 0) then itype = nhdr
iform = min( where(strpos(hdr,'TFORM') ge 0))     ;first TFORM line
if (iform lt 0) then iform = nhdr
iunit = min( where(strpos(hdr,'TUNIT') ge 0))     ;first TUNIT line
if (iunit lt 0) then iunit = nhdr
icol  = min( where(strpos(hdr,'TCOL') ge 0))      ;first TBCOL line
if (icol lt 0) then icol = nhdr
ict = itype < iform < iunit < icol
;
while ( (again eq 1) and (ict lt nhdr) ) do begin
  hdrlin = strtrim(hdr(ict),2)
  remchar,hdrlin,'/'
  remchar,hdrlin,"'"
  try = gettok(hdrlin,'=')
  len = strlen(try)
  case strmid(try,0,5) of
    'TTYPE':  ttype(fix( strmid(try,5,len-5) ) - 1) = strtrim(sxpar(hdr,try),2)
    'TFORM':  tform(fix( strmid(try,5,len-5) ) - 1) = strtrim(sxpar(hdr,try),2)
    'TUNIT':  tunit(fix( strmid(try,5,len-5) ) - 1) = strtrim(sxpar(hdr,try),2)
    'TBCOL':  tbcol(fix( strmid(try,5,len-5) ) - 1) = fix(hdrlin)
    'END  ':  again = 0
    else:
  endcase
  ict = ict + 1
endwhile                      ;Done reading FITS header
;
; ------ put data into structures
; --- first define structure tag_names and format arrays
;
t_names = strarr(nfields)
t_formats = strarr(nfields)
t_units = strarr(nfields)
;
for i=0,nfields-1 do begin 
  xtype = ttype(i) 
  xunit = tunit(i) 
  xform = tform(i)
  remchar,xtype,"'" 
  remchar,xunit,"'" 
  remchar,xform,"'"
  t_names(i) = xtype
  t_formats(i) = xform
  t_units(i) = xunit
endfor
; 
; use fits_get on the first entry to get the number of values
;
dum = fits_get(hdr,tab,1)
nval = n_elements(dum)
create_struct,struct,strname,t_names,t_formats,dimen=nval,ch=0
;
; ---- now place values into structure vectors
;
struct.(0) = dum
for i=1,nfields-1 do struct.(i) = fits_get(hdr,tab,i+1)     ;now do rest
;
return
end         ;pro fits_to_struct
