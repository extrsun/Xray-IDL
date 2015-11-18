;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
; NAME:
;	RS_SOURCES
;
; PURPOSE:
; 	Read SASS output from sources file into IDL Version 2 Structure
;
; CALLING SEQUENCE:
;	RS_SOURCES, filename, HDR, STRUCT
;
; INPUTS:
;	filename -  Name of file (including directory). Assumes extension is
;                   '.fits' if not supplied.
;
; OUTPUTS:
;       hdr    -  FITS table header (string array)
;	struct -  IDL structure with info from file 
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
;  Now reads FITS file directly. Do NOT convert to pseudo SDAS format!
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
;  FTGET    (IDL Astronomical Users' Library)
;  FTSIZE          "                "
;  FTINFO          "                "
;  ADSTRING        "                "
;  RADEC           "                "
;  NINT            "                "
;
; MODIFICATION HISTORY:
;  Version 1.0 RAS January 1992
;  Modified 26 Feb 1992 for Rosat IDL Library (GAR). Combines rs_simbad
;    and previous rs_sources.
;  Modified 06 Jul 1992 to use READFITS and new version of MATCH_FILES;
;    and to read type of structure from header (GAR)
;  Modified 21 Apr 1993 to search for files temp_*.pro before trying to 
;    delete them
;  Modified 10 Sep 1993 (GAR) to work with new version of CREATE_STRUCT
;-
; ==================================================================
pro rs_sources,filename,hdr,struct
;
; procedure to read source information and return IDL structure
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RS_SOURCES, filename, HDR, STRUCT'
  retall
endif
;
fdecomp,filename,disk,dir,file,ext,ver
if (ext eq '') then ext = '.fits' else ext = '.' + ext
match_files,disk+dir,file,ext,name,nlist    ;look for the right files
name = name(0)                              ;use latest version
tab = readfits(name,hdr,ext=1)
;
extname = sxpar(hdr,'EXTNAME')
pos = strpos(extname,'_src')
if (pos(0) ge 0) then strname = 'sasslist' else begin
  pos = strpos(extname,'_sky')
  if (pos(0) ge 0) then strname = 'skylist'
endelse
if (pos(0) lt 0) then begin
  print,' Table is not a Rosat SASS _src or _sky table. Returning.'
  retall
endif
;
nfields = sxpar(hdr,'tfields')
if strtrim(sxpar(hdr,'XTENSION'),2) ne 'TABLE' then begin
  print,'WARNING - Header is not for a FITS Table',/INF
  retall
endif
if (!ERR EQ -1) then begin
  print,'ERROR - FITS Header does not include required TFIELDS keyword'
  retall
endif
;
tbcol = intarr(nfields)
tform = strarr(nfields) & tunit = tform & ttype =tform
tbcal = fltarr(nfields) + 1.
;
nhdr = n_elements(hdr)
again = 1
ict = min( where(strpos(hdr,'TTYPE') ge 0))             ;first TTYPE line
while ( (again eq 1) and (ict lt nhdr) ) do begin
  hdrlin = strtrim(hdr(ict),2)
  remchar,hdrlin,'/'
  remchar,hdrlin,"'"
  try = gettok(hdrlin,'=')
  len = strlen(try)
  case strmid(try,0,5) of
    'TTYPE':  ttype(fix( strmid(try,5,len-5) ) - 1) = strtrim(hdrlin,2)
    'TFORM':  begin
              ipos = strpos(hdrlin,'%')
              if (ipos ge 0) then hdrlin = strmid(hdrlin,0,ipos)
              tform(fix( strmid(try,5,len-5) ) - 1) = strtrim(hdrlin,2)
              end
    'TUNIT':  tunit(fix( strmid(try,5,len-5) ) - 1) = strtrim(hdrlin,2)
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
t_names = [t_names,'radec']      ;add field for RA in hms, DEC in dms
t_formats = [t_formats,'a']           
; 
sz = size(tab)
nval = sz(2)
create_struct,row,strname,t_names,t_formats,dimen=1,ch=0
struct = replicate(row,nval)
;
; ---- now place values into structure vectors
;
for i=0,nfields-1 do struct.(i) = ftget(hdr,tab,i+1)
;
; now change ra and dec in degrees to ra in hr, m, s & dec in deg, m, s
;
ra = struct.ra
dec = struct.dec
radec = strarr(nval)
for ii=0,nval-1 do radec(ii) = adstring(ra(ii),dec(ii))
struct.radec = radec
;
; Now remove (Unix) or delete (VMS) temporary .pro files
;
tempfil = 'temp_'+strname+'.pro'
if (!version.os eq 'vms') then tempfil = tempfil+';*' 
  names = findfile(tempfil,count=nlist)
if (nlist gt 0) then begin
  if (!version.os eq 'vms') then delstr = 'delete '+tempfil $
    else delstr = 'rm '+tempfil
  spawn,delstr
endif
;
return
end         ;pro rs_sources

