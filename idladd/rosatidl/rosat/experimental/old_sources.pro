;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
; NAME:
;	OLD_SOURCES
;
; PURPOSE:
; 	Read SASS output from sources file into IDL Version 2 Structure
;
; CALLING SEQUENCE:
;	OLD_SOURCES, filename, HDR, STRUCT
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
;  Writing privileges are not required.
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
;  modified 24 Dec 1993 (GAR): name changed from RS_SOURCES to OLD_SOURCES
;    (to allow RS_SOURCES to read RDF format files as well as US format)
;  modified 27 Dec 1993 (GAR): code calling create_struct replaced with
;    explicit structure definitions for sasslist & skylist. 
;    The temporary .pro file is no longer created, so writing privileges
;    are no longer required.
;-
; ==================================================================
pro old_sources,filename,hdr,struct
;
; procedure to read source information and return IDL structure
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' OLD_SOURCES, filename, HDR, STRUCT'
  retall
endif
;
fdecomp,filename,disk,dir,file,ext,ver
if (ext eq '') then ext = '.fits' else ext = '.' + ext
match_files,disk+dir,file,ext,name,nlist    ;look for the right files
name = name(0)                              ;use latest version
tab = readfits(name,hdr,ext=1)
ndat = fxpar(hdr,'NAXIS2')                   ;number of rows (data points)
;
extname = sxpar(hdr,'EXTNAME')
pos = strpos(extname,'_src')
if (pos(0) ge 0) then datyp = 'src' else begin
  pos = strpos(extname,'_sky')
  if (pos(0) ge 0) then datyp = 'sky'
endelse
if (pos(0) lt 0) then begin
  print,' Table is not a Rosat SASS _src or _sky table. Returning.'
  retall
endif
;
case datyp of
  'src':  begin
          row = {sasslist,src:0,mplsx_src:0,ra:0.0,dec:0.0,x:0.0,y:0.0, $
                 im_x:0.0,im_y:0.0,pix_err:0.0,offax:0.0,ccts:0.0, $ 
                 ccts_err:0.0,cbkg:0.0,x_size:0.0,y_size:0.0,ept:0.0, $
                 psn0:0.0,sdist:0.0,rdist:0.0,rec:0.0,ext_like:0.0, $
                 vary:'',hr_1:0.0,he_1:0.0,hr_2:0.0,he_2:0.0,priority:'', $
                 chi_pl:0.0,chi_rs:0.0,mdet:'',radec:''}
          struct = replicate(row,ndat)
          struct.src = fits_get(hdr,tab,'src')
          struct.mplsx_src = fits_get(hdr,tab,'mplsx_src')
          struct.ra = fits_get(hdr,tab,'ra')
          struct.dec = fits_get(hdr,tab,'dec')
          struct.x = fits_get(hdr,tab,'x')
          struct.y = fits_get(hdr,tab,'y')
          struct.im_x = fits_get(hdr,tab,'im_x')
          struct.im_y = fits_get(hdr,tab,'im_y')
          struct.pix_err = fits_get(hdr,tab,'pix_err')
          struct.offax = fits_get(hdr,tab,'offax')
          struct.ccts = fits_get(hdr,tab,'ccts')
          struct.ccts_err = fits_get(hdr,tab,'ccts_err')
          struct.cbkg = fits_get(hdr,tab,'cbkg')
          struct.x_size = fits_get(hdr,tab,'x_size')
          struct.y_size = fits_get(hdr,tab,'y_size')
          struct.ept = fits_get(hdr,tab,'ept')
          struct.psn0 = fits_get(hdr,tab,'psn0')
          struct.sdist = fits_get(hdr,tab,'sdist')
          struct.rdist = fits_get(hdr,tab,'rdist')
          struct.rec = fits_get(hdr,tab,'rec')
          struct.ext_like = fits_get(hdr,tab,'ext_like')
          struct.vary = fits_get(hdr,tab,'vary')
          struct.hr_1 = fits_get(hdr,tab,'hr_1')
          struct.he_1 = fits_get(hdr,tab,'he_1')
          struct.hr_2 = fits_get(hdr,tab,'hr_2')
          struct.he_2 = fits_get(hdr,tab,'he_2')
          struct.priority = fits_get(hdr,tab,'priority')
          struct.chi_pl = fits_get(hdr,tab,'chi_pl')
          struct.chi_rs = fits_get(hdr,tab,'chi_rs')
          struct.mdet = fits_get(hdr,tab,'mdet')
          end
;
  'sky':  begin
          row = {skylist,src:0,ra:0.0,dec:0.0,id:0,obj:'',acc:'',vmag:'', $
                 bmag:'',spmor:'',ruxvn:'',ref:0,dist:'',radec:''}
          struct = replicate(row,ndat)
          struct.src = fits_get(hdr,tab,'SRC')
          struct.ra = fits_get(hdr,tab,'RA')
          struct.dec = fits_get(hdr,tab,'DEC')
          struct.id = fits_get(hdr,tab,'ID')
          struct.obj = fits_get(hdr,tab,'OBJ')
          struct.acc = fits_get(hdr,tab,'ACC')
          struct.vmag = fits_get(hdr,tab,'VMAG')
          struct.bmag = fits_get(hdr,tab,'BMAG')
          struct.spmor = fits_get(hdr,tab,'SPMOR')
          struct.ruxvn = fits_get(hdr,tab,'RUXVN')
          struct.ref = fits_get(hdr,tab,'REF')
          struct.dist = fits_get(hdr,tab,'DIST')
          end
endcase
;
; Now change ra and dec in degrees to ra in hr, m, s & dec in deg, m, s
;
ra = struct.ra
dec = struct.dec
radec = strarr(ndat)
for ii=0,ndat-1 do radec(ii) = adstring(ra(ii),dec(ii))
struct.radec = radec
;
return
end         ;pro old_sources
