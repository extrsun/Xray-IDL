;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rsget_resp
;
;*PURPOSE:
; Gets response matrix information from .DMP files (supplied with SASS data 
;   products)
; Now reads the .DMP FITS file directly - no need to convert to ST SDAS
;   format
;
;*CATEGORY:
;
;*CALLING SEQUENCE:
;       rsget_resp,dmpname,resp,e0,deltaE,traf
;
;*PARAMETERS:
; INPUTS:
;       dmpname - name of .DMP file (include directory; extension not needed)
;
;*OUTPUTS:
;       resp    - response matrix
;       e0      - central energies of bins used by response file (keV)
;       deltaE  - width of energy bins (keV)
;       traf    - filter transmission coefficient
;
;*PROCEDURE:
; Function reads the response matrix info from the .DMP FITS file
; (supplied as part of the SASS data products).
; Optionally outputs e0, deltaE and traf info as well.
;
;*RESTRICTIONS:
;
;*NOTES:
;
;*SUBROUTINES CALLED:
; MATCH_FILES
;
;*MODIFICATION HISTORY:
;    Dec 9, 1991 written by M. F. Corcoran (as get_rsp)
;    23 Jun 1992 modified to use READFITS; and for compatibility with
;       with ROSAT IDL library (GAR)
;-
;-------------------------------------------------------------------------------
pro rsget_resp,dmpname,resp,E0,deltaE,traf
;
npar = n_params(0)
if (npar eq 0) then begin
   print, 'RSGET_RESP, dmpname, RESP, E0, DELTAE, TRAF'
   retall
endif
;
fdecomp,dmpname,disk,dir,rootname,qual,ver
if (qual eq '') then qual = '.dmp' else qual = '.' + qual
dir = disk + dir
match_files,dir,rootname,qual,name,nlist     ;look for the right files
name = name(0)                               ;default = take latest version
;
tab = readfits(name,hdr,ext=1,/sil)
E0 = tbget(hdr,tab,1)
deltaE = tbget(hdr,tab,2)
E0 = E0/1000.0                               ; convert to keV
deltaE = deltaE/1000.0
traf = tbget(hdr,tab,3)
resp = tbget(hdr,tab,4)
;
return
end              ;pro rsget_resp
