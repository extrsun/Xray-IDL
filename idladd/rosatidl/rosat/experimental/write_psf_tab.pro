;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   write_psf_tab
;
;*PURPOSE:
;   A procedure to write a PSF profile to a ST SDAS binary table file
;
;*CALLING SEQUENCE:
;   write_psf_tab,psfreg,imgfil,proftab,offang,prof,tcb,profhdr,$
;                 profmult=profmult
;
;*PARAMETERS:
; INPUTS:
;   psfreg   - ASCII region descriptor for region over which PSF profile 
;              was calculated
;   imgfil   - name of output FITS file for PSF image (or 'NONE')
;   proftab  - name of output ST SDAS table (_psf.tab)
;   offang   - off axis angles to be written into table
;   prof     - values of PSF profile (at offang)
;
; OPTIONAL INPUTS:
;   profmult - value by which radially averaged profile was multiplied
;              to make maximum value = 1.0
;              (default = 1.0)
;
; OUTPUTS:
;   tcb      - table control block for output SDAS table
;   profhdr  - FITS header for output SDAS table
;
;*NOTES: 
;
;*SUBROUTINES CALLED:
;  TAB_EXPAND
;  TAB_PUT
;  TAB_WRITE
;
;*MODIFICATION HISTORY:
;    written 14 July 1992 by GAR
;    modified 07 Aug 1992 (GAR) to be compatible with new version of
;      make_psf
;-
;-------------------------------------------------------------------------------
pro write_psf_tab,psfreg,imgfil,proftab,offang,prof,tcb,profhdr,$
                  profmult=profmult
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' WRITE_PSF_TAB, psfreg, imgfil, proftab, offang, prof, TCB, PROFHDR,'
  print,'                profmult=profmult (def=1)'
  retall
endif
if (n_elements(profmult) eq 0) then profmult = 1.0       ;default = 1
;
tcb = lonarr(16,7)
table = bytarr(24, 10)
profhdr = strarr(10)
;
noff = n_elements(offang)
temp = string(noff,format='$(i20)')
profhdr(0) = "IMNAME   = '"+strtrim(imgfil)+"' /"
profhdr(1) = "REGION   = '"+psfreg+"'      /"
profhdr(2) = "OFFANG   ="+temp+" /"
profhdr(3) = "PROF     ="+temp+" /"
temp = string(profmult,format='$(e12.4)')
profhdr(4) = "SCALEFAC ="+temp+" /"
profhdr(5) = "END"
;
tcb(0,0) = [100,4,5,10,10,2,5,12,12,11,lonarr(6)]
tcb(0,1) = [1,0,4,7]
tcb(0,2) = [2,4,4,7]
maxcol=tcb(6,0)
byte_name=replicate(32b,64)
byte_name(0)=byte(proftab)
tcb(0,maxcol+1)=long(byte_name,0,16)    ;place filename into tcb
;
hbyte = bytarr(8)
hbyte(0) = byte('-10d')
for ii=1,2 do tcb(14,ii) = long(hbyte,0,2)
;
hbyte = bytarr(20)
hbyte(0) = byte('radius')
tcb(4,1) = long(hbyte,0,5)
hbyte = bytarr(20)
hbyte(0) = byte('counts_r')
tcb(4,2) = long(hbyte,0,5)
;
if (!debug gt 2) then stop,' Stopping in write_psf_tab before header update'
;
tab_expand,tcb,table,5,noff,12            ;new number of rows = noff
tab_put,'radius',offang,tcb,table,0
tab_put,'counts_r',prof,tcb,table,0
;
if (!debug gt 1) then stop,' Stopping before writing SDAS profile table'
tab_write,proftab,tcb,table,profhdr
;
return
end         ;pro write_psf_tab
