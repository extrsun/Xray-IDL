pro writefits_evt,mlist,fname,rfname,cra=cra,cdec=cdec,cpx=cpx,cpy=cpy,ctype1=ctype1,ctype2=ctype2,pixsize=pixsize
;+
; NAME:
;    writefits_evt
; PURPOSE:
; write an events list into a fits file with keywords in Chandra
; evt file format (not compltely, though)
;
; CALLING SEQUENCE:
;  writefits_evt,mlist,fname,rfname,cra=cra,cdec=cdec,cpx=cpx,cpy=cpy'
; ctype1=ctype1,ctype2=ctype2,pixsize=pixsize
;
; INPUTS:
; mlist - event list
; fname - output evt file name
; rfname - reference evt file name (e.g., the original evt2 file name)
; 
; cra, cdec - RA and Dec (or Long and Lati) in deg
; cpx, cpy - reference pixel (e.g, (mdim(1)*block+1)*0.5)
; ctype1, ctype2 - Coordinate type (e.g., 'RA--TAN' and 'DEC--TAN')
; pixsize - pixel size in units of deg
;
; MODIFICATION HISTORY:
; written by wqd, Aug, 23, 2003
;-
;===============================================================
if n_params() eq 0 then begin
print,'CALL SEQUENCE - writefits_evt,mlist,fname,rfname,cra=cra,cdec=cdec'
print,',cpx=cpx,cpy=cpy,ctype1=ctype1,ctype2=ctype2,pixsize=pixsize'
return
endif
;
;get the reference headers for the two extensions:
h0=headfits(rfname,ext=0)
FXWRITE,fname,h0
if n_elements(rfname) ne 0 then begin
    rh1=headfits(rfname,ext=1)
    if n_elements(cra) eq 0 then cra=sxpar(rh1,'tcrvl11')
    if n_elements(cdec) eq 0 then cdec=sxpar(rh1,'tcrvl12')
    if n_elements(cpx) eq 0 then cpx=sxpar(rh1,'tcrpx11')
    if n_elements(cpy) eq 0 then cpy=sxpar(rh1,'tcrpx12')
    if n_elements(pixsize) eq 0 then pixsize=sxpar(rh1,'tcdlt12')
    if n_elements(ctype1) eq 0 then ctype1=sxpar(rh1,'tctyp11')
    if n_elements(ctype2) eq 0 then ctype2=sxpar(rh1,'tctyp12')
endif
fxbhmake,h1,n_elements(mlist),'events',/init
sxaddpar,h1,'tctyp11',ctype1 
sxaddpar,h1,'tcrvl11',cra
sxaddpar,h1,'tcrpx11',cpx
sxaddpar,h1,'tcdlt11',-pixsize
sxaddpar,h1,'tctyp12',ctype2
sxaddpar,h1,'tcrvl12',cdec
sxaddpar,h1,'tcrpx12',cpy ;(mdim(1)*block+1)*0.5
sxaddpar,h1,'tcdlt12',pixsize
mwrfits,mlist,fname,h1
return
end
