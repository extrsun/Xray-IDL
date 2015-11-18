pro map_exp,expt,tb,ta,bv=bv,fhead=fhead,mapdir=mapdir,tbw=tbw,tanorm=tanorm $
,hdr=hdr
;+
; create exposure maps from instrument maps, plus a combined exposure map
; weighted by tbw 
; The exposure normalization is in respect to the reference pixel,
; which is assumed to be the aiming point and needs to be inside the
; image. The fits parameter degradscale needs to be present in the
; fits header of the instrument map, which is implemented on July 31, 2003.
;
; expt - live time of the observation (exposure corrected for dead
;        time). if =0, no normalization will be performed
; tb - combined exposure map, normalized to the value
;	at the aiming point
; ta - array of exposure maps in individual (bv) selected bands, 
;		normalized to the value at the aiming point
; bv - selecting vector containing band numbers (def =[1,2,3,4])
; fhead - the head name of the instrument maps (def = 'expmap')
; mapdir - the directory in which the instrument maps are located
;		(def ='')
; tbw - vector containing the count flux weights in the bands
;	which depends on the spectral model used for creating the instrument
;	maps. (def =!tbw defined in sou_det_params)
;	if tbw=0, output no tb.
; tanorm - optional output of the normalization that is muliplied to the 
;	instrument maps to get the exposure maps ta	
; hdr - fits header for the output exposure maps: if given, the instrument 
; 	will be re-casted accordingly
; 
; written by wqd, 6/28/2001
; rewritten to accormodate the CCD degration scaling. wqd, July, 31, 2003
;-
if n_params() eq 0 then begin
print,'Calling Seq. - map_exp,expt,tb,ta,bv=bv,fhead=fhead,mapdir=mapdir'
print,',tbw=tbw,tanorm=tanorm,hdr=hdr'
return
endif

if n_elements(fhead) eq 0 then fhead='evt2file_new_clean_i'
if n_elements(mapdir) eq 0 then mapdir=''
if n_elements(bv) eq 0 then bv=[1,2,3,4]
nbt=n_elements(bv)
tref=fltarr(nbt)
for k=0,nbt-1 do begin
    fname=mapdir+fhead+strtrim(bv(k),2)+'.fits'
    instmap=readfits(fname,fhdr)
    xpref=nint(sxpar(fhdr,'crpix1'))
    ypref=nint(sxpar(fhdr,'crpix2'))
    if (xpref lt 0 or xpref gt (sxpar(fhdr,'naxis1')-1) or $
        ypref lt 0 or ypref gt (sxpar(fhdr,'naxis2')-1)) then $
        stop,'The reference pixel is outside the image!!!'
    tref(k)=instmap(xpref,ypref)
    degrad_scale=sxpar(fhdr,'degradscale',count=count)
    if count eq 0 then begin
        dtype=sxpar(fhdr,'dtype*',count=count)
        if count ne 0 then begin
            dval='dval'+strtrim(where(dtype eq 'degradscale')+1,2)
            degrad_scale=sxpar(fhdr,dval(0))
        endif 
    endif 
;    if count eq 0 then begin
;        stop,'no degradation scaling factor in the exposure map file!!!'
;        print,'If you are sure that this is OK, type .c to continue.'
;    endif else begin 
;        print,'degrad_scale = ',degrad_scale
;        tref(k)=tref(k)/degrad_scale
;    endelse 
    if n_elements(hdr) ne 0 then  begin
        print,'casting the exposure maps, ...'
        cast,'',hdr,outa=expmap,ina=instmap,inh=fhdr
    endif else expmap=instmap
    if k eq 0 then begin
    	sz=size(expmap)
    	ta=fltarr(sz(1),sz(2),nbt)
    endif
    ta(*,*,k)=expmap
endfor
if expt eq 0 then return ;no normalization to the exposure 
tanorm=expt/tref
print,'tanorm = ',tanorm
for k=0,nbt-1 do ta(*,*,k)=ta(*,*,k)*tanorm(k)

if n_elements(tbw) eq 0 then tbw=!tbw else $
	if tbw(0) eq 0 then return ;return normalized maps
tb=ta
if nbt gt 1 then begin
    tbnorm=tbw(bv-1)/total(tbw(bv-1))
    for k=0,nbt-1 do tb(*,*,k)=ta(*,*,k)*tbnorm(k)
    tb=total(tb,3)
endif
if !debug eq 2 then stop
return
end
