pro map_exp,expt,tb,ta,bv=bv,fhead=fhead,mapdir=mapdir,tbw=tbw,tanorm=tanorm $
,hdr=hdr,mask=mask,expfth=expfth
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
; tanorm - optional input and output of the normalization (in all 4 bands) 
; 	that is muliplied to the instrument maps to get the exposure maps ta	
; hdr - fits header for the output exposure maps: if given, the instrument 
; 	will be re-casted accordingly
; expfth - the minmum excepted exposure fraction of the max(exposure)
;          (def=0.1) to minimize the effect of any small offset from
;          evt images
;
; written by wqd, 6/28/2001
;
; rewritten to accormodate the CCD degration scaling. wqd, July, 31,
; 2003
; commented out on June 6, 2007 by wqd
; Now the normalization is not calculated here and is included in a vector.
; The normalization can be calcualted using the old map_exp.pro, which is 
; now renamed as exp_map_test.pro. To use the later, .run map_exp_test
; but this would only give you the corrent normalization (i.e., the
; ice-blocking will not be corrected).
;
; wqd, Sept 9, 2003.
;-
if n_params() eq 0 then begin
print,'Calling Seq. - map_exp,expt,tb,ta,bv=bv,fhead=fhead,mapdir=mapdir'
print,',tbw=tbw,tanorm=tanorm,hdr=hdr,expfth=expfth'
return
endif
if n_elements(expfth) eq 0 then expfth=0.001
if n_elements(fhead) eq 0 then fhead='evt2file_new_clean_i'
if n_elements(mapdir) eq 0 then mapdir=''
if n_elements(bv) eq 0 then bv=[1,2,3,4]
nbt=n_elements(bv)
;tref=fltarr(nbt)
for k=0,nbt-1 do begin
    fname=mapdir+fhead+strtrim(bv(k),2)+'.fits'
    instmap=readfits(fname,fhdr)
	degrad_scale=sxpar(fhdr,'degradscale',count=count)
;commented out on June 6, 2007 by wqd
;    if count eq 0 then begin
;        dtype=sxpar(fhdr,'dtype*',count=count)
;        if count ne 0 then begin
;            dval='dval'+strtrim(where(dtype eq 'degradscale')+1,2)
;            degrad_scale=sxpar(fhdr,dval(0))
;        endif 
;    endif 
;       if count eq 0 then $
;	print,'Warning: no degradscale parameter in the instrument map herder!!!'
    if n_elements(hdr) ne 0 then  begin
        print,'casting the exposure maps, ...'
        cast,'',hdr,outa=expmap,ina=instmap,inh=fhdr,/noi
    endif else expmap=instmap
    if k eq 0 then begin
    	sz=size(expmap)
    	ta=fltarr(sz(1),sz(2),nbt)
        ss=where(expmap lt expfth*max(expmap) and expmap gt 0.,nss)
    endif
    if nss ne 0 then expmap(ss)=0.
    ta(*,*,k)=expmap
endfor
;if expt eq 0 then return ;no normalization to the exposure 
;if n_elements(tanorm) eq 0 then begin
if expt ne 0 then begin
    if n_elements(tanorm) eq 0 then begin ;changed, wqd, april 9, 2006
	tanorm =!tanorm
	print,'Using the standard !tanorm = ',tanorm
    endif
    for k=0,nbt-1 do ta(*,*,k)=ta(*,*,k)*tanorm(bv(k)-1)
endif

if n_elements(tbw) eq 0 then tbw=!tbw else $
	if tbw(0) eq 0 then return ;return normalized maps
tbnorm=tbw(bv-1)/total(tbw(bv-1))
tb=ta
for k=0,nbt-1 do tb(*,*,k)=ta(*,*,k)*tbnorm(k)
if n_elements(mask) gt 1 then begin
    ss=where(mask gt 0)
    imagea_sel,ss,tb
endif
if nbt gt 1 then tb=total(tb,3)
if !debug eq 2 then stop
return
end
