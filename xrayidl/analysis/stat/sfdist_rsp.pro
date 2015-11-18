pro sfdist_rsp,expt,bc,sfmin,rsp,minind,lenind,sldn=sldn,esldn=esldn $
,slo=slo,shi=shi,def=def,asfrac=asfrac,outfname=outfname,sv=sv,mslv=mslv $
,esv=esv,mesv=mesv,psel=psel,pweight=pweight,ctof=ctof,tnbs=tnbs,choice=choice
;+ 
;*Name:
;     sfdist_rsp - calculate the response matrix of X-ray source
;                  detection (see Wang 2004, ApJ).
;
;*Inputs:
; expt, bc, sfmin - the exposure (s), background count (counts),
;                   detection flux threshold (counts/s) vectors (e.g.,
;                   pre-selected image pixels) or arrays (e.g.,
;                   images). 
; psel - vector containing image pixel indeces selected for calculating
;       the matrix and to be applied to expt, bc, sfmin, and pweight,
;       which all need to have the same dimension.
; slo, shi - lower and upper boundaries of the detected source flux
;            vector: def=min(sfmin), 0.1 counts/s, which should be
;            slightly broader than the observed source flux upperlimit in
;            the region. 
; sldn, esldn - number of divided bins within a decade of the source flux
;         (def = 30) for the output and expected source
;         flux vectors
; def - if set, esv will be calculated internally
; asfrac - the PSF energy-encircled radius used in the source
;          detection (def =0.9)
; pweight - the pixel weight for the spatial average of the matrix
; outfname - the file name for the matrix output, which can
;                      be used by XSPEC. If not given, no file output. 
; ctof - the flux to count rate conversion for estimating the
;        interloper contribution: def =450/2.2 suitable for the source
;        detection in the  ACIS-S 0.3-7 keV band
; choice - 1: 0.5-2 keV band for background AGN number estimate
;        - 2: 2-10 keV band
;
;*Outputs:
; rsp - the response matrix: 1st dim - detected source flux; 2rd -
;       intrinsic source flux
; minind - starting x-axis index values of the non-zero rsp
; lenind - x-axis index lengths of the non-zero rsp
; sv - bounaries of the output flux (J) channels (in units of counts/s)
; mslv - mean fluxes of the channels
; mesv - mean expected source flux (I) channels
; tnbs - array -- the expected number of interloper per deg2 at each pixel
;
;*Inputs or outputs:
; esv - boundaries of the expected source flux (I) channels: if def is
;       set, = a factor 10 fainter and a factor of 3 brighter than the
;       maximum flux output channel. When given, esldn is not used.
; 
;*Notes:
; 1) Sources are assumed to be uniformly distributed across the
; field, unless pweight is given. 
; 2) The interloper estimate is based on the ACIS-I 0.5-2 keV
; characterization given by Moretti et al. (2003) and is probably an
; underestimate of the true number (if there are sources that are only
; detected in a hard band (e.g., 1.5- 7 keV).
; 3) The matrix don't account for the uncertainties in the background
; image, which are assumed to be small (i.e., in a "Map Detect").
; 4) The falling-off of the probability total(rsp,1) at high mesv is
; artificial, which should have no effect on the subsequent analysis
; though, as long as it is well above the upper limit to the observed flux.
;
; Written by wqd, Oct 19, 2003
; Revised for a more accurate calculation of the matrix and for
; including the interloper estimate. wqd, May 15, 2004
;-
if n_params() eq 0 then begin
print,'Calling Seq - sfdist_rsp,expt,bc,sfmin,rsp,minind,lenind,sldn=sldn,esldn=esldn'
print,'psel=psel,,slo=slo,shi=shi,def=def,asfrac=asfrac,outfname=outfname,sv=sv'
print,',mslv=mslv,esv=esv,mesv=mesv,pweight=pweight,ctof=ctof,tnbs=tnbs,choice=choice'
return
endif 

;check the consistency of the vector dimensions 
nbin=n_elements(expt)
npw=n_elements(pweight)
if n_elements(bc) ne nbin or n_elements(sfmin) ne nbin or $ 
  (npw ne 0 and npw ne nbin) then begin
    print,'The dimentions of exposure, background, sensitivity limit and weight'
    print,'vectors should be the same!'
    return
endif 
npsel=n_elements(psel)
if npsel ne 0 then begin
    expts=expt(psel)
    bcs=bc(psel)
    sfmins=sfmin(psel)
    if npw ne 0 then pweights=pweight(psel) ;else pweights=expts*0.+1./npsel
endif else begin 
    expts=expt & bcs=bc & sfmins=sfmin 
    if npw ne 0 then pweights=pweight ;else pweights=expt*0.+1./n_elements(expts)
endelse 
nbin=n_elements(expts)
;npw=n_elements(pweights)
if n_elements(asfrac) eq 0 then asfrac=0.9
ttd=expts*asfrac ;PSF EEF-modified effective source exposure

;Define the J channel significance range for the probability
;calculation on both sides of I channel
sigmalo=4.
sigmahi=4.

;define the ouptut source flux vector
if n_elements(slo) eq 0 then slo=min(sfmins)
if n_elements(shi) eq 0 then shi=1.e-1 
lslo=alog10(slo) & lshi=alog10(shi)
if n_elements(sldn) eq 0 then sldn=30
sldim=nint(lshi-lslo)*sldn 
  ;total number of J channels
slv=[(lshi-lslo)*findgen(sldim+1)/sldim+lslo]
sv=10^slv
shi=sv(sldim) ;make sure this is the case, removing round-up errors
svmax=long(shi*ttd+bcs) ;maximum counts in each pixel
svmin=long(sfmins*ttd+bcs) ;minimum

;define the expected (or intrinsic) source flux vector
if n_elements(esv) eq 0 or keyword_set(def) ne 0 then begin
    if n_elements(esldn) eq 0 then esldn=sldn
    lelo=alog10(slo/10.) & lehi=alog10(shi*3.) 
    eledim=long((lehi-lelo)*esldn) 
    lev=[(lehi-lelo)*findgen(eledim+1)/eledim+lelo]
    esv=10^lev
    nes=n_elements(esv)-1
    mesv=10^(0.5*(lev(1:nes)+lev(0:nes-1)))
endif else begin
    nes=n_elements(esv)-1       ;number of channels
    if nes eq 0 then begin
	mesv=esv(0)
	nes=1
    endif else mesv=sqrt(esv(1:nes)*esv(0:nes-1))
endelse

;define the output matrix and vectors:
minind=intarr(nes) ;to contain the starting J channel index for each I channel
lenind=intarr(nes) ;to contain the number of J channels for each I channel
rsp=fltarr(sldim,nes) ;output metrix
rspt=fltarr(sldim,nbin) ;temperory metrix for each I channel

if npw eq 0 then begin
;for the interloper estimate:
    if n_elements(ctof) eq 0 then ctof=450/2.2 
                                ;450 for the ACIS-I(0.5-2) ctof in
                                ;units 10^{-14} ergs/s/cm^2/(cts/s)
                                ;both in the 0.5-2 keV band. see 
                            ; 2.2 for ACIS-S(0.3-7)/ACISI(0.5-2)
    int_bs_new,esv*ctof,nbs,choice=choice ;,bsp=bsp,smax=smax ;assuming the def
    tnbs=fltarr(nbin) ;to contain the estimated interloper source number per pixel
endif

for kk=0,nes-1 do begin ;loop over the expected source flux channels
    ec=mesv(kk)*ttd+bcs ;expected counts for all bins
    svlo=long((ec-sigmalo*sqrt(ec)) > svmin)
            ;lower count limit to the J channel in the metrices 
    svhi=long(ec+sigmahi*sqrt(ec > 15.)  < svmax);upper limit
    ;therefore the range defined by svlo and svhi is always within sv
    ss=where(svhi gt svlo,nss) 
             ;the opposite could happend for very low source flux
    if nss ne 0 then begin
        for m=0L,nss-1 do begin ;loop over the slected bins
            k=ss(m)
            cv=long(sv*ttd(k)+bcs(k)) ;counts in the J channels at this pixel
            sel=where(cv gt svlo(k) and cv lt svhi(k),nsel)
                                ;calculate the probabibilities in the
                                ;J channels, including the boundary effect:
            if nsel gt 0 then begin
                cve=[svlo(k),cv(sel),svhi(k)] 
                prob_poi_gau,ec(k),cve,prob 
                rspt(sel(0)-1:sel(0)-1+nsel,k)=prob
            endif else begin
                sel=where(cv ge svhi(k)) ;find the nearest high J channel
                if sel(0) ne sldim then begin ;if not at the end of sv
                    cve=[svlo(k),svhi(k)]
                    prob_poi_gau,ec(k),cve,prob 
                    rspt(sel(0)-1,k)=prob
                endif 
            endelse         
;if !debug eq 3 then stop
        endfor

	if npw ne 0 then begin
            for k=0L,nbin-1 do rspt(*,k)=rspt(*,k)*pweights(k)
            temp=total(rspt,2)
	endif else begin
            temp=total(rspt,2)/nbin
            tnbs=tnbs+total(rspt,1)*(nbs(kk)-nbs(kk+1))
        endelse
        rsp(*,kk)=temp
        sel=where(temp ne 0.,nsel)
        if nsel ne 0 then begin 
            minind(kk)=sel(0)
            lenind(kk)=sel(nsel-1)-sel(0)+1
        endif 
        rspt(*)=0. ;for next cycle
        print,'finishing kk = ',kk+1,' out of ',nes $
          ,' total prob = ',total(temp)
    endif
;if !debug eq 3 then stop
endfor

;rsp=rsp(1:*,*) ;remove the first row which should all be 0
if n_elements(outfname) ne 0 then $
  write_rsp,outfname,esv,sv,rsp,minind,lenind

;output the center values of the intrinsic and detected flux vectors
; (geometric means)
mslv=0.5*(slv(1:sldim)+slv(0:sldim-1))

if n_elements(psel) ne 0 then begin
    cc=expt*0.
    cc(psel)=tnbs
    tnbs=cc
endif
if !debug eq 3 then stop
return
end 

