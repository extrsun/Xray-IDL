pro bs_norm,sfname,bfname,rmfname,bexpt,cbg,bbg,eeg,glo,ghi,eflo=eflo,efhi=efhi,ctonth=ctonth,fnorm=fnorm,noplot=noplot,psfile=psfile,rselout=rselout
;+
; Compare two spectra (e.g., source and background) and estimate the 
; normalization needed to best-match them. Specific application, for
; example, includes the estimate of the adjusted exposure for an off-source
; spectrum so that it can be used as the on-source background
; spectrum, by matching the two corresponding blank-sky spectra.
;
;*Input
; sfname, bfname - names of the source and background spectral fits.
; rmfname - rmf file used for reading out the energy boundaries of the
;           spectral channels. (ciao arf file contains many more channels,
;           which don't correspond to those in the spectral file and
;           cover a narrower energy range.
;*Outputs:
; bexpt - corrected exposure time for the blank-sky
;         background file, which may be corrected, by using fv, for example.
; 
;*optional inputs and outputs:
; ctonth - the count to noise threshold used for the adaptively
;          grouping: def=4
; cbg, bbg - grouped count and normalized background counts 
; fnorm - fraction correction of the background exposure (to be divided)
; eflo, efhi - the lower and upper limits of the channel interval within
;		which, the comparison between the observation and
;		blank-sky spectra are to be done. Def eflo=0.3,
;		efhi=10 keV. They could be vectors. gaps (e.g.,
;		abnormal emission lines) between the
;		selected energy ranges will excluded from the fit.
; noplot - if set, no plot will be made. Otherwise, both the
;          comparison of the spectra and the residual will be ploted 
; psfile - postscript file name. If given, the plots will exported to
;          the file.
; rselout - vector indexs excluded in the chi^2 fit to get the
;           correction
;
;*Note
; the keywords, exposure and backscal in the fits headers of the source and 
; background files must have been defined correctly.
;
;*Example:
; bs_norm,'bcluster_s.pi','bcluster_b.pi','cluster_sou.wrmf',bexpt,cbg,bbg,glo,ghi,eflo=[0.4,1.7],efhi=[1.9,5],psfile='spec_ref_comp.ps'
; where the 1.7-1.9 range is excluded from the fit.
;
; written by wqd, May 31, 2003
;
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- bs_norm,sfname,bfname,rmfname,bexpt,cbg,bbg,eeg'
print,',glo,ghi,eflo=eflo,efhi=efhi,ctonth=ctonth,fnorm=fnorm'
print,',noplot=noplot,psfile=psfile,rselout=rselout'
return
endif

if n_elements(ctonth) eq 0 then ctonth=4.
; input source data
tab=readfits(sfname,ext=1,hdr)
snrow=sxpar(hdr,'naxis2')
sc=tbget(hdr,tab,'counts',lindgen(snrow))
sexpt=sxpar(hdr,'exposure',count=count)
if count ne 1 then stop,'stop: count not = 1'
sarea=sxpar(hdr,'backscal',count=count)
if count ne 1 then stop,'stop: count not = 1'

; input background data
tab=readfits(bfname,ext=1,hdr)
bnrow=sxpar(hdr,'naxis2')
if bnrow ne snrow then stop,'stop: snrow ne bnrow!'
bc=tbget(hdr,tab,'counts',lindgen(bnrow))
bexpt=sxpar(hdr,'exposure',count=count)
if count ne 1 then stop,'stop: count not = 1'
barea=sxpar(hdr,'backscal',count=count)
if count ne 1 then stop,'stop: count not = 1'

;read channel boundaries from the rmf file
tab=readfits(rmfname,ext=2,hdr)
rmfrow=sxpar(hdr,'naxis2')
if rmfrow ne snrow then print,'rmfrow ne bnrow: ',rmfrow, bnrow
;elo=tbget(hdr,tab,'energ_lo',lindgen(bnrow))
;ehi=tbget(hdr,tab,'energ_hi',lindgen(bnrow))
elo=tbget(hdr,tab,'e_min',lindgen(bnrow))
ehi=tbget(hdr,tab,'e_max',lindgen(bnrow))

;normalization of the background 
bnorm=sexpt*sarea/(bexpt*barea)

;get the starting and ending channels
nch=n_elements(bc)
if n_elements(eflo) eq 0 then eflo=0.3
if n_elements(efhi) eq 0 then efhi=10.
bracket_v,elo,min(eflo),i_value=i,m_value=m
if m ne -1 then klo=long(m(0)) else klo=long(min(i) > 0)
glo=[elo(klo)]
bracket_v,ehi,max(efhi),i_value=i,m_value=m
if m ne -1 then khi=long(m(0)) else khi=long(max(i) < (nch-1))

;adaptively bin the data
sctonth=ctonth^2
cb=0 &  bb=0
kk=0
for k=klo, khi do begin
	cb=cb+sc(k)
	bb=bb+bc(k)
	if cb gt sctonth  and bb gt sctonth then begin 
            if kk eq 0 then begin
                cbg=float(cb)
                bbg=float(bb)
                ghi=[ehi(k)]
                glo=[glo,elo(k)]
            endif else begin
                cbg=[cbg,cb]
                bbg=[bbg,bb]
                ghi=[ghi,ehi(k)]
                glo=[glo,elo(k)]
            endelse 
            cb = 0 & bb=0
            kk=kk+1
	endif
endfor
glo=glo(0:kk-2)

;mark the bins which are to be excluded from chi^2 estimate
nesel=n_elements(eflo)
if nesel gt 1 then begin
    for k=1, nesel-1 do begin
        s=where((glo lt eflo(k) and ghi gt efhi(k-1)) $ ;completely enclosed
              or (glo gt eflo(k) and glo lt efhi(k-1)) $ ;partially enclosed
              or (ghi gt eflo(k) and ghi lt efhi(k-1)),ns) ;partially enclosed
        if k eq 1 then rsel=s else rsel=[rsel,s]
    endfor
endif

;chi^2 fit to find the normalization
data_chi2,cbg,bbg,fnorm,eeg,chi2,chi,bnorm=bnorm,dof=dof,rsel=rsel
if n_elements(rsel) ne 0 then rselout=rsel
print,'bexpt and fnorm =', bexpt, fnorm
bexpt=bexpt/fnorm
print,'The normalized exposure for the background is ',bexpt

;find bins with large deviations
chiv=(cbg-bbg)/sqrt(eeg)
s=where(abs(chiv) gt 3.,ns)
if ns ne 0 then begin
    print,'bins with Chi > 3 are ',(glo(s)+ghi(s))*0.5,' keV'
    print,'Features at these energies should be treated with caution!'
endif

;calculate the spectra in right units
cbg=cbg/(ghi-glo)/sexpt
bbg=bbg/(ghi-glo)/bexpt
if keyword_set(noplot) eq 0 then begin
    plot_oo,(glo+ghi)*0.5,cbg,xrange=[min(eflo),max(efhi)],xstyle=1 $
      ,xtit='Channel energy (keV)',ytit='Counts s!U-1!N keV!U-1!N'
    oplot,(glo+ghi)*0.5,bbg,thick=2
    if nesel gt 1 then begin
        for k=1, nesel-1 do begin
            oplot,[1,1]*eflo(k),[1.e-10,10]
            oplot,[1,1]*efhi(k-1),[1.e-10,10]
        endfor
    endif 
    print,'In the plot, the red curve represents the background'
    stop,'Type c. to plot the residual'
    plot_oi,(glo+ghi)*0.5,chiv,xrange=[min(eflo),max(efhi)],xstyle=1 $
      ,xtit='Channel energy (keV)',ytit='Chi'
endif
if n_elements(psfile) ne 0 then begin
    set_plot,'ps'
    device,color=1,filename=psfile
    plot_oo,(glo+ghi)*0.5,cbg,xrange=[min(eflo),max(efhi)],xstyle=1 $
      ,xtit='Channel energy (keV)',ytit='Counts s!U-1!N keV!U-1!N'
    oplot,(glo+ghi)*0.5,bbg,thick=2
    if nesel gt 1 then begin
        for k=1, nesel-1 do begin
            oplot,[1,1]*eflo(k),[1.e-10,10]
            oplot,[1,1]*efhi(k-1),[1.e-10,10]
        endfor
    endif 
    plot_oi,(glo+ghi)*0.5,chiv,xrange=[min(eflo),max(efhi)],xstyle=1 $
      ,xtit='Channel energy (keV)',ytit='Chi'
    device,/close
    set_plot,'x'
endif
return
end
