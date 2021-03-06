pro var_ks,ttref,lls,tlo,thi,vslist,probth=probth,noprint=noprint,ssel=ssel
;+
; Test the no variation hypothesis for X-ray sources, using the KS 
; statistic and allowing background variation, which is assumed to be 
; the same as that in off-source region (ttref)
; The procedure should be called by sou_ks_loop.pro
;
; ttref - presorted reference time series (background counts)
;	to be compared with.
; lls - count list presorted according to arrival time.
; tlo, thi - the GTI, including the lower and upper time limits
; vslist - source list containing input parameters: 
;	sn - source number
; 	nxp, nyp - event pixel positions of the sources
; 	vsr - vector contains the radii (in pixels) from which counts are 
;		to be extracted from the list
;	vbc - background counts per bin at each position of the sources
; 	as output: 	vprob -- probability for the constancy; 	
;			vsc -- net source counts
; probth - probability threshold below which a source is to be marked as 
;		variable. Default probth=-2 
; noprint - if =1 no output print
;
; written by wqd, 12/4/98
; completely revised by wqd, Sept 14, 2002, to use source structure.
;-
if ( N_params() LT 4 ) then begin
print,'Syntax - var_ks,ttref,lls,tlo,thi,vslist,probth=probth,noprint=noprint'
return
endif

;if n_elements(probth) eq 0 then probth=alog10(0.001350) ;3 sigma
if n_elements(probth) eq 0 then probth=-2
print,'The assumed log(probability) threshold is ',probth

;get the accumulated count distribution of the reference background region
ntint=n_elements(tlo)
ttr=ttref
ttr=[tlo(0),ttr,thi(ntint-1)]

;get time, x, y of source counts: 
tts=lls.time 
xxs=lls.x
yys=lls.y

;remove gaps in both the source and reference count vectors:
if ntint gt 0 then begin 
 tgap=0d
	sel=where(tts ge tlo(0) and tts le thi(0),nsel)
	if nsel ne 0 then ttsv=tts(sel)
	sel=where(ttr ge tlo(0) and ttr le thi(0),nsel)
	if nsel ne 0 then ttrv=ttr(sel)
 for k=1,ntint-1 do begin
	tgap=tgap+tlo(k)-thi(k-1)
	sel=where(tts ge tlo(k) and tts le thi(k),nsel)
	if nsel ne 0 then ttsv=[ttsv,tts(sel)-tgap]
	sel=where(ttr ge tlo(k) and ttr le thi(k),nsel)
	if nsel ne 0 then ttrv=[ttrv,ttr(sel)-tgap]
 endfor
endif
tts=ttsv
ttr=ttrv
ttot=total(thi-tlo)
nttr=n_elements(ttr)
normv=[findgen(nttr-1),nttr-1]/(nttr-1)
;--------------------------------------------------
srr=(vslist.vsr)^2
;--------------------------------------------------
; do the KS test:
ns=n_elements(vslist)
sb=vslist.vbc
nssv=intarr(ns)
probv=fltarr(ns)
if not keyword_set(noprint) then $
 print,'Source No.,    sn,     net c,      bc,      dd,      prob    var flag'
nssel=n_elements(ssel)
for k=0,nssel-1 do begin
    kk=ssel(k)
	;choose only those counts for this source:
	ss=where(((xxs-vslist(kk).nxp)^2+(yys-vslist(kk).nyp)^2) le srr(kk),nss)
	if (nss-sb(kk) gt 10) then begin ;only sources with net counts greater
					 ; than 10 are considered
	ttss=tts(ss)
	linterp,ttr,normv,ttss,ff ;find the background function values at ttss
	ratio=sb(kk)/nss
	ff=ratio*ff+(1.-ratio)/ttot*(ttss-tlo(0))
	;the first part is due to background which varies according to ttr
	;the second part is due to the source assumed to be constant
	f0 = (findgen(nss)+1.)/nss
	dd=max(abs(f0-ff))
	prob_ks, dd, nss, prob
	prob=alog10(prob > 1.e-30)
 	if not keyword_set(noprint) then begin
		if prob lt probth then $
	  	 print,kk, vslist(kk).sn,nss-sb(kk), sb(kk), dd, prob, ' var' $
		 else print,kk, vslist(kk).sn,nss-sb(kk), sb(kk), dd,prob
	endif
	probv(kk)=prob
	nssv(kk)=nss
	endif
endfor
struct_col_add,vslist,probv,['vprob'],[0.0],vslistn
struct_col_add,vslistn,nssv-sb,['vsc'],[0.0],vslist
return
end
