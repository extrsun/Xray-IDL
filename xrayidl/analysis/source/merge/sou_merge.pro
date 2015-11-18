pro sou_merge,infile,outfile,outrfile,outdfile,slow=slow,flow=flow,sradius=sradius,psigma=psigma,sperr=sperr,nooapsyserr=nooapsyserr
;+
; Merge various source detections
;
; infile - input source file name
; slow, flow - S/N selection criteria for the sources to be merged
; sradius - (scalar) the minimum radius in which sources are considered to 
;	be duplicate ones (in units of arcsec). 
; psigma - number of sigma value for the position error radius 
; sperr - systematic error in units of arcsec
; nooapsyserr - if set, no offaxis position uncertainty dependence will be added
;
;*outputs:
; outfile - file name to contain the merged source list
; outrfile - file name to contain the removed source list
; outdfile - file name to contain both the merge sources and their 
;		removed sources
;
;*example
; sou_merge,'sou_comb','sou_merge','sou_merge_r','sou_merge_d',slow=2.5
;
;*NOTE:
;
; written by wqd, 12/24/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sou_merge,infile,outfile,outrfile,outdfile'
print,',slow=slow,sradius=sradius,psigma=psigma,sperr=sperr,nooapsyserr=nooapsyserr'
return
endif

if n_elements(sperr) eq 0 then sperr=0.
if n_elements(psigma) eq 0 then spsigma=1. else spsigma=psigma^2
sou_fits_info,infile,slist,/all,slow=slow,flow=flow,nsel=ns
if ns eq 0 then begin
	print,'No source in the file!'
	return
endif 

;find duplicate sources:
perr=slist.perr
slist=slist(sort(perr)) ;sort according to position accuracy

if n_elements(sradius) eq 0 then psyserr=0.3  else psyserr=sradius ;in arcsec
if not keyword_set(nooapsyserr) then $
	psyserr=psyserr+1.4*(slist.offaxis/8.)^2 ; a rough fit to the
	; the Orion x-o offsets as a function of axis angles (Feigelson) 
	; plate scale uncertainty may also be considered to be included.
struct_col_add,slist,psyserr,['PSYSERR'],0.0*[1]
srs=perr^2+psyserr^2 
;
sra=slist.ra
sdec=slist.dec
sel=intarr(ns) ;for source removal
for k=ns-1,1,-1 do begin 
	trans_dist,sra(k),sdec(k),sra(0:k-1),sdec(0:k-1),px,py,/deg,/das
	sep=(px^2+py^2)
	seperr=srs(0:k-1)+srs(k)+sperr^2 ;combined position error square
	ser=spsigma*seperr
	rsel= where(sep lt ser, nrsel) 
	if nrsel ne 0 then sel(k)=1 ;mark this duplicate source
endfor

;find all the duplicate sources marked
rsel=where(sel eq 1,nrsel)
if nrsel eq 0 then begin
	print,'No duplicate source!'
	return
endif else sel=where(sel eq 0,ns) 

; output the list of duplicated sources
rslist=slist(rsel)
rslist.sn=indgen(nrsel)+1
sou_struct_fits,rslist,outrfile
rsrs=srs(rsel)

; output the list of selected sources
slist=slist(sel)
slist=slist(sort(slist.ra))
slist.sn=indgen(ns)+1
sou_struct_fits,slist,outfile
sou_struct_out,slist,text
sou_struct_out,rslist,rtext
if n_elements(outdfile) ne 0 then openw,unoutd,outdfile,/get_lun
ndup=intarr(ns)
for k=0,ns-1 do begin
	; now find all the duplicated sources associated with the unique one
	trans_dist,sra(k),sdec(k),rslist.ra,rslist.dec,px,py,/deg,/das
	sep=(px^2+py^2)
	seperr=rsrs+srs(k)+sperr^2
	ser=spsigma*seperr
	rseln= where((sep lt ser), nsrsel)  
	if nsrsel ne 0 then begin
		;calculate the mean cntr and its error as well as chi^2, ndf
		avg_least,[slist(k).cntr,rslist(rseln).cntr], $
			[slist(k).cntre,rslist(rseln).cntre] $
			,cntrs,cntres,chi=chi,ndf=ndf
		;include these new parameters in the output
		text(k)=text(k)+strtrim(cntrs,2)+' | '+strtrim(cntres,2) $
		+' | '+strtrim(chi,2)+' | '+strtrim(ndf,2)
	 	texts=' r '+rtext(rseln)+' | ' $
		+strtrim(sqrt(ser(rseln)),2)+' | '+strtrim(sqrt(sep(rseln)),2)
		print,text(k)
		for n=0,nsrsel-1 do print,texts(n)
		if n_elements(outdfile) ne 0 then begin
			printf,unoutd,text(k)
			for n=0,nsrsel-1 do printf,unoutd,texts(n)
		endif
		ndup(k)=nsrsel
	endif
endfor
if n_elements(ndupname) eq 0 then ndupname='NDUP'
struct_col_add,slist,ndup,[ndupname],0*[1]
print,'The number of sources is ',ns
free_lun,unoutd
return
end 
 
