pro get_group,sc,bc,ch,ff,ffe,chl,chh,ctonth=ctonth,fout=fout $
	,chlow=chlow,chhigh=chhigh,bnorm=bnorm,chmin=chmin
;+
;
; calculate the boundary channel numbers for a constant signal to noise ratio
; spectrum
;
; sc - count vector for the source (including background)
; bc - expected background count vector
; ch - mean channel number of the output spectrum bins
; ff, ffe - output spectrum and its erro bars
; chl, chh - the lower and upper channel numbers of the output spectrum bins
; ctonth - the signal to noise threshold; def=3
; fout - the output file name (to be used by grppha for example)
; 	which can be used in the program grppha for grouping.
; chlow, chhigh - the lower and upper limits of the channel interval within
;		which, the spectrum is to be calcualated. The boundaries 
;		affect in particular the lowest and highest spectral bins
;		Def =min(sc) and max(sc)
; bnorm - background normalization for the subtraction
; chmin - 0 for SAS and FTOOL processed data; 1 for CIAO data (def)
;
; written by wqd, April 28, 2000
;
; modified to include the chlow and chhigh. wqd, May 7, 2002
;
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- get_group,sc,bc,ch,ff,ffe,chl,chh'
print,',ctonth=ctonth,fout=fout,chlow=chlow,chhigh=chhigh,bnorm=bnorm'
return
endif
nch=n_elements(sc)
if n_elements(chmin) eq 0 then begin
    chmin=1
    print,'Minmum channel number is assumed to be 1 (for Chandra spectra)
    print,'otherwise, chmin=0 should be used!!!!'
endif
get_hist_var,lindgen(nch)+chmin,sc,bc,replicate(1.,nch),ch,chl,chh,ff,ffe,t,cton=ctonth,dlow=chlow,dhigh=chhigh,bnorm=bnorm
gl=[-999]
gh=[-999]
if !debug eq 1 then stop
chh=fix(chh)
nch=n_elements(chl)
;chl=nint(chl)
chl=fix(chl)
chl(1:nch-1)=chh(0:nch-2)+1

;

;ng=n_elements(ch)
;chh=[chl(1:ng-1)-1,nch]
chd=chh-chl+1
sel=where(chd ne 1,nsel)
print,'channellow channelhigh groupfactor '
for k=0,nsel-1 do begin
	kk=sel(k)
	print,chl(kk),chh(kk),fix(chd(kk))
	; the output is to be used in grppha for grouping.
endfor
if n_elements(fout) ne 0 then begin
 openw,un,fout,/get
 for k=0,nsel-1 do begin
	kk=sel(k)
	printf,un,chl(kk),chh(kk),fix(chd(kk))
 endfor
 free_lun,un
endif
; ploterr,ch,ff,ffe
if !debug eq 1 then stop
return
end



