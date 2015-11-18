pro sp_group,sfname,bfname,ch,ff,ffe,chl,chh,ctonth=ctonth,fout=fout $
	,fnorm=fnorm,chlow=chlow,chhigh=chhigh,bchi=bchi,chmin=chmin
;+
;
; Adaptively calculate the boundary channel numbers for a constant 
; signal to noise ratio spectrum
;
;*Input
; sfname, bfname - names of source and background files, which should not 
; be grouped. Otherwise, the resolution is not optimized.
;
;*optional output:
; ch - mean channel number of the output spectrum bins
; ff, ffe - output spectrum and its erro bars
; chl, chh - the lower and upper channel numbers of the output spectrum bins
;
; ctonth - the signal to noise threshold; def=3
; fnorm - adjusting factor for the background normalization; def= 1.
;	This parameter is useful when background needs to be scaled by a factor
;	(fnorm > 1, higher).
; fout - the output file name (to be used by grppha for example)
; 	which can be used in the program grppha for grouping.
; chlow, chhigh - the lower and upper limits of the channel interval within
;		which, the spectrum is to be calcualated. The boundaries 
;		affect in particular the lowest and highest spectral bins
;		Def =min(sc) and max(sc). For example, the gcs survey has
;		a lower energy cutoff of 1 keV. Thus, chlow=69 and chhigh=
;		547 are suggested. The arf file gives the correspondace between
;		the channel numbers and energies.
; bchi - if set, two (background) spectra are just compared vie a chi^2 test.
; chmin - 0 for SAS and FTOOL processed data; 1 for CIAO data (def)
; 
;
;*Note
; the keywords, exposure and backscal in the fits headers of the source and 
; background files must have been defined correctly.
;
;*Example:
; sp_group,'halo2.pi','bgnd_halo2.pi',cton=3,fout='test.grp'
;
; written by wqd, Feb 22, 2001
; modified to include the chlow and chhigh. wqd, May 7, 2002
;
;
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- sp_group,sfname,bfname,ch,ff,ffe,chl,chh'
print,',ctonth=ctonth,fout=fout,fnorm=fnorm,chlow=chlow,chhigh=chhigh,bchi=bchi'
return
endif

if n_elements(fnorm) eq 0 then fnorm=1.
; input source data
tab=readfits(sfname,ext=1,hdr)
snrow=sxpar(hdr,'naxis2')
sc=tbget(hdr,tab,'counts',lindgen(snrow))
sexpt=sxpar(hdr,'exposure',count=count)
if count ne 1 then stop,'stop: count not = 1'
sarea=double(sxpar(hdr,'backscal',count=count))
if count ne 1 then stop,'stop: count not = 1'

; input background data
tab=readfits(bfname,ext=1,hdr)
bnrow=sxpar(hdr,'naxis2')
if bnrow ne snrow then stop,'stop: snrow ne bnrow!'
;bc=tbget(hdr,tab,'rate',lindgen(bnrow))
bexpt=sxpar(hdr,'exposure',count=count)
fxbfind,hdr,'TTYPE',COLUMNS, VALUES, N_FOUND
match,strtrim(values,2),'COUNTS',a,b,count=count
if count ne 0 then begin
    bc=tbget(hdr,tab,'counts',lindgen(bnrow))
endif else begin
    br=tbget(hdr,tab,'rate',lindgen(bnrow))
    bc=bexpt*br
endelse
if count ne 1 then stop,'stop: count not = 1'
barea=double(sxpar(hdr,'backscal',count=count))
if count ne 1 then stop,'stop: count not = 1'
;bc=bexpt*br
;normalization of the background into the source region with the 
; correct exposure
bnorm=fnorm*sexpt*sarea/(bexpt*barea)
if keyword_set(bchi) ne 0 then begin
  nch=n_elements(bc)
  cb=0 &  bb=0
  kk=0
  ;works to here!
  for k=0L, nch-1 do begin

    cb=cb+sc(k)
    bb=bb+bc(k)
        
    if bb gt ctonth then begin 
      if kk eq 0 then begin
        cbg=float(cb)
        bbg=float(bb)
      endif else begin
        cbg=[cbg,cb]
        bbg=[bbg,bb]
      endelse 
      cb = 0 & bb=0 
      kk=kk+1
    endif
    ;        print, k
  endfor
  print, "point 2"
  eeg=cbg+bnorm^2*bbg
  norm=total(cbg*bbg/eeg)/total(bbg^2/eeg)/bnorm
  chi2=total((cbg-norm*bnorm*bbg)^2/eeg)
  print, "point 3"
  print,'chi2 and dof = ',chi2,nch-1

  eeg=cbg+(bnorm*norm)^2*bbg
  chi2=total((cbg-norm*bnorm*bbg)^2/eeg)
  print,'chi2 and dof = ',chi2,nch-1
  print,'bexpt and norm =', bexpt, norm
  print,'corrected bexpt should be ',norm*bexpt
endif else begin
  ;calculate the grouping and output into the file.
  ;get_group,sc,bc*bnorm,ch,ff,ffe,chl,chh,ctonth=ctonth,fout=fout $
    get_group,sc,bc,ch,ff,ffe,chl,chh,ctonth=ctonth,fout=fout $
      ,chlow=chlow,chhigh=chhigh,bnorm=bnorm,chmin=chmin
endelse

;-------------------------------
return
end
