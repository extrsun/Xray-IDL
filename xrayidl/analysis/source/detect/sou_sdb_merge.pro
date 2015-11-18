pro sou_sdb_merge,evtfname,outfile,slist=slist,nbv=nbv,fmid=fmid,fstail=fstail,dfac=dfac,sradius=sradius,probth=probth,psfrfac=psfrfac
;+
; Merge source files in various bands
;
;*INPUTS:
; evtfname - event file name head (e.g., acisf02292N001_evt2)
;
;*OUTPUTS:
; outfile - file name for the output parameters in a LaTex format
; slist - output source structure
;
;*OPTIONAL Inputs:
; nbv - source detection bands 
;	(['B','S','H','S1','S2','H1','H2']; def=['B','S','H'])
; fmid - the string that is added to evtfname (def ='_map70')
; ftail - characters appended to the filenames in filelist (def='_mlm')
; dfac - the factor multiplied to the source error radius (def =1).
; sradius - systematic error (minimum radius) of the source positions
;	in units of arcsec. Set sradius=0 equivalent to no systematic error
; psfrfac - factor used (when sradius =0) to scale the psf radius for
;           the systematic uncertainties in the merge. (def=0.4)
; 
; written by wqd 5/15/2002
; add psfrfac, 6/30/2005
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE -  sou_sdb_merge,evtfname,outfile,slist=slist,nbv=nbv'
print,',fmid=fmid,fstail=fstail,dfac=dfac,sradius=sradius,probth=probth,psfrfac=psfrfac'
return
endif
if n_elements(fmid) eq 0 then fmid='_map70'
if n_elements(ftail) eq 0 then ftail='_mlm'
if n_elements(nbv) eq 0 then nbv=['B','S','H']

froot=evtfname+fmid
nb=n_elements(nbv)
snew=1
for kk=0,nb-1 do begin
;	kk=sbv(k)
	fname=froot+nbv(kk)+ftail
	print,fname
	sou_fits_info,fname,slist,/all,nsel=nrow,probth=probth
	;nrow=n_elements(slist)
if !debug eq 2 then stop
	if nrow ne 0 then begin
		if snew then begin
		 struct_col_add,slist,replicate(nbv(kk),nrow),'SDB','' $
			,struct_temp=struct_temp
		 slistn=slist
		 snew=0
		endif else begin
			struct_col_add,slist,replicate(nbv(kk),nrow) $
				,struct_temp=struct_temp
			slistn=[slistn,slist]
		endelse
	endif
    endfor
if n_elements(sradius) eq 0 then begin
    if n_elements(psfrfac) eq 0 then psfrfac=0.4
    sradius=slistn.sradius*!size_pixel*psfrfac
endif
;stop
sou_merge_fits,'',outfile,slist=slistn,outslist=slist,probth=probth,dfac=dfac $
	,sradius=sradius,/sdb ;,radius=1.5
if !debug eq 2 then stop
return
end

