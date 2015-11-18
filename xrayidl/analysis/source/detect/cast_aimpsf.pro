pro cast_aimpsf,flist,mh,aimpsf,ta,bv=bv,fdir=fdir $
,perclimit=perclimit,mtype=mtype,ftail=ftail,inr=inr,outr=outr,inarr=inarr
;+
; create merged psf and exposure maps from instrument maps
;
;*INPUTS:
; flist - file list: each row contains a file name (or filehead so that
;		file name = filehead+ftail		
; mh - the header of the merged map, which can be produced with get_fitshead
;        time). 
;*OUTPUTS:
; aimpsf - stacked psf images
;*OPTIONAL Inputs:
; bv - selecting vector containing band numbers (def =[1,2,3,4])
; ftail - characters appended to the filenames in filelist (e.g., '_i')
; fdir - the directory in which the instrument maps are located
;		(def ='')
; perclimit - energy-encirlced fraction of the psf (def =0.9)
; mtype - if =1, the merged coordinates are assumed to be Galactic
; inr,outr - If given, only regions within the inner and outer radii
;            will be selected (def = 0, 1.e22 in units of arcmin)
;
; written by wqd, 4/5/2006
;
;-
npara=n_params()
if npara eq 0 then begin
print,'Calling Seq. - cast_aimpsf,flist,mh,aimpsf,bv=bv,fdir=fdir'
print,',perclimit=perclimit,mtype=mtype,ftail=ftail'
print,',inr=inr,outr=outr,inarr=inarr'
return
endif
if n_elements(perclimit) eq 0 then perclimit=0.9
if n_elements(mtype) eq 0 then mtype=0 
if n_elements(bv) eq 0 then bv=[1,2,3,4]
if n_elements(ftail) eq 0 then ftail='_i'
nbt=n_elements(bv)
tref=fltarr(nbt)
dim=sxpar(mh,'naxis*')
aimpsf=fltarr(dim(0),dim(1),nbt)
if npara gt 3 then ta=fltarr(dim(0),dim(1),nbt)
for k=0,nbt-1 do begin
    cast_psf_main,flist,mh,impsf,ftail=ftail,mtype=mtype,fdir=fdir $
                  ,eband=bv(k),perclimit=perclimit,/difexp $
                  ,inr=inr,outr=outr
    cast_psf_main,flist,mh,t,ftail=ftail,mtype=mtype,fdir=fdir,eband=bv(k) $
                  ,inr=inr,outr=outr,inarr=inarr
    aimpsf(*,*,k)=imdiv(impsf,t)
    if npara gt 3 then ta(*,*,k)=t
if !debug eq 3 then stop
endfor
aimpsf=sqrt(aimpsf) 
sel=where(aimpsf gt 0. and aimpsf lt 1.,nsel)
if nsel ne 0 then aimpsf(sel) = aimpsf(sel) > 1. ;greater than 1 pixel!!!
return
end
