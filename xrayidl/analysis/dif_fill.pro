pro dif_fill,tim,cims,tims,cimf,cbm,cth=cth,edim=edim

if n_elements(cth) eq 0 then cth=16.
sz=size(tim)
if sz(0) eq 2 then nim=1 else nim=sz(3)
xdim=sz(1)
ydim=sz(2)
bim=tim(*,*,0)*0.+1.e-10 ;so that it is small enough
cbm=tim*0.
cimf=tim*0.
for k=0,nim-1 do begin
	fac=fix(alog(sqrt(cth/avg(cims(*,*,k))))/alog(2.))+1 
		;index of a factor of 2
	fac=2^fac
	print,'fac = ',fac
	if n_elements(edim) ne 0 then begin
		cs=intarr(edim(0),edim(1))
		tss=fltarr(edim(0),edim(1))
		cs(0:xdim-1,0:ydim-1)=cims(*,*,k)
		tss(0:xdim-1,0:ydim-1)=tims(*,*,k)
	endif else begin
		cs=cims(*,*,k)
		tss=tims(*,*,k)
	endelse
	cs=image_comp(cs,1./fac)
	tss=image_comp(tss,1./fac)
	image_median,7,imdiv(cs,tss),bim,tss,mf,lmin=9
	fm=image_comp(mf,fac)/fac^2
	if n_elements(edim) ne 0 then fm=fm(0:xdim-1,0:ydim-1)
	tdif=tim(*,*,k)-tims(*,*,k)
	ran_pos,where(tdif gt 0.),fm*tdif,0,0,xs1,ys1,block=1
	list_image,l,0,0,rim,sz(1),sz(2),block=1,xp=xs1,yp=ys1
	cimf(*,*,k)=cims(*,*,k)+rim
	cbm(*,*,k)=fm*tim(*,*,k)
endfor
return
end
