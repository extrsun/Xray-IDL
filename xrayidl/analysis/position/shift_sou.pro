pro shift_sou,spx,spy,shiftx,shifty,emin=emin,emax=emax,block=block,rs=rs,obfile=obfile,obsfile=obsfile,xshift=xshift,yshift=yshift,ashift=ashift

if n_elements(emin) eq 0 then emin=0
if n_elements(emax) eq 0 then emax=0
if n_elements(block) eq 0 then block=1
if n_elements(rs) eq 0 then rs=0.25
ns=n_elements(spx)
rs=fltarr(ns)+rs
dim=nint(2*rs*60/(block*!size_pixel))

if n_elements(obfile) eq 0 then obfile=!seq_no+'_obtime.dat'
	openr,unit,obfile,/get
	readf,unit,nob
	tint=lonarr(2,nob)
	for n=0,nob-1 do begin
		readf,unit,nn,t1,t2
		tint(0,n)=t1
		tint(1,n)=t2
	endfor
	free_lun,unit

for n=0,ns-1 do begin
 getlistimage,l,dim=dim(n),xc=spx(n),yc=spy(n),emin=emin,emax=emax $
  ,block=block,/xyt,xmin=xmin,ymin=ymin $
	,xshift=xshift,yshift=yshift,ashift=ashift
 if n eq 0 or (n gt 0 and rs(n) ne rs(n-1 > 0)) then $
	filter_sp,-1,-1,fltarr(dim(n),dim(n))+1,ts,block=block,rs=rs(n) $
		,rb1=rs(n),rb2=rs(n)*1.4 ;no real use

 spec_data_dif,l,ts,xmin,ymin,ls,block=block,/noimage
 list_center,ls,xobm,yobm,cob,obfile=obfile,plot=plot,tint=tint,xmv=xms,ymv=yms
 if n eq 0 then begin
	nob=n_elements(xobm)
	xobms=fltarr(ns,nob)
	yobms=xobms
	cobs=xobms
	xmv=fltarr(ns)
	ymv=xmv
 endif
 xobms(n,*)=xobm
 yobms(n,*)=yobm
 cobs(n,*)=cob
 xmv(n)=xms
 ymv(n)=yms
endfor
nc=total(cobs,1)
;shiftx=total(cobs*xobms,1)/nc
;shifty=total(cobs*yobms,1)/nc
shiftx=fltarr(nob)
shifty=shiftx
shifta=shiftx
;
xmv=xmv-4095.5
ymv=ymv-4095.5
for n = 0,nob-1 do begin
  if ns ge 2 then begin
	offset_fit,xobms(*,n)+xmv,yobms(*,n)+ymv,xmv,ymv,cobs(*,n),val
	shiftx(n)=val(0)
	shifty(n)=val(1)
	shifta(n)=val(2)
  endif else begin
	avg_median,xobms(*,n),shx
	avg_median,yobms(*,n),shy
	shiftx(n)=shx & shifty(n)=shy
  endelse
;	print,'nob = ',n
;	print,'shiftx,xobms = ', shiftx(n),xobms(*,n)
;	print,'shifty,yobms = ',shifty(n),yobms(*,n)
	print,'shiftxya = ',shiftx(n),shifty(n),shifta(n)
;	print,'nc,cobs = ',nc(n),cobs(*,n)
endfor
if n_elements(obsfile) eq 0 then obsfile=!seq_no+'_obstime.dat'
openw,unit,obsfile,/get
printf,unit,nob
for n=0,nob-1 do printf,unit,tint(0,n),tint(1,n),shiftx(n),shifty(n),shifta(n),nc(n)
free_lun,unit
end


