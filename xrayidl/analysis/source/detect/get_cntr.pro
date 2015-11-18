pro shift_sou,spx,spy,cntr,cntre,emin=emin,emax=emax,block=block,rs=rs,rb1=rb1,rb2=rb2,obsfile=obsfile,xshift=xshift,yshift=yshift,ashift=ashift,tfile=tfile

if n_elements(emin) eq 0 then emin=0
if n_elements(emax) eq 0 then emax=0
if n_elements(block) eq 0 then block=1
if n_elements(rs) eq 0 then rs=0.2
if n_elements(rb1) eq 0 then rb1=1.25*rs
if n_elements(rb2) eq 0 then rb2=rb1*2
ns=n_elements(spx)
rs=fltarr(ns)+rs
if n_elements(t) ne 0 then begin
	sz=size(t) 
dim=nint(2*rb2*60/(block*!size_pixel)*1.5) 
	;the factor 1.5 is for possible sources lying nearby

if n_elements(obsfile) eq 0 then obsfile=!seq_no+'_obstime.dat'

for n=0,ns-1 do begin
 getlistimage,l,dim=dim(n),xc=spx(n),yc=spy(n),emin=emin,emax=emax $
  ,block=block,/xyt,xmin=xmin,ymin=ymin $
	,xshift=xshift,yshift=yshift,ashift=ashift,tfile=tfile
 if n eq 0 or (n gt 0 and rs(n) ne rs(n-1 > 0)) then $
	get_filter,spx,spy,t,ts,block=block,sra=sra,sdec=sdec $
	,radius=radius,cra=cra,cdec=cdec,slow=slow,flow=flow $
	,subvalue=subvalue,blow=blow,bhigh=bhigh,factor=factor $
	,pixcenter=pixcenter,soufile=soufile
;

	filter_sp,-1,-1,fltarr(dim(n),dim(n))+1,ts,block=block,rs=rs(n) $
		,rb1=rs(n),rb2=rs(n)*1.4 ;no real use
 list_shift,l,ls,shiftlimit=shiftlimit $
	,xshift=xshift,yshift=yshift,ashift=ashift
 list_image,ls,xmin,ymin,c,dim(n),block=block
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


