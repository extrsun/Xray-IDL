pro ascaeef,theta0,r0,dir=dir,eefname=eefname,outeef,avgeef  
if n_params(0) eq 0 then begin
 print,'ascaeef,theta0,r0,dir=dir,eefname=eefname,outeef,avgeef'
 retall
end
if n_elements(dir) eq 0 then begin
 dir=' '
 read,'Enter name of directory containing XRT EEF file ',dir
endif
if n_elements(eefname) eq 0 then begin
 eefname=' '
 read,'Enter name of XRT EEF file ',eefname
endif
nrad=n_elements(r0) & avgeef=fltarr(nrad,4)
r0in=fltarr(1) 
theta0in=fltarr(1) & theta0in(0)=theta0
fname=dir+eefname
tab=readfits(fname,h,ext=1)
elo=tbget(h,tab,'ENERG_LO')
radlo=tbget(h,tab,'RAD_LO')
theta=tbget(h,tab,'THETA')
phi=tbget(h,tab,'PHI')
eef=tbget(h,tab,'EEF')
nen=(size(elo))(1) 
nt=(size(theta))(1)
np=(size(phi))(1)
nr=(size(radlo))(1)
eefr=reform(eef,nr,nt,np,nen)
outeef=fltarr(nen,np)
for i=0l,nrad-1 do begin
r0in(0)=r0(i)
for k=0l,np-1 do begin
 for j=0l,nen-1 do begin
tg=where((theta0 ge theta),ntg)
tl=where((theta0 lt theta),ntl)
if ntg gt 0 then itlo=max(tg)
if ntl gt 0 then ithi=min(tl)
 yin1=eefr(0:nr-1,itlo:itlo,k:k,j:j)
 yin2=eefr(0:nr-1,ithi:ithi,k:k,j:j)
 fint,radlo,yin1,r0in,yout1
 fint,radlo,yin2,r0in,yout2
 xin3=[theta(itlo),theta(ithi)]
 yin3=[yout1,yout2]
 fint,xin3,yin3,theta0in,yout3
 outeef(j,k)=yout3
; print,phi(k),elo(j),yout3
 endfor
endfor
;compare the eef for low and high energies and for 2 values of phi
avgeef(i,0)=avg(outeef(0:9,0:0)) & avgeef(i,1)=avg(outeef(90:99,0:0))
avgeef(i,2)=avg(outeef(0:9,1:1)) & avgeef(i,3)=avg(outeef(90:99,1:1))
endfor
;compare the eef for low and high energies and for 2 values of phi
;plot,elo,outeef(0:nen-1,0:0),xtype=1,ytype=1
;oplot,elo,outeef(0:nen-1,1:1)
return
end
