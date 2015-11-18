pro sisexp,sis,gti,dir=dir,satfile=satfile,hotfile=hotfile,expname=expname
;Author T. Yaqoob - March 1993 ->**
if n_params(0) eq 0 then begin
 print,'Create exposure maps for each SIS chip '
 print,'sisexp,sis,gti,dir=dir,satfile=satfile,hotfile=hotfile,expname=expname'
 print,'SIS	:=0 for S0, =1 for S1'
 print,'DIR	:Name of dir containing SIS cal files'
 print,'GTI(*,2):Time intervals between which to compute map'
 print,'satfile = output file from TELEMGTI '
 print,'hotfile = input hot pixel list '
 print,'expname = rootname of exposure map FITS files '
 retall
end
if n_elements(satfile) eq 0 then begin
 satfile=' '
 read,'Enter name of input telemetry saturation file from TELEMGTI ',satfile
endif
if n_elements(expname) eq 0 then begin
 expname=' '
 read,'Enter name of output exposure map FITS file ',expname
endif
enorm=0.0
;stuff for SIS linearization
rf=0 & units=1 & screw=2
;image array stuff
nxr=418l & nyr=420l & nxd=1280l & nyd=1280l
;set up values of x and y covering a whole chip
xchip=intarr(nyr)+7
ychip=indgen(nyr)+2
ytemp=indgen(nyr)+2
for i=8,424 do begin
  xtemp=intarr(nyr)+i
  xchip=[xchip,xtemp]
  ychip=[ychip,ytemp]
endfor
xm=424 & ym = 421 & xm1=xm-1
nun=0l & nsa=0l 
print,' Reading Telemetry Saturation File ...'
readcol,satfile,ccdi,xsi,ysi,timi
nhot=0l
if n_elements(hotfile) gt 0 then begin
 print,' Reading Hot Pixel File ...'
 readcol,hotfile,chot,xhot,yhot
 nhot=(size(xhot))(1)
 print,'Total of ',nhot,' Hot Pixels '
endif
;only use telemtry frames which lie within the give gti intervals
ngt=(size(gti))(1)
ntot=0l & ifirst=0
if ngt le 0l then begin
 print,'No GTI constraints '
 ccd=ccdi & xs=xsi & ys=ysi
endif
if ngt gt 0l then begin
for n=0l,ngt-1l do begin
 wev=where((timi ge gti(n,0) and timi le gti(n,1)),nwev)
 print,nwev,' Frames in GTI No. ',n+1
 ntot=ntot+nwev
 if nwev gt 0 then begin
  ccdtemp=ccdi(wev)
  xstemp=xsi(wev)
  ystemp=ysi(wev)
  if ifirst gt 0 then begin
	ccd=[ccd,ccdtemp]
	xs=[xs,xstemp]
 	ys=[ys,ystemp]
  endif
  if ifirst eq 0 then begin
	ccd=ccdtemp & xs=xstemp & ys=ystemp & ifirst=1
  endif
 endif
endfor
print,'Total number of frames in GTIs = ',ntot 
endif
for j=0, 3 do begin
 print,'Doing chip ',j
 expmap=fltarr(xm,ym)
 wc=where((ccd eq j),nwc)
 if nwc gt 0 then begin
	x=xs(wc) & y=ys(wc) & xpy=x+y
;how many unsaturated frames?
	wun=where((xpy eq 0),nun)
  	print,nun,' unsaturated frames '
;how many saturated frames?
	wsa=where((xpy ne 0),nsa)
	print,nsa,' SATURATED frames '
	if nun gt 0 then expmap=expmap+float(nun) 
 if nsa gt 0 then begin
  xsat=x(wsa) & ysat=y(wsa)
  for k=0l,nsa-1 do begin
    expmap(0:xm1,0:(ysat(k)-2))=expmap(0:xm1,0:(ysat(k)-2))+1.0
    expmap(0:(xsat(k)-1),(ysat(k)-1):(ysat(k)-1))= $
	expmap(0:(xsat(k)-1),(ysat(k)-1):(ysat(k)-1))+1.0
  endfor
 endif
 endif
 if nhot gt 0 then begin
  whc=where((chot eq j),nwhc)
  print,'Marking ',nwhc,' Hot Pixels on Chip ',j
  if nwhc gt 0 then begin
   hx = xhot(whc) & hy=yhot(whc)
   expmap(hx,hy) = 0l
  endif
 endif
 tempname='c'+strtrim(string(j),2)+'.exp'
 writefits,tempname,long(expmap)
 enorm=max([max(expmap),enorm])
endfor
im=lonarr(nxd,nyd)
for j=0,3 do begin
 tempname='c'+strtrim(string(j),2)+'.exp'
 expmap=float(readfits(tempname,h))
 ccd=intarr(nxr*nyr)+j
 sisdet,xchip,ychip,ccd,sis,units,rf,screw,xdet,ydet,dir=dir
; enorm=max(expmap(xchip-1,ychip-1))
 if enorm gt 0 then im((xdet-1),(ydet-1))=$
 long(10000.*((10.*float(1+j))+(expmap(xchip-1,ychip-1)/enorm)))
 if enorm le 0 then im((xdet-1),(ydet-1))=100000l*(1+j)
endfor
;note the value of the exposure is designed to identify the chip as
;well. The value in im is ((1+ccdid)*10+efrac)*10000.
;thus for chips 0-3 the values are 100000-110000 etc.Thus the chip id
;can be recovered by (value/100000l)-1; the exposure fraction can be
;recovered by float(val)/10000.-(10.*[1+ccid])
writefits,expname,im
im2=rebin((im-100000l*(im/100000l)),320,320)
writefits,expname+'map',im2
return
end
