pro mkchipmap,sistyp,dir=dir,mapname=mapname
;Author T. Yaqoob - March 1993->**
if n_params(0) eq 0 then begin
 print,'mkchipmap,sistyp,dir=dir,mapname=mapname'
 print,'Make an SIS chip map in Human detector coords '
 print,'Think of it as dummy exposure map '
 print,'SISTYP	: = 0 for S0, = 1 for S1 '
 print,'DIR	: directory containing SIS cal file '
 print,'MAPNAME : Name of output FITS image file containing the map '
 print,'SIS calfile names Hardwired: s0(s1)_alignment_ascalin.fits '
 retall
end
if n_elements(dir) eq 0 then begin
 dir=' '
 read,'Name of directory containing SIS cal file '
endif
if n_elements(mapname) eq 0 then begin
 mapname=' '
 read,'Enter Name of output FITS chip map '
endif
nxr=418l & nyr=420l & nxd=1280l & nyd=1280l
;lower edge of chip
xl=findgen(420)+2. & yl=fltarr(420)+7.
;upper edge of chip 
xu=xl & yu=fltarr(420)+424.
;left side of chip
xsl=fltarr(418)+2. & ysl=findgen(418)+7.
;right side of chip
xsr=fltarr(418)+421. & ysr=findgen(418)+7.
window,0,xsize=900,ysize=900
rf=0 & screw=2 & units=1
xmin=fltarr(4) & ymin=fltarr(4) & xmax=xmin & ymax=ymin
;im=intarr(1280,1280)
;complete set of raw x and y s for a chip
xchip=intarr(nyr)+7
ychip=indgen(nyr)+2
ytemp=indgen(nyr)+2
for i=8,424 do begin
  xtemp=intarr(nyr)+i
  xchip=[xchip,xtemp]
  ychip=[ychip,ytemp]
endfor 
im=lonarr(nxd,nyd)
for k=0,3 do begin
 ccd=intarr(nxr*nyr)+k
 sisdet,xchip,ychip,ccd,sistyp,units,rf,screw,xdet,ydet,dir=dir
	im((xdet-1),(ydet-1))=(k+1)*100000l+10000l
 if k eq 0 then plot,xdet,ydet,xr=[0,nxd],yr=[0,nyd],psym=3
 if k gt 0 then oplot,xdet,ydet
endfor
writefits,mapname,im
return
end
