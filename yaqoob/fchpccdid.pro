pro fchpccdid,sistyp
if n_params(0) eq 0 then begin
 print,'fchpccdid,sistyp'
 print,'Find ranges of RAW 4 chip coords for each SIS chip'
 print,'SISTYP	: = 0 for S0, = 1 for S1 '
 retall
end
nxr=418l & nyr=420l & nxd=1280l & nyd=1280l
;lower edge of chip
xl=findgen(420)+2. & yl=fltarr(420)+7.
;upper edge of chip 
xu=xl & yu=fltarr(420)+424.
;left side of chip
xsl=fltarr(418)+2. & ysl=findgen(418)+7.
;right side of chip
xsr=fltarr(418)+421. & ysr=findgen(418)+7.
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
for k=0,3 do begin
 ccd=intarr(nxr*nyr)+k
 raw2new,xchip,ychip,fxchip,fychip,sistyp,ccd
 print,'Chip ',k
 print,'X - range ',minmax(fxchip)
 print,'Y - range ',minmax(fychip)
endfor
return
end
