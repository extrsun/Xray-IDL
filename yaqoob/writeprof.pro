pro writeprof,inst,chip,cts,xcen,ycen,rlo,rhi,rtr,chlo,chhi,sismap=sismap,outfile=outfile
if n_params(0) eq 0 then begin
 print,'writeprof,inst,chip,cts,xcen,ycen,rlo,rhi,rtr,chlo,chhi,sismap=sismap,outfile=outfile'
 print,'Write out radial profile from ASCA to a QDP file '
 print,'This program is intended for use with the output of CTPROF'
 print,'**INPUTS** '
 print,'INST 	- =0,1,2,3 for S0,S1,S2,S3 '
 print,'CHIP	- SIS chip ID'
 print,'CTS(nrad,npha)	- number of counts in per radial and energy'
 print,'XCEN,YCEN 	- centre in pixels '
 print,'RLO,RHI 	- Boundaries of radial bins: the lowest boundary '
 print,'	- r=0 is implied. '
 print,'RTR	- Transition radius. Below RTR area is computed'
 print,'     	- analytically '
 print,'CHLO,CHHI 	- channel boundaries '
 print,'SISMAP  - SIS exposure map [e.g. from mkchipmap or sisexp'
 print,'OUTFILE - Name of output QDP file to sump results to '
 retall
end
if inst le 1 and n_elements(sismap) eq 0 then begin
 sismap=' '
 read,"Enter name of SIS expourse map (incl path) ',sismap
endif
if n_elements(outfile) eq 0 then begin
 outfile=' '
 read,'Enter name of output file ',outfile
endif
nrad=(size(rlo))(1)
if inst ge 2 and le 3 then begin
 annuli=fltarr(nrad)
 for j=0,nrad-1 do annuli(j)=!pi*(rhi(j)*rhi(j)-rlo(j)*rlo(j))
endif
if inst le 1 then begin
 im=readfits(sismap,h)
 rhip=rhi/0.027 & rtrp=rtr/0.027 
 sisannuli,chip,xcen,ycen,rhip,rtrp,im,annuli 
 annuli=annuli*.027*.027
endif
openw,1,outfile
printf,1,'!Radial profile for ASCA instrument ',inst
if inst le 1 then printf,1,'!Chip ',chip
printf,1,'SKIP SINGLE '
printf,1,'!Following are the radial bins with their annulus areas '
printf,1,'!Units: mm and mm^2 '
rcen = 0.5*(rlo+rhi) & rwid=(rhi-rlo)
for k=0,nrad-1 do printf,1,rcen(k),rwid(k),annuli(k)
printf,1,'no no no '
npha=(size(chlo))(2)
for j=0,npha-1 do begin
printf,1,'! Number of counts per bin in channels ',chlo(inst,j),chhi(inst,j)
for k=0,nrad-1 do printf,1,rcen(k),rwid(k),cts(k,j)
endfor
return
end
