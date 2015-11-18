pro remkgspec,evname,specname,dir=dir,gdir=gdir,gfile=gfile
if n_params(0) eq 0 then begin
 print,'remkgspec,evname,specname,dir=dir,gdir=gdir,gfile=gfile'
 print,'Re-make a GIS spectrum given a spectral file SPECNAME '
 print,'made from the exact corresponding event file EVNAME '
 print,'The only difference between the old and new spectral files'
 print,'is that PHA2PI is re-run on the events, with the temporal'
 print,'gain correction included.'
 print,'The output spectral file has the same roortname, with _gc.pha'
 print,'DIR	- directory containing gain maps'
 print,'GDIR	- directory containing gain history file'
 print,'GFILE	- name of gain history file'
 retall
end
qreadasca,evname,plist,h1,gti,ta1b
istr=sxpar(h1,'INSTRUME')
gis=0
if strmid(istr,0,4) eq 'GIS2' then gis=2 
if strmid(istr,0,4) eq 'GIS3' then gis=3 
if gis lt 2 then begin
 print,' ** NOT GIS FILE **'
 return
end
posdet=0
dot = strpos(specname, '.')
outname = strmid(specname, 0, dot) + '_gc.pha'
;read 3 headers from the spectral file
wmap=readfits(specname,sh0)
sh1=headfits(specname,ext=1)
sh2=headfits(specname,ext=2)
PHA2PI,plist,xlin,ylin,posdet,gis,dir=dir,gdir=gdir,gfile=gfile,/tcor
;now write the new file
spec=histogram([0,plist.pi],max=1023,binsize=1)
wrtfitspec,sh0,sh1,sh2,spec,gti,wmap,specname=outname,hdr,hdr1,hdr2
return
end
