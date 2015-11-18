pro testraw,dname,fname,sistyp,xr,yr,ccd,x,y
if n_params(0) eq 0 then begin
 print,'testraw,dname,fname,sistyp,xr,yr,ccd,x,y'
 retall
endif
openr,1,fname
i=0
ifirst=0
while not eof(1) do begin
 name=' '
 readf,1,name
 fitsname=dname+name
 hdr=headfits(fitsname)
 nevents=sxpar(hdr,'NEVENTS')
 if nevents gt 0 then begin
 tab=readfits(fitsname,h,ext=1)
  print,'read file',i+1
  rawx=tbget(h,tab,'RAWX')
  rawy=tbget(h,tab,'RAWY')
  ccdid=tbget(h,tab,'CCDID')
  if ifirst eq 0 then begin
   xr=rawx & yr=rawy & ccd=ccdid
   ifirst=1
  endif
  if ifirst eq 1 then begin
   xr=[xr,rawx] & yr=[yr,rawy] & ccd=[ccd,ccdid]
  endif
 endif
 i=i+1
endwhile
raw2new,xr,yr,x,y,sistyp,ccd
close,1
return 
end
