pro hpxsmo,im1,smo,sigma,xyl
if n_params(0) eq 0 then begin
 print,'hpxsmo,im1,smo,sigma,xyl'
 print,'Take an image im1 and clean hot pixels by comparing to a '
 print,'smoothed image. Produce a hot pixel list also'
 retall
endif
if n_elements(xyl) eq 0 then hotnew = 0 else hotnew =1 
nx=(size(im1))(1) & ny=(size(im1))(2)
;imo=fltarr(nx,ny)
print,'Started smoothing ...'
imt=smooth(im1,smo)
print,'Finished smoothing '
;look at number of cts per pixel in raw image
maxcts=max(im1) & limit=2*(1.-gaussint(4.))
print,' Image contains a maximum counte rate per pixel of ',maxcts
for i=1l,fix(maxcts) do begin
 wany=where((fix(im1) eq i),nany)
 if nany eq 0 then print,' No pixels with count rate of ',i
 if nany gt 0 then begin
; print,' No. of pixels with count rate of ',i,' = ',nany
 cts=im1(wany) & ycts=wany/nx & xcts=wany-ycts*nx 
 smocts=imt(wany)
 poiss,smocts,i,prob
 whot=where((prob lt limit),nhot)
 print,nhot,' Hot pixels with count rate of ',i
 if nhot eq 0 then print,' No hot pixels with count rate of',i
 if nhot gt 0 then begin
  xyltmp=lonarr(n_elements(whot),2)
  xyltmp(*,1)=ycts(whot) & xyltmp(*,0)=xcts(whot)
 im1(xyltmp(*,0),xyltmp(*,1))=imt(xyltmp(*,0),xyltmp(*,1))
  if hotnew eq 0 then begin
   xyl=xyltmp & hotnew =1 
  endif
  if hotnew gt 0 then xyl=[xyl,xyltmp]
 endif
 endif
endfor
return
end
