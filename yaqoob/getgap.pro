pro getgap,dname,fname,x0,y0,x1,y1,x2,y2,x3,y3
if n_params(0) eq 0 then begin
 print,'getgap,dname,fname,x0,y0,x1,y1,x2,y2,x3,y3'
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
  wc0=where((ccdid eq 0),c0)
  wc1=where((ccdid eq 1),c1)
  wc2=where((ccdid eq 2),c2)
  wc3=where((ccdid eq 3),c3)
  if c0 then begin
    xt0 = rawx(wc0)
    yt0 = rawy(wc0)
    if n_elements(x0) gt 0 then begin
	x0=[x0,xt0] & y0=[y0,yt0]
    endif
  if n_elements(x0) eq 0 then begin
       x0=xt0 & y0=xt0
  endif
  endif
  if c1 then begin
    xt1 = rawx(wc1)
    yt1 = rawy(wc1)
    if n_elements(x1) gt 0 then begin
	x1=[x1,xt1] & y1=[y1,yt1]
    endif
    if n_elements(x1) eq 0 then begin
       x1=xt1 & y1=xt1
    endif
  endif 
  if c2 then begin
    xt2 = rawx(wc2)
    yt2 = rawy(wc2)
    if n_elements(x2) gt 0 then begin
	x2=[x2,xt2] & y2=[y2,yt2]
    endif
    if n_elements(x2) eq 0 then begin
       x2=xt2 & y2=xt2
     endif
   endif
   if c3 then begin
    xt3 = rawx(wc3)
    yt3 = rawy(wc3)
    if n_elements(x3) gt 0 then begin
	x3=[x3,xt3] & y3=[y3,yt3]
    endif
    if n_elements(x3) eq 0 then begin
       x3=xt3 & y3=xt3
    endif
   endif
endif
 i=i+1
endwhile
print,' MIN MAX x0 y0'
if n_elements(x0) then print,'MIN MAX x0 ',minmax(x0)
if n_elements(y0) then print,'MIN MAX y0 ',minmax(y0)
if n_elements(x1) then print,'MIN MAX x1 ',minmax(x1)
if n_elements(y1) then print,'MIN MAX y1 ',minmax(y1)
if n_elements(x2) then print,'MIN MAX x2 ',minmax(x2)
if n_elements(y2) then print,'MIN MAX y2 ',minmax(y2)
if n_elements(x3) then print,'MIN MAX x3 ',minmax(x3)
if n_elements(y3) then print,'MIN MAX y3 ',minm