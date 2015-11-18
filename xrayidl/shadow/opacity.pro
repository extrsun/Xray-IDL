pro opacity,file,nh,op,quad=quad
openr,5,file
a=''
for k=0,12 do begin
	readf,5,a
	print,a
endfor
arr=fltarr(2,66)
readf,5,arr
close,5

nhf=fltarr(66)
opf=fltarr(66)
nhf(0:*)=arr(0,*)
opf(0:*)=arr(1,*)
opf=opf/opf(0)
if keyword_set(quad) ne 0 then quadterp,nhf,opf,nh,op else $
	linterp,nhf,opf,nh,op
end