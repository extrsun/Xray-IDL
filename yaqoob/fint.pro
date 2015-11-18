pro fint,xin,yin,x0,y0
if n_params(0) eq 0 then begin
 print,'fint,xin,yin,x0,y0'
 print,'Logarithmic Interpolation '
 print,'Inputs: xin,yin,x0'
 print,'Output: y0'
 retall
endif
y0=fltarr((size(x0))(1))
xmax=max(xin) & xmin=min(xin)
wxmax=where(xin eq xmax) & wxmin=where(xin eq xmin)
n1=(size(xin))(1) & n2=(size(x0))(1)
if n1 lt 2 then goto, fin 
if n1 ne (size(yin))(1) then goto, fin 
for j=0l,n2-1 do begin
;this x0 is equal to one of the grid points
 weq=where((xin eq x0(j)),cweq)
 if cweq gt 0 then begin
   y0(j)=yin(weq) 
   x1=xin(weq) & x2=x1 & y1=yin(weq) & y2=y1
   goto, nxtx0
 endif
;check if this x0 outside lower bound of grid
 if x0(j) lt xmin then begin
   y0(j) = yin(wxmin)
   goto,nxtx0
 endif
;check if this x0 is outside upper bound of grid
 if x0(j) gt xmax then begin
   y0(j) = yin(wxmax) & goto,nxtx0
 endif
;find the two points of xin which bound x0
 x1=max(xin(where(xin lt x0(j))))  
 x2=min(xin(where(xin gt x0(j))))
 y1=yin(where(xin eq x1)) & y2=yin(where(xin eq x2))
 y1=y1(0) & y2=y2(0)
 if x1 gt 0. then x1l=alog(x1) else x1l=-80.
 if x2 gt 0. then x2l=alog(x2) else x2l=-80.
 if y1 gt 0. then y1l=alog(y1) else y1l=-80.
 if y2 gt 0. then y2l=alog(y2) else y2l=-80.
 if x0(j) gt 0. then x0l=alog(x0(j)) else x0l=-80.
 ly0 = ((x0l-x1l)*(y2l-y1l)/(x2l-x1l))+y1l
 y0(j)=exp(ly0)
nxtx0: dummy=0
;print,'X: ',x1,x0(j),x2 & print,'Y: ',y1,y0(j),y2
endfor
fin: return
end
