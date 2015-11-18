function angle,a,b       
; possibilities: 1) size(a)=size(b); 2) size(a)>size(b)=2
c=float(a) & d=float(b) & e=size(c)
if (size(c))(0) gt 1 then d=rebin(d,e(1),e(2))
return,caaab([90.-c(1,*),d(0,*)-c(0,*),90.-d(1,*)])
end  

