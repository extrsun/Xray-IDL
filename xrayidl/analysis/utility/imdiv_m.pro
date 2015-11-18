function imdiv,num,denom
;
;
;   allows for division of 1 image by another where
;   denominator image may contain zeroes
;
; modified to allow a zero value returned when denom is all zero. Aug 1992(WQD)
;
image=denom*0.0
i=where(denom ne 0.)
if i(0) eq -1 then image=0. else image(i)=num(i)/denom(i) 
return,image
end
