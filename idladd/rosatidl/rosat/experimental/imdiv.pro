function imdiv,num,denom
;
;
;   allows for division of 1 image by another where
;   denominator image may contain zeroes
;
;
i=where(denom ne 0)
test=num(i)/denom(i)
image=denom*0
image(i)=test
return,image
end
