function interp2,e,am
; Designed to interpolate two data columns accord to 'e' pos in colmn #1
; 'e' is a const or a vector; 'am' is a three column array 
nen=n_elements(e) & ee=[0,e] & n=intarr(nen) & a=am
e1=rotate(a(0,*),3) & e2=rotate(a(1,*),3) & e3=rotate(a(2,*),3)
na=size(a) & na=na(2)-1
for i=1,nen do n(i-1)= (fix(total(ee(i) gt e1))<na-1)
de=([e1,0]-[0,e1])(1:na)
da=([e2,0]-[0,e2])(1:na) & db=([e3,0]-[0,e3])(1:na)
aa=replicate(0.,nen) & bb=aa & for i=0,nen-1 do begin
aa(i)=e2(n(i)) + ((e(i)-e1(n(i)))/de(n(i)))*da(n(i))
bb(i)=e3(n(i)) + ((e(i)-e1(n(i)))/de(n(i)))*db(n(i))
endfor
return,z=transpose([[aa],[bb]])
end

