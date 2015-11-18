pro hpmask,x,y,hpxy,mout
; Generate a vector mask which, when multiplying the'X', 'Y' and 'PHA"
; event data, remove those associated with 'hot pixels'.
; INPUTS
; 'X', 'Y', 'HPXY' => hot pixel xy list.
; OUTPUT
; 'MOUT'.  Use as follows: 'X(good)'=X*MOUT
if n_params(0) eq 0 then begin
print,'HPMASK,x,y,hpxy,mout'
print,'Generate a ''hot pixel'' mask to operate on obs parameters'
retall
end
a=(size(x))(1) & mout=intarr(a)
for i=0l,a-1 do begin
mout(i)=(total((x(i) eq hpxy(0,*)) and (y(i) eq hpxy(1,*)))) lt 1
endfor
return
end
