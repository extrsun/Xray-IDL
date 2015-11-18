function absorb,d,e       
;  Compute photon absorption coeff at energies [e] based on data [d]
dn=size(d) & en=size(e) & if en(0) eq 0 then begin
z=e gt d(0,*) & i=fix(total(z))-1 & zz=d(1,i)*e^(-d(2,i)) 
endif else begin & zz=e
z=(rebin(e,en(1),dn(2)) gt rebin(d(0,*),en(1),dn(2)))
for i=0, en(1)-1 do begin 
j=fix(total(z(i,*)))-1  & zz(i)=d(1,j)*e(i)^(-d(2,j))
endfor
endelse
return,zz
end

