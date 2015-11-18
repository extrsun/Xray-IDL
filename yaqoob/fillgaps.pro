function fillgaps,a
; Fill in both verical and horizontal gaps in reduced SIS matrix
; resulting from DOWN8SIS
b=(a(51,*)+a(55,*))/2. & b=rebin(b,3,105)
a(52:54,*)=fix(b+sqrt(b)*randomn(seed,3,105))
b=(a(*,51)+a(*,53))/2. & a(*,52)=fix(b+sqrt(b)*randomn(seed,109,1))
return,a
end

