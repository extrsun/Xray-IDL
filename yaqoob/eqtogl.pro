function eqtogl,e
; Converts equatorial coordinates into galactic (deg).
e=float(e) & se=size(e) & if se(0) eq  1 then se=1 else se=se(2)
a=e(0,*)-192.25 & b=90.-e(1,*) & c=caaab([replicate(62.6,1,se),a,b])
z=123.-c(1,*) & z=z+360.*(z lt 0) 
return,z=[z,90.-c(0,*)]
end

