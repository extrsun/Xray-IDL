function gltoeq,g
; Converts galactic coordinates into R.A. and Decl. (deg)
g=float(g) & sg=size(g) & if sg(0) eq 1 then sg=1 else sg=sg(2)
a=90.-g(1,*) & b=123.-g(0,*) & c=caaab([a,b,replicate(62.6,1,sg)])
z=90.-c(0,*) & d=caaab([c,90.-g(1,*)]) & e=(192.25+d(1,*)) mod 360. 
return,z=[e,z]
end

