function spin,th,c
; Given coord for pair of orthog axes in ‘c’ program finds new coord
; for second axis following an ‘th’ degree ccw rotation about the first. 
; Setting ‘th’ to 90 produces the third orthogonal axis. 
th=float(th) & c=float(c) & a=(caaab([90.-c(1),c(2)-c(0),90.-c(3)]))(1)-th
b=caaab([90.,a,90.-c(1)]) & z=[(caaab([b(0),b(1),90.]))(1)+c(0),90.-b(0)]
return,z=z mod 360.
end

