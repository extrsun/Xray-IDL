pro probks,alam,prob
maxi=fix(sqrt(13.)/alam < 1000)
i=indgen(maxi)+1
a2=-2.*alam^2
odd=exp((2.*i-1)^2*a2)
even=exp((2.*i)^2*a2)
prob=odd-even
return
end