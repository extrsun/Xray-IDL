function maxloc,im
; Locate the maximum count and location in a 2-dim array 'im'
a=(size(im))(1) & b=(where(im eq max(im)))/float(a)
c=fix(b) & z=[[reform(fix((b-c)*a+.1)),max(im)],[reform(c),0]]
return,transpose(z)
end
