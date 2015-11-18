function interv,m,n
; use to extract event intervals from a "QDP" file
; 'm' is the truncated "QDP" file, 'n' is the number of observing
; segments in the data. 
a=(datxfer(m,4,0))(0,0:*) & a=reform(a(0:*)) & b=([a,0]-[0,a])(1:*)
b=b(0:n_elements(b)-2) & c=rotate(sort(b),2) & b(c(0:n-1))=0
return,b(where(b))
end
