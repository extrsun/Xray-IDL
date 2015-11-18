function tminterv,a
; Extract event intervals from 'plist.time'
a=a(sort(a)) & b=([a,0]-[0,a])(1:*) & b=b(0:n_elements(b)-2)
b(where(b gt 1000.))=0.
b=b(where(b)) & c=fix(b*10.*n_elements(b)/total(b))
return,c
end
