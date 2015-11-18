pro testwhere
a=[1,2,3,4]
b=[4,6,3,5]
c=fltarr(2,4)
c[0,*]=a[*]
c[1,*]=b[*]
print,n_elements(where(c[0,*] lt 3 and c[1,*] gt 3))
print,where(c[0,*] lt 3 and c[1,*] gt 3)
end
