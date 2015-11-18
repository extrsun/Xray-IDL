function down8sis,im
; Reduce a (884x882) SIS matrix by two dimensions in both directions
a=down2(im(0:419,0:421))&b=down2(im(0:419,428:847))
c=down2(im(446:883,428:847))&d=down2(im(446:883,0:421))
a=a(*,1:*)&c=c(0:217,*)&d=d(0:217,1:*)
a=down2(a)&b=down2(b)&c=down2(c)&d=down2(d)
a=a(1:*,1:*)&b=b(1:*,0:103)&c=c(0:107,0:103)&d=d(0:107,1:*)
a=down2(a)&b=down2(b)&c=down2(c)&d=down2(d)
e=reform(intarr(156),3,52)&f=reform(intarr(109),109,1)
return,z=[[a,e,d],[f],[b,e,c]]
end


