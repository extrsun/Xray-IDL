function down2,a
; Reduce a 2-dim matrix by two in both dimensions
b=size(a) & z=2*rebin(float(a),(b(1)/2),b(2))
z=fix(2*rebin(z,(b(1)/2),(b(2)/2)))
if (b(0) eq 1) then z=z/2
return,z
end

