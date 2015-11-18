pro plotchi,dof,npt,lc,uc,x,dist
if n_params(0) eq 0 then begin
 print,' PLOTCHI, DOF ,npt,lc,lu, x, dist [DOF=degrees of freedom] '
 print,' plot theoretical chi-square distribution for DOF degrees freedom '
 print,' npt = number of points, lc & uc = lower and upper bounds on stat'
 print,' dist and dist1 are output distributions for dof and dof-1 vs x'
 retall
end
delc = (uc-lc)/float(npt-1)
dist=fltarr(npt,dof) & x=lc+findgen(npt)*delc
for j=0l,dof-1 do begin
df=float(j+1)
for  k =0l,npt-1 do dist(k,j)=chi_sqr1(x(k),df)
if j eq 0l then plot,x,xtitle='chi-square',ytitle='cumulative probability',$ 
xrange=[lc,uc],yrange=[-0.05,1.05],dist(0:npt-1,j)
if j gt 0l then oplot,x,dist(0:npt-1,j)
endfor
return
end
