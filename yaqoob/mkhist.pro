pro mkhist,vals,nbins,loc,xval,yval,xp,yp
if n_params(0) eq 0 then begin
 print,'mkhist, vals,nbins,loc,xval,yval,xp,yp '
 print,' make a histogram of VALS and output xval (bin centre) and yval '
 retall
end
nval=(size(vals))(1) & yval=fltarr(nbins)
minv=min(vals) & maxv=max(vals)
delx=(maxv-minv)/float(nbins)
xlow=findgen(nbins)*delx +  minv
xhigh=xlow+delx & xval=(xhigh+xlow)/2.
loc=lonarr(nval)
print,minv,maxv
loc=long((vals-minv)/delx) < (nbins-1)
for k=0l,nbins-1 do begin
 ind = where(loc eq k)
 if  total(ind) gt 0.0 then $ 
yval(k) = (size(where(loc eq k)))(1)
endfor
;now compute values for the purpose of plotting a bar diagram
;for nbins we need nbins*2 - 2 values
nwbin=2*nbins-2 & xp=fltarr(nwbin) & yp=xp
xp(0)=xlow(0) & xp(nwbin-1) = xhigh(nbins-1)
yp(0)=yval(0) & yp(nwbin-1) = yval(nbins-1)
norm=max(yval) 
for i=1,nbins-2 do begin
 xp(2*i-1)=xval(i) & xp(2*i)=xval(i)
 yp(2*i-1)=yval(i-1) & yp(2*i)=yval(i)
endfor
yp=100.*yp/norm
return
end
