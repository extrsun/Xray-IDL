pro moment_2d,xx,yy,xm,ym,mu,ww=ww
mu=fltarr(2,2)
if N_elements(ww) eq 0 then begin
	xm=avg(xx)
	ym=avg(yy)
	mu(0,0)=avg((yy-ym)^2)
	mu(1,1)=avg((xx-xm)^2)
	mu(0,1)=-avg((xx-xm)*(yy-ym))
endif else begin
	xm=avg_w(xx,ww)
	ym=avg_w(yy,ww)
	mu(0,0)=avg_w((yy-ym)^2,ww)
	mu(1,1)=avg_w((xx-xm)^2,ww)
	mu(0,1)=-avg_w((xx-xm)*(yy-ym),ww)
endelse
mu(1,0)=mu(0,1)
return
end