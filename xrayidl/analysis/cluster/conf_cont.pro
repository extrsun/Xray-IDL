pro conf_cont,ca,p0,p1,pxl,pyl,lev=lev,slev=slev,cmin=cmin,pmin=pmin $
  ,pos=pos,symsize=symsize,charsize=charsize,xrange=xrange,yrange=yrange $
  ,xtitle=xtitle,ytitle=ytitle,c_ann=c_ann,c_charsize=c_charsize
;+
; get the contour plot of the confidence levels with an input C or chi^2 grid
;
; ca - grid containing C or chi^2 values
; p0 and p1 - vectors containing parameter values along the two axises
;
; lev - confidence levels above the minimum value (def = 2.3,4.61,9.21)
; slev - single confidence level, if provided, the confidence intervals 
;	for one parameter case will be calculated and plotted. 
; camin - the minimum ca value from a fit (e.g., mle_cl_main.pro)
; pmin - 2-d vector containing parameter values that give camin
; pos - normalized positino of the contour plot (def = [0.2,0.2,0.9,0.9])
; symsize - size of the symbol of the best fit (def = 2)
; charsize - size of x and y labels (def = 2)
; xrange,yrange - x and y ranges for plotting the contour plot
; 		def = min and max of p0 and p1
; ,xtitle, ytitle - x and y axis titles of the plot
;
; written by wqd, 8/5/96
;-
if n_params() eq 0 then begin 
 print,'conf_cont,ca,p0,p1,pxl,pyl,lev=lev,slev=slev,cmin=cmin,pmin=pmin'
 print,',pos=pos,symsize=symsize,charsize=charsize,xrange=xrange,yrange=yrange'
 print,',xtitle=xtitle,ytitle=ytitle,c_ann=c_ann,c_charsize=c_charsize'
 return
endif
;------------------------------------------------
if n_elements(xrange) eq 0 then xrange=minmax(p0)
if n_elements(yrange) eq 0 then yrange=minmax(p1)
if n_elements(xtitle) eq 0 then xtitle='!7b'
if n_elements(ytitle) eq 0 then ytitle='!7h!6!Dc!n (arcsecond)'
if n_elements(pos) eq 0 then pos=[0.2,0.2,0.9,0.9]
if n_elements(charsize) eq 0 then charsize=2
!x.style=1 ; to garrantee the pxl and pyl are consistent with the plot limts
!y.style=1


; one-parameter confidence limits:
if n_elements(slev) ne 0 then $
	cont_limits,ca,p0,p1,pxl,pyl,lev=slev,pos=pos,cmin=cmin $
		,xrange=xrange,yrange=yrange

if n_elements(lev) eq 0 then lev=cmin+[2.3,4.61,9.21]
if n_elements(c_ann) eq 0 then c_ann=['!6 68%','!6 90%','!6 99%']
;------------------------------------------------
if n_elements(c_charsize) eq 0 then c_charsize=1
contour,ca,p0,p1,lev=lev,pos=pos,charsize=charsize $
	,c_ann=c_ann,c_charsize=c_charsize $
	,xtitle=xtitle,ytitle=ytitle,xrange=xrange,yrange=yrange

if n_elements(pmin) ne 0 then begin
	if n_elements(symsize) eq 0 then symsize=2
	plots,pmin(0),pmin(1),psym=7,symsize=symsize
endif

return
end
;=========================
pro cont_limits,ca,p0,p1,pxl,pyl,lev=lev,pos=pos,cmin=cmin $
	,xrange=xrange,yrange=yrange
;+
; derive confidence limits from the one-parameter case contour in a grid
; written by wqd, 8/5/96
; ca - array containing values of C or chi^2 statistics
; p0 and p1 - values of the two axis
; 
;-
if n_elements(pos) eq 0 then pos=[0.1,0.1,0.9,0.9]
if n_elements(lev) eq 0 then lev=2.71
;--------------------------------------
; get the location of minimum ca
camin=min(ca,locmin)
sz=size(ca)
pmin=[p0(locmin mod sz(1)),p1(locmin/sz(2))]
print,'camin, x, y = ',camin,pmin

if n_elements(cmin) ne 0 then begin
	if camin gt cmin then camin=cmin else $
	print,'use camin (< cmin) = ',camin
endif else cmin=camin
;--------------------------------------
;contour,ca,lev=camin+lev,p0,p1,pos=pos,c_line=lev*0.+1 $
;	,xrange=xrange,yrange=yrange
contour,ca,lev=camin+lev,p0,p1,pos=pos,path_xy=xy,path_info=info,/overplot $
	,xrange=xrange,yrange=yrange
xl=minmax(xy(0,4:*))
yl=minmax(xy(1,4:*))

pxl=xrange(0)+(xrange(1)-xrange(0))/(pos(2)-pos(0))*(xl-pos(0))
pyl=yrange(0)+(yrange(1)-yrange(0))/(pos(3)-pos(1))*(yl-pos(1))
print,'x and y contour limits = '
print,pxl,pyl
;stop
return
end
