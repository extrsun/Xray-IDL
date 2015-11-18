pro gal_dif_it1,glad,im,imout,filter=filter,imin=imin,imax=imax, $
  title=title,ncol=ncol,field=field
;**
;** Calculate a model distribution of a Galactic diffuse emission based on 
;** an input emission image and 2-D disk model for emission.  Do fits iter-
;** atively by subtracting fit the one bin interval with the largest absolute
;** residual (from the fit), and then repeating the fit.
;**
;** INPUTS:
;** glad   == Galactic latitudes of individual image pixels 
;**             from glactc_im.pro;
;** im     == image;
;** imin,imax == the minimum and maxmum values of the image bins to be 
;**                included in the fit (def=[0,10^20]);
;** filter == pixels with filter value < 0. will not be used in the model fit.
;** ncol   == number of latitude intervals to break image down into.
;**OUTPUTS:
;** imout == the model image.
;**
;** Written by kachun 17 June, 1994; based on gal_dif_im by wqd, 4/17/94,
;**   and gal_dif_it2 by kachun (15 June, 1994).
;**
if n_params() eq 0 then begin
  print,'CALLING SEQUENCE - gal_dif_im,glad,im,imout,filter=filter'
  print,',imin=imin,imax=imax,ncol=ncol'
  return
endif
;** Create arrays for saving all values of the coefficients, and for
;**   plots during fits:
coefs = fltarr(2,round(ncol/2.))
plota = fltarr(round(ncol/2.),ncol,2)

!p.thick=2.75
!p.charsize=1.75
!p.charthick=1.75

if n_elements(ncol) eq 0 then ncol=30
if n_elements(imin) eq 0 then imin=0.
if n_elements(imax) eq 0 then imax=1.e20 ;essentially infinite 
if n_elements(filter) eq 0 then filter=im*0.+1.
if n_elements(title) eq 0 and n_elements(field) eq 0 then $
  title = '' else $
if n_elements(title) eq 0 and n_elements(field) ne 0 then $
  title = '!6ISSA ' + strtrim(title,2) + ' (Field ' + $
  strtrim(string(field),2) + ')'

ncoef0 = [-10.,10.] ;** Arbitrary values of coefficients to start comparisons.
print,'Starting values of coefficients to compare to: ',ncoef0

sel=where(im ge imin and im le imax and filter gt 0.,nsel)
if nsel eq 0 then stop,'there is no bin selected'
binvals=fltarr(nsel)
binvals(0:*)=im(sel)
glads=glad(sel)
;** Get the median values for individual latitude intervals:
print,'Getting latitude intervals . . .'
get_ref_median,glads,binvals,colval,colvallo,colvalhi,glam,glalo,glahi, $
  ncol=ncol,nbin=nbin
;** Do first least-squares fit it the median values with a 2-D disk model
galactc_fit_v,colval,glam,coef,ww=1./((colvalhi-colvallo)/2.)^2
coefs(*,0) = coef
modv=coef(0)+coef(1)/sin(abs(glam*!pi/180.))
plota(0,0:n_elements(glam)-1,0) = glam
plota(0,0:n_elements(glam)-1,1) = modv
newglam = glam
newcolv = colval
newncol = ncol
!p.thick=1.
;** Do iteration of fits and remove a latitude interval in each loop:
for i = 0,round(ncol/2.)-1 do begin
  print,'% change in coefficients: ', 100.*(coef(0)-ncoef0(0))/coef(0), $
    100.*(coef(1)-ncoef0(1))/coef(1)
  modv=coef(0)+coef(1)/sin(abs(newglam*!pi/180.))
  plota(i,0:n_elements(newglam)-1,0) = newglam
  plota(i,0:n_elements(newglam)-1,1) = modv
  ;** Get greatest difference between fit and latitude intervals:
  sel = where(abs(modv-newcolv) eq max(abs(modv-newcolv)))
  sel = sel(0)  ;** Variable sel originally is an Array(1); also in case
                ;** more than one point is at the maximum.
  newncol = newncol - 1
  print,'Point '+strtrim(sel+1,2)+' with residual of '+ $
    strtrim(string(modv(sel)-newcolv(sel)),2)+' removed from future fits.'
  ;** Make new latitude and intensity vectors for next fit:
  if sel eq 0 then begin
    newglam = newglam(1:newncol)
    newcolv = newcolv(1:newncol)
  endif else if sel eq newncol then begin
    newglam = newglam(0:newncol-1)
    newcolv = newcolv(0:newncol-1)
  endif else begin
    temp = fltarr(newncol)
    temp(0:sel-1) = newglam(0:sel-1)
    temp(sel:*) = newglam(sel+1:*)
    newglam = temp
    temp(0:sel-1) = newcolv(0:sel-1)
    temp(sel:*) = newcolv(sel+1:*)
    newcolv = temp
  endelse
  ncoef0 = coef
  galactc_fit_v,newcolv,newglam,coef,ww=1./((colvalhi-colvallo)/2.)^2
  coefs(*,i) = coef
endfor
!p.thick=2.75

;** Figure out which bins were not used:
uglam = fltarr(n_elements(glam)-n_elements(newglam))
ucolv = fltarr(n_elements(glam)-n_elements(newglam))
j = 0
for i = 0,ncol-1 do begin
  if total(where(glam(i) eq newglam)) eq -1 then begin
    uglam(j) = glam(i)
    ucolv(j) = colval(i)
    j = j+1
  endif
endfor

;** Do final plots:
ok = 0
yesno = ''
latrange = ''
a = findgen(16)*!pi*2/16.
usersym,cos(a),sin(a)
repeat begin
  print,'Enter latitude range to plot (default = none);'
  read,'(Min./Max. latitude values are '+strtrim(glalo(0),2)+'/'+$
    strtrim(glahi(ncol-1),2)+'): ',latrange
  if latrange eq '' then $
    plot,glam,colval,psym=3,xrange=[glalo(0),glahi(ncol-1)], $
    yrange=[min(colvallo(*)),max(colvalhi(*))], $
    xtitle='!6Galactic latitude (degrees)', $
    ytitle='!6IRAS 100 !7l!6m Intensity (MJy sr!U-6!N)',title=title
  if latrange ne '' then begin
    reads,latrange,lat1,lat2
    plot,glam,colval,psym=3,xrange=[lat1,lat2], $
      yrange=[min(colvallo(*)),max(colvalhi(*))],xstyle=1, $
      xtitle='!6Galactic latitude (degrees)', $
      ytitle='!6IRAS 100 !7l!6m Intensity (MJy sr!U-1!N)',title=title
  endif
  errplot_y,colval,glalo,glahi
  errplot,glam,colvallo,colvalhi
  !p.thick = .5
  for i = 0,round(ncol/2.)-2 do $
    oplot,plota(i,0:ncol-i-1,0),plota(i,0:ncol-i-1,1),linestyle=1
;  for i=0,round(ncol/2.)-2 do oplot,glam, $
;    coefs(0,i)+coefs(1,i)/sin(abs(glam*!pi/180.)),linestyle=1
  !p.thick = 2.75
  modv=coef(0)+coef(1)/sin(abs(glam*!pi/180.))
  oplot,glam,modv
  oplot,uglam,ucolv,psym=8
  if yesno eq 'p' or yesno eq 'p' then lx
  yesno = ''
  read,'Quit (default), redo plot (r), or print out plot (p)? ',yesno
  if yesno eq 'p' then landplot else if yesno eq 'r' then ok = 0 else ok = 1
endrep until ok

;** Get the model image of the diffuse distribution:
imout = im*0.
imout = coef(0)+coef(1)/sin(abs(glad*!pi/180.))

!p.thick=1.
!p.charsize=1.
!p.charthick=1.

end


