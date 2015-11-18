pro stackplot,x,y1,y2,y3,y4,y5,y6,y7,y8,ynames=ynames

; procedure to plot a vertical stack of plots (with no margins in between,
; and all individual plots of the same size)

; written by Gail Reichert (Universities Space Research Association) and 
; Bobby Candey (Atlantic Research Corp.)
; 1991 June 26

npar = n_params(0)
if (npar eq 0) then begin
  print,'STACKPLOT, x, y1 [, y2, y3, y4, y5, y6, y7, y8]'
  print,'          [, ynames=ynames]'
  print, 'Uses !p.title, !x.title, and !x.tickname only once for the page'
  return
end
if (npar eq 1) then begin
  print,'You must specify at least two variables to plot. Returning.'
  return
end
if (npar gt 9) then begin
  print,'You may not stack more than 8 plots at a time. Returning.'
  return
end
;
if (n_elements(ynames) eq 0) then $
  ynames = ['Y1','Y2','Y3','Y4','Y5','Y6','Y7','Y8']
if (!x.title eq '') then xname = 'X' else xname = !x.title
if (!p.title eq '') then title= ' ' else title = !p.title
nplots = npar - 1         ;number of plots to be stacked
nx = n_elements(x)
yplot = fltarr(nx,nplots)
yplot(0,0) = y1
if (nplots ge 2) then yplot(0,1) = y2 
if (nplots ge 3) then yplot(0,2) = y3 
if (nplots ge 4) then yplot(0,3) = y4 
if (nplots ge 5) then yplot(0,4) = y5 
if (nplots ge 6) then yplot(0,5) = y6 
if (nplots ge 7) then yplot(0,6) = y7 
if (nplots eq 8) then yplot(0,7) = y8 
;
cxmin = !x.window(0) & cxmax = !x.window(1)
cymin = !y.window(0) & cymax = !y.window(1)
xregion = !x.region & yregion = !y.region
pclip = !p.clip
xtitle = !x.title & ytitle = !y.title & mtitle = !p.title
xtickname = !x.tickname
ymargin = !y.margin & yomargin = !y.omargin
pmulti = !p.multi
charsize = !p.charsize
!x.tickname = replicate(' ',6) & !x.title = ''
!p.title = title
!p.multi = [0,1,nplots]
!y.margin = [0,0]
!y.omargin = [10,10]
!p.charsize = (nplots - 1.)/6. + 1.
;
for jj=0,nplots-1 do begin
  !y.title = ynames(jj)
  yy = yplot(*,jj)
  if (jj eq (nplots-1)) then begin ; last plot
    !x.tickname = xtickname
    !x.title = xname
  endif
  plot,x,yy,/ynozero
  !p.title = ''
endfor
;
!x.title = xtitle & !y.title = ytitle & !p.title = mtitle
!x.tickname = xtickname
!y.margin = ymargin & !y.omargin = yomargin
!p.multi = pmulti
!p.charsize = charsize
!x.window = [cxmin,cxmax]
!y.window = [cymin,cymax]
!x.region = xregion & !y.region = yregion
!p.clip = pclip
;
return
end             ;pro stackplot
