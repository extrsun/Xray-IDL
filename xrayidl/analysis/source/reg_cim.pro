pro reg_cim,list,hdr,regfnamec,asdim=asdim,detagname=detagname,cor=cor,sn=sn,charsize=charsize,thick=thick,xlsh=xlsh,ylsh=ylsh,color=color,shdr=shdr,pcor=pcor,sxmin=sxmin,symin=symin,sdim=sdim
;+
; plot count close-ups of individual sources 
;
; list - evt list
; hdr - header of the list
; sra, sdec, sn - source ra, dec, and label 
; asdim - arcsecond dimension of each plot
; xnp - number of plots along the x-axis
; detagname - the event coordinate modifier (e.g., 'm' -  mergered
;             coord)
; cor - the normalized lower and upper boundaries of the page
; charsize, thick - character size and thickness (also used for the frame)
; xlsh, ylsh - the offset of the label from the left and upper
;              boundaries of each plot in normalized coordinates
; color - color for the counts
; shdr,pcor - header of the plotted image and the normalized
;             coordinates in the plot, useful only for a single plot.
;
; written by wqd, Aug. 19, 2005
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sou_cim,list,hdr,sra,sdec,asdim,xnp'
print,',detagname=detagname,cor=cor,sn=sn,charsize=charsize,thick=thick'
print,',xlsh=xlsh,ylsh=ylsh,color=color,shdr=shdr,pcor=pcor'
return
endif

lcrval=sxpar(hdr,'crval*')
ldel=sxpar(hdr,'cdelt2')*3600.
lcrpix=sxpar(hdr,'crpix*')
sdim=nint(asdim/ldel)
ns=n_elements(sra)

if n_elements(color) eq 0 then begin
        if !d.name eq 'X' then color=!d.n_colors-1 else color=0
endif
if n_elements(cor) eq 0 then cor=[0,1,0,1]
xpsize=(cor(1)-cor(0))/float(xnp)
xcor=cor(0)+xpsize*findgen(xnp+1)
pn=indgen(ns) 
xpn=pn mod xnp
ypn=pn/xnp
ypsize=xpsize*(asdim(1)*float(!d.x_vsize)/asdim(0)/!d.y_vsize)
;accounting for the difference in the screen x and y size difference
ycor=cor(3)-ypn*ypsize

if n_elements(sn) ne 0 then begin
    lsn=1 
    if n_elements(xlsh) eq 0 then xlsh=0.05
    if n_elements(ylsh) eq 0 then ylsh=0.1
endif else lsn=0

;just initiate the plot
contour,[[0,0],[1,1]],/nodata, xstyle=4, ystyle = 4 ;,/noerase
for k=0,ns-1 do begin
    print,'Source ',k
    trans_dist,lcrval(0),lcrval(1),sra(k),sdec(k),xp,yp,pix=ldel,/deg
    sxmin=lcrpix(0)+xp-0.5*sdim(0)+0.5
    symin=lcrpix(1)+yp-0.5*sdim(1)+0.5
    pcor=[[xcor(xpn(k)),ycor(k)-ypsize],[xcor(xpn(k))+xpsize,ycor(k)]]
    xbox=[pcor(0),pcor(2),pcor(2),pcor(0),pcor(0)]
    ybox=[pcor(1),pcor(1),pcor(3),pcor(3),pcor(1)]
    plots,xbox,ybox,/normal,thick=thick
    pcor=[xcor(xpn(k)),xcor(xpn(k))+xpsize,ycor(k)-ypsize,ycor(k)]
    image_counts,list,pcor,sxmin,symin,sdim(0),sdim(1),symsize=1,color=color,detagname=detagname
    if ns eq 1 then begin
        cdim=[sxmin,symin] ;-1 
        ;commenting -1 out seems to match plot_reg and plot shape
        get_fitshead,fltarr(sdim(0),sdim(1)),shdr,hdr,cdim=cdim
    endif
    if lsn ne 0 then xyouts,xcor(xpn(k))+xpsize*xlsh,ycor(k)-ypsize*ylsh $
      ,sn(k),/norm,charsize=charsize,charthick=thick
endfor
end
