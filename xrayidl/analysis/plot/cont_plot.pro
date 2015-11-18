pro cont_plot,xy,info,image,pos,threshold=threshold,thlo=thlo,thhi=thhi $
,clo=clo,chi=chi,c_line=c_line

;+
; NAME:
;    CONT_PLOT
; PURPOSE:
;    Plot contours with the color changing with the overlaid image intensity
;    Called by cont_grey.pro.
;
; INPUTS:
;    xy, info - path_xy and path_info obtained in a call of the procedure
;		contour
;    image     - 2-dimensional array as a grey-scaled image.
;    pos       -four elements vector containing the normalized cordinates
;		of the grey scale plot
; threshold	- intensity threshold for changing the contour color
;			def=2.*avg(image)
; thlo, thhi	- the lower and upper limits (normlized by threshold) to
;		define a intensity buffer zone to avoid too many color-changing
;		segments (def = 0.5, 2).
; clo, chi - the color indexes for contours below or above the threshold
;		def=0, !d.n_colors-1.
;
; REVISION HISTORY:
; wqd, Sept 13, 1997
;
;-

if N_params() lt 2 then begin            
  print,'CALLING SEQUENCE - cont_plot,xy,info,image,pos=pos '
print,',threshold=threshold,thlo=thlo,thhi=thhi,clo=clo,chi=chi,c_line=c_line'
return
endif

if n_elements(clo) eq 0 then clo=0
if n_elements(chi) eq 0 then chi=!d.n_colors-1
if n_elements(threshold) eq 0 then threshold=2.*avg(image)
sz=size(image)
xrange=[-0.49999,sz(1)-0.501]
yrange=[-0.49999,sz(2)-0.501]
mm=nint(xrange(0)+(xrange(1)-xrange(0))/(pos(2)-pos(0))*(xy(0,*)-pos(0)))
nn=nint(yrange(0)+(yrange(1)-yrange(0))/(pos(3)-pos(1))*(xy(1,*)-pos(1)))

imnorm=image/threshold-1.
if n_elements(thlo) eq 0 then thlo=0.5
if n_elements(thhi) eq 0 then thhi=2
n_c=n_elements(info)
for i=0,n_c-1 do begin   
 npoints=info(i).n
 loc=info(i).offset+[indgen(npoints),0]
 m=mm(loc)
 n=nn(loc)
 points=xy(*,loc)
 lines=c_line(info(i).level)
 if imnorm(m(0),n(0)) le 0. then color=clo else color=chi
 for j=1, npoints do begin
 	if color eq clo and imnorm(m(j),n(j)) gt thhi then color=chi
	if color eq chi and imnorm(m(j),n(j)) lt thlo then color=clo
	plots,[points(0,j-1),points(0,j)],[points(1,j-1),points(1,j)],color=color,/norm,lines=lines
 endfor
endfor
return
end