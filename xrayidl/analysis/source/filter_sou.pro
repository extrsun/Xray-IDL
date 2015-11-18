;--------------------------------------------------------------------------
pro filter_sou,filter,cra,cdec,filter_ra,filter_dec,source_ra,source_dec,radius $
          ,block=block,subvalue=subvalue
;-----------------------------------------------------------------------------
if n_params() EQ 0 then begin
	print,'CALLING SEQUENCE - '
endif
;
sz=size(filter)
if n_elements(block) EQ 0 then block=1
xdim=sz(1)
ydim=sz(2)
if n_elements(subvalue) eq 0 then subvalue=0.
;
; gets the locations of the sources in the filter in pixels
;
trans_dist,filter_ra,filter_dec,source_ra,source_dec,xp,yp,/deg
;
xcpix=(xdim-1.)/2.
ycpix=(ydim-1.)/2.
xp=xp/block+xcpix
yp=yp/block+ycpix
;
; finds the size of the regions which are contaminated by the sources
;
dis=sqrt((xp-xcpix)^2+(yp-xcpix)^2)*block/!ampix
boxhalf=radius/(block*!size_pixel)
;
imin=fix(xp-boxhalf+1.0) > edge_side
imax=fix(xp+boxhalf) < (xdim-1-edge_side) 
jmin=fix(yp-boxhalf+1.0) > edge_side
jmax=fix(yp+boxhalf) < (ydim-1-edge_side)
;
; if no source is in the filter, return
;
c=where (imin LE imax AND jmin LE jmax, count)
if count eq 0 then begin
print,'No sources are covered in the filter.'  & goto,out 
endif
;
print,'Number of sources effecting the filter is ',count
;
for k=0,(count-1) do begin
n=c(k)
 for j=1,xdim do begin
	ys=(j+jmin-yp(n))^2
 for i=imin(n),imax(n) do begin
	dis=(i+imin-xp(n))^2+ys
	if dis le sboxhalf(n) and filter_sub(i,j) ne 0. then $
	filter_sub(i,j)=subvalue ;for identifying				     ;source regions
 endfor & endfor
endfor
;
print,'number of bins set to zero = ',nbin_o - n_elements(where(filter_sub ne 0))
out:
if !debug eq 2 then stop
;
stop
return
end