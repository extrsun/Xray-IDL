pro galtoait,oxlon,oxlat,sample,xline,xlon0=xlon0,pixsize=pixsize
if n_elements(pixsize) eq 0 then pixsize = 1. ; degree per pixel 
if n_elements(xlon0) eq 0 then xlon0=0.
dr=(!pi/180.)
R2D = 45. / ATAN(1.)
xlat=oxlat*dr
xlon=oxlon*dr
RHO = ACOS( COS(XLAT) * COS((XLON-XLON0)/2.) )
THETA = ASIN( imdiv(COS(XLAT) * SIN((XLON-XLON0)/2.),SIN(RHO)) )
F = (2. * R2D/pixsize) * SIN(RHO/2.)
SAMPLE = -2. * F * SIN(THETA)
;XLINE = -F * COS(THETA) ;original
XLINE = F * COS(THETA)
sel=where(xlat LT 0.,nsel)
IF(nsel ne 0.) then  XLINE(sel) = -XLINE(sel)
return
end