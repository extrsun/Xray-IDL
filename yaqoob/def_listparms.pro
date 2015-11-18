pro def_listparms,oparms,listparms
;
; If PLIST is not defined, then use OPARMS to define default values
; for the list selection (controlled by LISTPARMS)
;
; keyword LISTPARMS must be defined
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' DEF_LISTPARMS, oparms, LISTPARMS'
  retall & endif
;
ctyp = strupcase(strtrim(listparms(18),2))
imtyp = strupcase(strtrim(oparms(4),2))
if (imtyp eq '') then imtyp = 'SKY'       ;default is to select by sky coord.
if (ctyp eq '') then ctyp = imtyp         ;default is same as imtyp 
listparms(18) = ctyp
;
instr = strupcase(strtrim(oparms(3),2))
set_defcen,instr,imtyp,defxcen,defycen,pixsiz
;
tmin = double(listparms(21))
tmax = double(listparms(22))
if (tmin eq 0) then listparms(21) = oparms(21)
if (tmax eq 0) then listparms(22) = oparms(22)
pimin = fix(listparms(23))
pimax = fix(listparms(24))
if (pimin eq 0) then listparms(23) = oparms(23)
if (pimax eq 0) then listparms(24) = oparms(24)
;
pixel = strtrim(listparms(16),2)
if (pixel eq '') then pixel = 'UNBL'      ;default is for unblocked pixels
if (strupcase(pixel) ne 'UNBL') then begin
  block = fix(listparms(17))
  if (block eq 0) then block = 1
endif else block = 1
;
xmin = block*float(listparms(12))            ;change to unblocked pixels
xmax = block*float(listparms(13))
ymin = block*float(listparms(14))
ymax = block*float(listparms(15))
;
xcen = float(oparms(6))
ycen = float(oparms(7))
pixel = strtrim(oparms(8),2)
if (pixel eq '') then pixel = 'UNBL'      ;default is for unblocked pixels
if (strupcase(pixel) ne 'UNBL') then begin
  block = fix(oparms(9))
  if (block eq 0) then block = 1
endif else block = 1
xcen = xcen*block
ycen = ycen*block
binsize = float(oparms(12))
binrat = binsize/pixsiz
nxbin = float(oparms(13))
nybin = float(oparms(14))
;
if (xcen eq 0) then xcen = defxcen
if (ycen eq 0) then ycen = defycen
if (xmin eq 0) then begin
  xmin = xcen - nxbin*binrat/2.
  listparms(12) = string(xmin,format='$(f9.3)')
endif 
if (xmax eq 0) then begin
  xmax = xcen + nxbin*binrat/2.
  listparms(13) = string(xmax,format='$(f9.3)')
endif 
if (ymin eq 0) then begin
  ymin = ycen - nybin*binrat/2.
  listparms(14) = string(ymin,format='$(f9.3)')
endif 
if (ymax eq 0) then begin
  ymax = ycen + nybin*binrat/2.
  listparms(15) = string(ymax,format='$(f9.3)')
endif 
;
return
end          ;pro def_listparms
