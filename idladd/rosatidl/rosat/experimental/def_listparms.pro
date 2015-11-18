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
if (n_elements(listparms) eq 0) then rsgetpar,0,'make_list',listparms
;
ctyp = strupcase(strtrim(listparms(18),2))
imtyp = strupcase(strtrim(oparms(4),2))
if (imtyp eq '') then imtyp = 'SKY'       ;default is to select by sky coord.
if (ctyp eq '') then ctyp = imtyp         ;default is same as imtyp 
listparms(18) = ctyp
;
; Set the default X, Y center for the image and the default limits for the
;     list selection
;
instr = strupcase(strtrim(oparms(3),2))
set_defcen,instr,imtyp,defxcen,defycen,pixsiz,defxlim,defylim
set_defcen,instr,ctyp,dxcen,dycen,dpixs,dxlim,dylim
;
; Set time limits for list selection. Use limits for image if not defined.
; If list time limits are defined, and do not include entire range for image,
; then reset the list selection limits
;
tmin = double(listparms(21))
tmax = double(listparms(22))
tbot = double(oparms(21))
ttop = double(oparms(22))
if (tmin eq 0) then listparms(21) = oparms(21)
if (tmax eq 0) then listparms(22) = oparms(22)
if ( (tmin ne 0) and (tmin gt tbot) ) then listparms(21) = oparms(21)
if ( (tmax ne 0) and (tmax lt ttop) ) then listparms(22) = oparms(22)
;
; Set the pi channel limits for list selection. Same as for time limits
; above
;
pimin = fix(listparms(23))
pimax = fix(listparms(24))
pibot = fix(oparms(23))
pitop = fix(oparms(24))
if (pimin eq 0) then listparms(23) = oparms(23)
if (pimax eq 0) then listparms(24) = oparms(24)
if ( (pimin ne 0) and (pimin gt pibot) ) then listparms(23) = oparms(23)
if ( (pimax ne 0) and (pimax lt pitop) ) then listparms(24) = oparms(24)
;
listpix = strtrim(listparms(16),2)
if (listpix eq '') then listpix = 'UNBL'     ;default is for unblocked pixels
if (strupcase(listpix) ne 'UNBL') then begin
  listblock = fix(listparms(17))
  if (listblock eq 0) then listblock = 1
endif else listblock = 1
;
xmin = listblock*float(listparms(12))            ;change to unblocked pixels
xmax = listblock*float(listparms(13))
ymin = listblock*float(listparms(14))
ymax = listblock*float(listparms(15))
;
xcen = float(oparms(6))
ycen = float(oparms(7))
impix = strtrim(oparms(8),2)
if (impix eq '') then impix = 'UNBL'      ;default is for unblocked pixels
if (strupcase(impix) ne 'UNBL') then begin
  imblock = fix(oparms(9))
  if (imblock eq 0) then imblock = 1
endif else imblock = 1
xcen = xcen*imblock
ycen = ycen*imblock
binsize = float(oparms(12))
binrat = binsize/pixsiz
nxbin = float(oparms(13))
nybin = float(oparms(14))
;
; Use image X, Y center to set X, Y limits for list selection, if these
;   are not defined
; If CTYP and IMTYP are not equal, however, then we won't want to do this
;
if (ctyp eq imtyp) then begin
  if (xcen ne 0) then begin
    if (xmin eq 0) then xmin = xcen - nxbin*binrat/2.
    if (xmax eq 0) then xmax = xcen + nxbin*binrat/2.
  endif else begin
    if (xmin eq 0) then xmin = dxlim(0)
    if (xmax eq 0) then xmax = dxlim(1)
  endelse
  if (ycen ne 0) then begin
    if (ymin eq 0) then ymin = ycen - nybin*binrat/2.
    if (ymax eq 0) then ymax = ycen + nybin*binrat/2.
  endif else begin
    if (ymin eq 0) then ymin = dylim(0)
    if (ymax eq 0) then ymax = dylim(1)
  endelse
endif else begin
  if (xmin eq 0) then xmin = dxlim(0)
  if (xmax eq 0) then xmax = dxlim(1)
  if (ymin eq 0) then ymin = dylim(0)
  if (ymax eq 0) then ymax = dylim(1)
endelse
if (xcen eq 0) then xcen = defxcen
if (ycen eq 0) then ycen = defycen
;
oparms(6) = string(xcen/imblock,format='$(f9.3)')
oparms(7) = string(ycen/imblock,format='$(f9.3)')
listparms(12) = string(xmin/listblock,format='$(f9.3)')
listparms(13) = string(xmax/listblock,format='$(f9.3)')
listparms(14) = string(ymin/listblock,format='$(f9.3)')
listparms(15) = string(ymax/listblock,format='$(f9.3)')
; 
if (!debug ge 3) then begin
  print,' Image/list type: ',' '+oparms(4)+' '+listparms(18)
  print,' Tmin,tmax: ',' '+oparms(21)+' '+oparms(22)+' '+listparms(21)+$
       ' '+listparms(22)
  print,' Pimin,Pimax: ',' '+oparms(23)+' '+oparms(24)+' '+listparms(23)+$
       ' '+listparms(24)
  print,' Xmin,Xmax,Ymin,Ymax,Block: ',' '+listparms([12,13,14,15,17])
  print,' Xcen,Ycen,Block,Binsize,Nxbin,Nybin',' '+oparms([6,7,9,12,13,14])
endif
;
return
end          ;pro def_listparms
