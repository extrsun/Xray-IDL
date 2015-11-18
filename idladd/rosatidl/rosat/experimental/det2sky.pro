pro det2sky,time,detx,dety,asptim,roll,aspdx,aspdy,imx,imy,instr,aspcorr=aspcorr
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' DET2SKY, time, detx, dety, asptim, roll, aspdx, aspdy, IMX, IMY,'
  print,'          [instr ("P" or "H", def = "P"), aspcorr=aspcorr (1)]'
  return
endif
;
if (n_elements(aspcorr) eq 0) then aspcorr = 1    ;default is to correct
if (npar lt 10) then instr = ''
if (instr eq '') then instr = 'P'
if (instr eq 'P') then begin
  xrange = 8192
  pascp = 1.8684154
  yflip = 1
  xoff = 7680
endif else begin
  xrange = 4096
  pascp = 1.0
  yflip = 0
  xoff = 4096
endelse
yrange = xrange
;yoff = (1 + 2*yflip)*xoff
yoff = xoff
;
if (instr eq 'P') then begin
  dxcorr = 4119.
  dycorr = 3929.
endif
;
deg2rad = 3.14159/180.
rasp = roll
delx = aspdx
dely = aspdy
if (!debug gt 1) then stop,'stopping in det2sky before interpolating aspects.'
;
print,' Now interpolating aspect information to match photon arrival times.'
interp_aspect,time,asptim,rasp,delx,dely
cosr = cos(deg2rad*rasp)
sinr = sin(deg2rad*rasp)
rasp = 0
print,' Finished interpolating.'
;
; subtract 1 from detector coord, since in SASS calculation, pixel numbers 
; start with 0 ?? (but, NO, it doesn't seem so from the numbers in the file!!)
; also subtract boresight (?) corrections dxcorr and dycorr
;
xrel = detx - 1 - dxcorr
yrel = dety - 1 - dycorr
;xrel = detx - dxcorr
;yrel = dety - dycorr
;
; multiply by pascp to change from detector to image (sky) pixels
; add delx, dely to imx and imy (to refer to common aspect)
; add xoff, yoff to imx and imy (to move origin to center)
;
imx = pascp*(cosr*xrel - sinr*yrel) + aspcorr*delx + xoff
imy = yoff + (1-2*yflip)*( pascp*(sinr*xrel + cosr*yrel) + aspcorr*dely )
;
return
end           ;pro det2sky
