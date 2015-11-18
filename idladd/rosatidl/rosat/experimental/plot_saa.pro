;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
; Procedure to plot the saa contours for selected models in the pdb
;
;-
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro plot_saa,lsaa,bsaa,saamodel,x_color
;
ltyp = !linetype
;!linetype=1
if (n_elements(saamodel) eq 1) then mn=intarr(1)+saamodel else mn=saamodel
for i=0,n_elements(mn)-1 do begin
  x=lsaa(*,mn(i)) & y=bsaa(*,mn(i))
  inx=where(x+y ne 0)
  x=[x(inx),x(0),x(1)] & y=[y(inx),y(0),y(1)]
  x=x-360*(x gt 180)
  oplot,x(1:*),y(1:*),color=x_color(i)
endfor
;!linetype = ltyp
;
return
end       ;pro plot_saa
