pro plot_circle,rra,rdec,sr,cor,hdr,s_c=s_c,nd=nd,thick=thick,linestyle=linestype
;+
; rra, rdec - RA and Dec of the circle center (deg)
; sr - circle radius in units of arcmin
; cor - normalized plot corner from cont_grey
; hdr - the header of the plot
; s_c - color of the box
; nd - number of points to connect a circle (def=100); for a large
;      radius, one may want to have a larger nd.
;
; written by wqd, 5/30/2001
;-
if N_params() eq 0 then begin     
  print,'CALLING SEQUENCE -plot_circle,rra,rdec,sr,cor,hdr'
 print,',s_c=s_c,nd=nd,thick=thick,linestyle=linestype'
 return
endif
if n_elements(s_c) eq 0 then s_c=!d.n_colors-1
;	if !d.name eq 'X' then s_c=!d.n_colors-1 else s_c=0
;endif
if n_elements(nd) eq 0 then nd=100
an=findgen(nd+1)*(2.*!pi/nd)
if n_elements(sr) eq 1 then sr=sr+rra*0.
xp=fltarr(nd)
yp=fltarr(nd)
for ns=0,n_elements(rra)-1 do begin
	for k=0,nd-1 do begin
		xp(k)=sr(ns)*cos(an(k))
		yp(k)=sr(ns)*sin(an(k))
	endfor
	trans_loct,xp,yp,rra(ns),rdec(ns),sra,sdec,pixsize=60.,/deg
	source_plot,'',cor,hdr,sra=sra,sdec=sdec,psym=0,symsi=2,thick=thick,s_c=s_c,/silent,linestyle=linestype
endfor
return
end
