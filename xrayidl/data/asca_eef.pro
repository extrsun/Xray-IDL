pro asca_eef,dia,en,offang,phase,efffile=efffile
;-
; efffile - file containing encircled energy as a function of off-source
;		diameter and photon energy. The file is specifically for
;		a combination of the off-axis angle of a source and phase
;		(the angle from the boundary of quadrants).
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - asca_eef,dia,en,offang,phase,efffile=efffile'
return
endif
diaen=fltarr(61,100)
openr,un,efffile,/get
readf,un,diaen
close,un
env=findgen(100)*0.1+0.1
diav=findgen(61)+1
tabinv_m,env,en,ien
tabinv_m,diav,dia,idia
eef=rinter(diaen,idia,ien)
print,eef
stop
end
