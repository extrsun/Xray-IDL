pro kshell_th,zz,nn,ee
;+
; estimate the k-shell photonization energy 
; (see Band, I. M. et al. 1990, AA, 237, 267)
; written by wqd, 4/4/2004
;-
if n_params() eq 0 then begin
print,'CALLING Seq - kshell_th,zz,nn,ee'
print,'zz - proton number; nw - electron number, ee - ionization threshold (eV)'
return
end
ee=13.6*zz*zz*nn^(0.20-0.39/alog10(zz))
return
end
