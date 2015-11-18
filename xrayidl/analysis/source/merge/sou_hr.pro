pro sou_hr,ls,hr,hr2,hre,hr2e
;+
; calculate hardness ratios from a source structure
;
; ls - source structure
; hr, hr2 - HR1 and HR2
; hre, hr2e - errors of HR1 and HR2
;
; written by wqd, 3/23/2002
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sou_hr,ls,hr,hr2,hre,hr2e'
return
endif
cc1=ls.cntrb1 > 0.
cc2=ls.cntrb2 > 0.
cc3=ls.cntrb3 > 0.
cc1e=ls.cntrb1e
cc2e=ls.cntrb2e
cc3e=ls.cntrb3e

hr=imdiv(cc2+cc3-cc1,cc2+cc3+cc1)
hr2=imdiv(cc3-cc2,cc2+cc3)
hr2e=2/(cc2+cc3 > 0.00001)^2*sqrt((cc2e*cc3)^2+(cc3e*cc2)^2)
hre=2/(cc1+cc2+cc3 > 0.00001)^2*sqrt((cc1e*(cc3+cc2))^2+((cc3e^2+cc2e^2)*cc1^2))
return
end