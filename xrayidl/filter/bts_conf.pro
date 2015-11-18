pro bts_conf,para,parasm,paraelo,paraehi,siglevel=siglevel,pri=pri
;+
; calculate confidece limits of parameters based fits to resample data
; wqd, 7/15/96
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - bts_conf,para,parasm,paraelo,paraehi,siglevel=siglevel,pri=pri'
return
endif
sz=size(para)
length=sz(1)
paraelo=fltarr(length)
paraehi=paraelo
parasm=paraelo
for k=0,length-1 do begin
	avg_median,para(k,0:*),param,parae1,parae2,siglevel=siglevel
	paraelo(k)=parae1
	paraehi(k)=parae2
	parasm(k)=param
endfor

if n_elements(pri) ne 0 then begin
print,'para(k,0),parasm(k),paraelo(k),paraehi(k)'
for k=0,length-1 do print,para(k,0),parasm(k),paraelo(k),paraehi(k)
endif
return
end
