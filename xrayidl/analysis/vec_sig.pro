pro vec_sig,yo,yoe,xo,ston=ston,elimit=elimit
;-
; bin a vector to achieve minimum S/N ratio and minimum error limit
; yo, yoe - data and their errors
;
; xo - optional to be binned together (e.g., distance vector)
; ston - minimum S/N ratio to be achieved (def = 3)
; elimit - absolute elimit to be achieved  when the S/N cannot be
;	achieved (def = 0.)
;
; wqd, 9/4/95
;+
if n_params() eq 0 then begin
print,'vec_sig,yo,yoe,xo,ston=ston,elimit=elimit'
return
endif 

if n_params() eq 3 then xavg=1 else xavg=0
if n_elements(ston) eq 0 then ston=3.
if n_elements(elimit) eq 0 then elimit=5.e-5
nb=n_elements(yo)

y=[-999]
ye=[-999]
if xavg ne 0 then x=[-999]
n1=0
n2=0
while n2 le nb-1 do begin
	avg_least,yo(n1:n2),yoe(n1:n2),yy,yyme
	if yy/yyme gt ston or yyme lt elimit then begin
		y=[y,yy]
		ye=[ye,yyme]
		if xavg ne 0 then begin
			avg_least,xo(n1:n2),yoe(n1:n2),xx
			x=[x,xx]
		endif
	if yyme lt 1.e-6 then stop
		n1=n2+1
		n2=n1
	endif else begin
		n2=n2+1
	endelse
if !debug eq 1 then stop
endwhile

yo=y(1:*)
yoe=ye(1:*)
if xavg ne 0 then xo=x(1:*)
end
