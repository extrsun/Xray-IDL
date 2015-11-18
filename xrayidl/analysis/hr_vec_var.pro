pro hr_vec_var,yo,yoe,yo1,yo1e,hr,hre,xo,xhr,xlo,xhi,ston=ston,elimit=elimit,xhrlo=xhrlo,xhrhi=xhrhi
;-
; adaptively calculate a hardness ratio vector. 
;
;*INPUTS:
; yo, yoe - soft band flux vector and its error
; yo1, yo1e - hard band flux vector and its error
;*OUTPUTS:
; hr, hre - hardness ratio vector and its error
;
; xo - optional vector to be binned together (e.g., distance vector)
; xhr - binned xo
; xlo, xhi - lower and upper limits to xo
; xhrlo, xhrhi - binned xlo and xhi
;
; ston - minimum S/N ratio to be achieved (def = 5)
; elimit - absolute total flux error to be achieved  when the S/N cannot be
;          achieved (def = median(yot)*0.03), needed when S ~ 0.
;
; wqd, May 21, 2005
;+
if n_params() eq 0 then begin
print,'hr_vec_var,yo,yoe,yo1,yo1e,hr,hre,xo,xhr,xlo,xhi,ston=ston'
print,',elimit=elimit,xhrlo=xhrlo,xhrhi=xhrhi'
return
endif 
np=n_params()
if np gt 7 then xavg=1 else xavg=0
if n_elements(ston) eq 0 then ston=5.

yot=yo+yo1
yote=sqrt(yoe^2+yo1e^2)
if n_elements(elimit) eq 0 then elimit=median(yot)*0.03 ;5.e-5
nb=n_elements(yo)

y=[-999]
ye=[-999]
if xavg ne 0 then x=[-999]
n1=0
n2=0
if np gt 9 then begin
    xhrlo=[-999.] & xhrhi=[-999.]
endif 
while n2 le nb-1 do begin
	avg_least,yo(n1:n2),yoe(n1:n2),yy,yye
	avg_least,yo1(n1:n2),yo1e(n1:n2),yy1,yy1e
        terr=sqrt(yye^2+yy1e^2)
	if (yy/yye < yy1/yy1e) gt ston or terr lt elimit then begin
		y=[y,(yy1-yy)/(yy1+yy)]
		ye=[ye,2./(yy1+yy)^2*sqrt(yy^2*yy1e^2+yy1^2*yye^2)]
		if xavg ne 0 then begin
			avg_least,xo(n1:n2),yote(n1:n2),xx
			x=[x,xx]
                        if np gt 9 then begin
                            xhrlo=[xhrlo,xlo(n1)] & xhrhi=[xhrhi,xhi(n2)]
                        endif 
		endif
		n1=n2+1
		n2=n1
	endif else begin
		n2=n2+1
	endelse
if !debug eq 1 then stop
endwhile

hr=y(1:*)
hre=ye(1:*)
if xavg ne 0 then xhr=x(1:*)
if np gt 9 then begin
    xhrlo=xhrlo(1:*) & xhrhi=xhrhi(1:*)
endif 
end
