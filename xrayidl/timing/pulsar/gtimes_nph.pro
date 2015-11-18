pro gtimes_nph,tbeg,tend,ff,ffd,phn,nph
;+
; calculate the phase numbers included in good time intervals
;
; tbeg, tend - the starting and ending time of the observation
; ff - assumed frequency of the light curve variation
; ffd - the derivative of ff
; 
sel=where(tend-tbeg ne 0.) ;remove zero intervals 
tbegn=tbeg(sel) > 0.0d ;due to accuracy of the digits
tendn=tend(sel)
;
; get phase number
phbeg=long((ff+0.5d*ffd*tbegn)*tbegn+0.5d)
phend=long((ff+0.5d*ffd*tendn)*tendn+0.5d)
phdif=phend-phbeg+1L
nph=long(total(phdif,/double)) ;total number of phases included 
;
; put into a vector
phn=[-999L]
for k=0,n_elements(tbegn)-1 do begin
	phn=[phn,phbeg(k)+lindgen(phdif(k))]
endfor
phn=phn(1:*)
return
end