pro sisbr,spha,phaout,bsz=bsz
; Transforms 'bright mode' SIS spectrum into equal channel form
; INPUTS
; 'SPHA'=> list of source event pha
; OUTPUTS
; 'PHAOUT'=> Transformed pha vector
if n_params(0) eq 0 then begin
print,'SISBR,spha,phaout,bsz=bsz'
print,'Transforms ''bright mode'' SIS spectrum into equal channel form'
retall
end 
if (n_elements(bsz) eq 0) then bsz=2
a1=histogram([0,spha],max=1023,binsize=bsz*4)
a2=histogram(spha,min=1024,max=1535,binsize=bsz*2)
a3=histogram(spha,min=1536,max=2047,binsize=bsz)
phaout=[a1,a2,a3] & phaout(0)=0
return
end
