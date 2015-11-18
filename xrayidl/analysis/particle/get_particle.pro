;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; get_particle
;
;*PURPOSE:
; calculate particle count distribution as a function of angles
; in a PSPC image with the algorithm described by SLS
;
;*CALLING SEQUENCE:
; get_particle,mv,radius,chanlow,chanhigh,pcount,dtype=dtype
;
;*PARAMETERS:
; INPUTS:
; MV - the Master Vetor count rate as the output from the file *.evr (vector)
; radius - the off-axis radius (in arcminutes, vector)
; chanlow,chanhigh - the lower and upper limits of the ADC spectral 
;			channel number (8-249)
; dtype - the PSPC type, primary (C) or secondary (B --- default)
;
;*OUTPUTS:
; pcount - the total particle counts per arcmin^2 (summed over MV and the 
;         channel intervals) as a function of  the input angles
;
;*PROCEDURE:
; using the algorithm devised by SLS (ApJ  393, 819)
;
;*EXAMPLES:
;
;*RESTRICTIONS:
; The algorithm is good for MV < 170 counts/s
;
;*NOTES:
; none
;
;*SUBROUTINES CALLED:
; none
;
;*MODIFICATION HISTORY:
; writen by WQD, Nov 16, 1992
;
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro get_particle,mv,radius,chanlow,chanhigh,pcount,dtype=dtype
;
if n_params() eq 0 then begin
print,'CALLING SEQUECNCE --- get_particle,mv,radius,chanlow,chanhigh,pcount,dtype=dtype'
return
endif
;
lmv=1-1.4e-4*mv
if n_elements(dtype) eq 0 then dtype='B' ;default
if strupcase(dtype) eq 'C' then fi=total((0.021+8.64e-4*mv)/lmv)
if strupcase(dtype) eq 'B' then fi=total((0.018+8.64e-4*mv)/lmv)

fal=total((-0.006 + 2.44e-4*mv)/lmv)
fe =total((0.009 + 0.46e-4*mv)/lmv)
; spatial dependence
if strupcase(dtype) eq 'C' then phii=8.42e-5 + 3.95e-7*radius
if strupcase(dtype) eq 'B' then $
phii=1.02e-4 - 3.3e-5*exp(-(radius-20.6)*(radius-20.6)/12.8)
phie=1.04e-4
;
;spectral distribution
;
chan=chanlow+indgen(chanhigh-chanlow+1)
if strupcase(dtype) eq 'C' then si=total(1.36*(chan+0.5)^(-1.97)+0.00340)
if strupcase(dtype) eq 'B' then si=total(1.40*(chan+0.5)^(-1.97)+0.00338)
sal=total(0.835*(chan+0.5)^(-3./4.)* $
     exp(-0.71551*(12.247-sqrt(chan+0.5))*(12.247-sqrt(chan+0.5))))
se=0.00413*(chanhigh-chanlow+1)
;
pcount_i=fi*phii*si
pcount_al=fal*phie*sal
pcount_e=fe*phie*se
;
; sum over the three terms
;
 pcount=(pcount_i+pcount_al+pcount_e)*2. ;because of the MV step = 2 seconds
; 
end