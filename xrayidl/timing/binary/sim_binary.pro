pro sim_binary,ta,pp,nsc,ts,phi=phi,frac=frac,tol=tol,seed=seed
;+
; Simulate the arrival time of counts, assuming a sinusoid wave and allowing
; for gaps between good time intervals.
;
;*Inputs:
; ta - the 2-D good time interval array: the first dimension store
;	the start and stop time of each interval.
; pp - period 
; nsc - number of simulated counts
;*Outputs:
; ts - a vector of photon arrival time
;*Optional Inputs:
; phi - the initial phase (0 < phi < 2!pi), def=0
; frac - the amplitude fraction, compared to the average flux
; tol - the tolerance of ts used in the nonlinear inversion 
; seed - the seed value for the simulation
;
;*Limitation:
; The accumulated function needs to be smooth enough; the nonlinear 
; conversion of the accumulated function in afreverse
; depends some inital guesses of an inverted value. This means that
; the period should not be too much small (say, <10%) than the width of time
; intervals and/or that the pulsed fraction cannot be too large (e.g., 
; say, < 50%).
; When the printed value "max deviation from 0 " not small, you are in
; trouble!!!
;
;*Example:
; IDL> ta=fltarr(2,2)
; IDL> ta(*)=[0.,1000.,2000.,4000.]
; IDL> sim_binary,ta,500.,6000,ts,frac=0.5
; IDL> plot,histogram(ts,bin=10,min=0,max=4000)
;
; written by wqd, 7/28/2000
;-
if n_elements(tol) eq 0 then tol=1.e-7
if n_elements(phi) eq 0 then phi=0.
if n_elements(frac) eq 0 then frac=0.5
ppn=pp/(2.*!pi) ;just normalization of convenience
sz=size(ta)
if sz(0) ne 2 then stop,'ta must be a 2-D time array' ;just simple check
nt=sz(2) ; total time intervals
afun=fltarr(nt+1) ;to store accumulated function
ifun=fltarr(nt)
; integrating over the time intervals
for k=0,nt-1 do begin
  ifun(k)=ta(1,k)-ta(0,k)+frac*ppn*(cos(ta(0,k)/ppn+phi)-cos(ta(1,k)/ppn+phi))
endfor
;now accumulate these integrations
for k=1,nt do afun(k)=afun(k-1)+ifun(k-1) 
tafun=afun(nt)
print,'tafun = ',tafun
afun=afun/tafun ;normalized so that the total=1

;get simulated random values
srv=randomu(seed,nsc)
;get the effective index of simulated counts
tabinv,afun,srv,ie 
ie=long(ie) ;indexes of simulated counts in the right time intervals
;now get the function values of the simulated counts
fv=(srv-afun(ie))*tafun
;reverse the integration to get the time values
afreverse,pp,frac,phi,ta,ie,fv,ts,tol=tol
;stop
return
end