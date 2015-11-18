;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       psfprof
;
;*PURPOSE:
; Calculate Rosat PSPC on point spread function profile for an assumed input
;   counts spectrum
;
;*CALLING SEQUENCE:
;       psfprof,offcen,rate,offang,prof,bs=bs,group=group,ecen=ecen 
;	,sigmaw=sigmaw,prof1=prof1,defang=defang
;
;*PARAMETERS:
; INPUTS:
;       offcen  - Off axis angle of (center of) source (in arcmin)
;       rate    - vector containing observed (or assumed) counting rate for 
;                 each of the channels specified by group
;       group   - pha channel boundaries (e.g., as specified by !group)
;       ecen    - central energies
;                 Note: if group and ecen are both defined, then ecen will
;                       be redefined according to group
;	offang - off source angles in units of arcsec, overruled by the
;		 default if defang is set
;       prof    - binned (1 dimensional) PSF radial profile 
;                 Will be centered on the center of the computed PSF
;       ecen    - central energies for each element of rate
;                 will be calculated (or overwritten) if group is defined
;	sigmaw - the gaussian component width of the psf
;	prof1  - the gaussian component profile
;
;*MODIFICATION HISTORY:
;	extracted and modified version of the calcpsfp.pro. wqd, 4/18/96
;-
;-------------------------------------------------------------------------------
pro psfprof,offcen,rate,offang,prof,bs=bs,group=group,ecen=ecen $
,sigmaw=sigmaw,prof1=prof1,defang=defang
;
npar = n_params(0)
if (npar eq 0) then begin
   print,' psfprof,offcen,rate,offang,prof,bs=bs,group=group,ecen=ecen'
   print,',sigmaw=sigmaw,prof1=prof1,defang=defang'
   retall
endif 
if(n_elements(ecen) eq 0) then begin               ;use channel groupings
  ecalc = 1              ;will need to calculate central energies
  if (n_elements(group) eq 0) then group = !group   ;use !group as default
  s = size(group) & nchan = s(1)
endif else begin
  if (n_elements(group) eq 0) then begin            ;use central energies
    ecalc = 0 
    nchan = n_elements(ecen)
  endif else begin                                  ;ecen will be redefined
    ecalc = 1
    s = size(group) & nchan = s(1)
  endelse
endelse
if (ecalc) then $
   chan2energy,ecen,edel,group=group   ;calculate channel central energies

; First, create a fine off axis angles for calculating the psf
;
if n_elements(bs) eq 0 then bs=(1.+(10.-1.)/60.*offcen)
	;assuming that psf is roughly ten times larger at off-axis of 60'
	; and bs=1" on-axis
if n_elements(defang) ne 0 then offang=[0.,bs*1.03^indgen(300)] 
	;max offang = 118' for bs=1"
; these defaults are used by detect_params.pro

noffang=n_elements(offang)

;   print,' Calculating psf at various energies'
prof = fltarr(noffang)   ;psf calculated at each angle
prof1 = fltarr(noffang)  ;added by wqd
sigmaw=0.
for ii=0,nchan-1 do begin
  rspsfp,offcen,ecen(ii),offang,temp,ierr=ierr,sigma=sigma,term1=term1 
  sigmaw=sigmaw+rate(ii)*sigma
  prof = prof + rate(ii)*temp      ;weight by counts and add
  prof1 = prof1 + rate(ii)*term1
endfor  
prof = prof/total(rate)   ;renormalize profile so integral = 1
prof1 = prof1/total(rate)
sigmaw=sigmaw/total(rate)
;
return
end            ;pro calcpsfp
