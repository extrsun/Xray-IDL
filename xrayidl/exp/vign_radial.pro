pro vign_radial,angle,chmin,chmax,vig,specfile=specfile
;+
; calculate vignetting function for off axis angles with a user input spectrum
; angle - the off-axis angles (vector) in units of arcminutes
; specfile - the file containing the spectral data (0-33) which may be
; 	obtained with GET_SPEC_DIF within the central area of an image
; chmin,chmax - the min and max of the channels (0-33)
; vig - the output containing the vigentting fucntion at the angles
; writen by WQD, Nov 30, 1992
;-
if n_params() eq 0 then begin
print,'CALLING SEQUNENCE - vign_radial,angle,chmin,chmax,vig,specfile=specfile'
return
endif
if n_elements(specfile) eq 0 then specfile='~/rosat/expmaps/spec_b.dat'
spec=fltarr(34)
energy=(!ebnds(*,0)+!ebnds(*,1))*0.5
;
openr,un,specfile,/get
readf,un,spec
free_lun,un
;
flux=0.
vig=angle*0.
for k = chmin,chmax do begin
	off_ax_a,angle,energy(k),vig_e,ierr
	vig=vig+vig_e*spec(k)	
	flux=flux+spec(k)
endfor
vig=vig/flux
end