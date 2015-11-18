bg=fltarr(3)
bg(0)=10.5
bg(1)=6.5
bg(2)=2.2
texp=fltarr(3) ; true exposure / nominal exposure
texp(0)=0.8
texp(1)=0.92
texp(2)=0.85
eta=0.9
;s - detected source counts
;mu0 - true source counts

; outfile
rmffile='test.rmf'
rsfil=rmffile

; initialize seed
seed=1001L

; number of simulations
n = 100000

; number of energy bins
nenerg = 14
nchan = 43

; initialize array to store matrix
p1 = replicate(0.0,n)

; define the energy (true source counts) boundaries
elo=[-999.0]
ehi=[-999.0]
emi=[-999.0]

n_grp = replicate(1,nenerg)
f_chan = intarr(nenerg)
n_chan = intarr(nenerg)
rmf = fltarr(nenerg,nchan)

; initialize RMF file with primary header
fxhmake, h0, /extend, /date
fxwrite,rsfil,h0

; make extension table header
fxbhmake,h1,nenerg,'MATRIX'

fxbaddcol, elo_index, h1, elo(0), 'ENERG_LO', tunit='keV'
fxbaddcol, ehi_index, h1, ehi(0), 'ENERG_HI', tunit='keV'
fxbaddcol, ngp_index, h1, n_grp(0), 'N_GRP'
fxbaddcol, fch_index, h1, f_chan(0), 'F_CHAN'
fxbaddcol, nch_index, h1, n_chan(0), 'N_CHAN'
fxbaddcol, rsp_index, h1, rmf(0,*), 'MATRIX', VARIABLE=1.


; write in OGIP standard keywords
comment='Telescope (mission) name'
fxaddpar, h1, 'TELESCOP', 'NONE', comment

comment='Instrument name'
fxaddpar, h1, 'INSTRUME', 'NONE',comment

comment='Instrument filter'
fxaddpar, h1, 'FILTER', 'NONE' , comment

comment='OGIP Format version'
fxaddpar, h1, 'RMFVERSN', '1992a', comment

comment='Detector Channel Type in use (PHA or PI)'
fxaddpar, h1, 'CHANTYPE', 'PI', comment

comment='Total number of detector channels'
fxaddpar, h1, 'DETCHANS', nchan, comment

comment='Format conforms to OGIP standard'
fxaddpar, h1, 'HDUCLASS', 'OGIP', comment

comment='dataset relates to spectral response'
fxaddpar, h1, 'HDUCLAS1', 'RESPONSE', comment

comment='dataset is a spectral response matrix'
fxaddpar, h1, 'HDUCLAS2', 'RSP_MATRIX', comment

comment='photon redistribution matrix'
fxaddpar, h1, 'HDUCLAS3', 'REDIST', comment

comment='Version of format (OGIP memo CAL/GEN/92-002a)'
fxaddpar, h1, 'HDUVERS', '1.3.0', comment

comment='Obsolete - included for backwards compatibility'
fxaddpar, h1, 'HDUVERS1', '1.3.0', comment

comment='Lower threshold used to construct the matrix'
fxaddpar, h1, 'LO_THRES', '1.0e-5', comment

comment='Minimum value legally allowed in FCHAN column'
fxaddpar, h1, 'TLMIN4', 0, comment

comment='Minimum value legally allowed in FCHAN column'
fxaddpar, h1, 'TLMAX4', nchan-1, comment

; add extension header to file
fxbcreate, unit, rsfil, h1

for i = 0, nenerg-1 do begin
	s = float(i)
	mu0=eta*texp(0)*s+bg(0)
	print, i, mu0
	vec = replicate(mu0,n)
	if ( mu0 le 20.0) then begin
		p1 = poissonq(vec,seed)
	endif else begin
		p1 = randomu(seed,n,/normal)*sqrt(mu0)+mu0
	endelse
	plothist, p1, xarr, yarr, halfbin=1.

	; get the energy boundaries
	if ( s-0.5 ge 0.0 ) then begin
		elo=[elo,s-0.5]
	endif else begin
		elo=[elo,0.0]
	endelse
	ehi=[ehi,s+0.5]
	emi=[emi,s]
	fxbwrite, unit, elo(i+1), elo_index, i+1
	fxbwrite, unit, ehi(i+1), ehi_index, i+1
	fxbwrite, unit, n_grp(i), ngp_index, i+1
	f_chan(i) = fix(xarr(0))
	n_chan(i) = n_elements(xarr)
	fxbwrite, unit, f_chan(i), fch_index, i+1
	fxbwrite, unit, n_chan(i), nch_index, i+1
	fxbwrite, unit, yarr/float(n), rsp_index, i+1
endfor

; close file
fxbfinish, unit

; get rid of the zeros
elo=elo(1:*)
ehi=ehi(1:*)
emi=emi(1:*)

; define the energies of channel (observed source counts) boundaries
lower=findgen(nchan)
hiher=lower
middl=lower
lower(1:*)=lower(1:*)-0.5
hiher=hiher+0.5

; make extension table header
fxbhmake,h2,nchan,'EBOUNDS'

fxbaddcol, chan_index, h2, 0, 'CHANNEL'
fxbaddcol, enlo_index, h2, lower(0), 'E_MIN', tunit='keV'
fxbaddcol, enhi_index, h2, hiher(0), 'E_MAX', tunit='keV'


; write in OGIP standard keywords
comment='mission/satellite name'
fxaddpar, h2, 'TELESCOP', 'NONE', comment

comment='instrument/detector name'
fxaddpar, h2, 'INSTRUME', 'NONE',comment

comment='filter in use'
fxaddpar, h2, 'FILTER', 'NONE' , comment

comment='OGIP classification of FITS format'
fxaddpar, h2, 'RMFVERSN', '1992a', comment

comment='Detector Channel Type in use (PHA or PI)'
fxaddpar, h2, 'CHANTYPE', 'PI', comment

comment='Total number of detector channels'
fxaddpar, h2, 'DETCHANS', nchan, comment

comment='Format conforms to OGIP standard'
fxaddpar, h2, 'HDUCLASS', 'OGIP', comment

comment='dataset relates to spectral response'
fxaddpar, h2, 'HDUCLAS1', 'RESPONSE', comment

comment='dataset is a spectral response matrix'
fxaddpar, h2, 'HDUCLAS2', 'EBOUNDS', comment

comment='photon redistribution matrix'
fxaddpar, h2, 'HDUCLAS3', 'REDIST', comment

comment='Version of format (OGIP memo CAL/GEN/92-002a)'
fxaddpar, h2, 'HDUVERS', '1.3.0', comment

comment='Obsolete - included for backwards compatibility'
fxaddpar, h2, 'HDUVERS1', '1.3.0', comment

fxbcreate, unit, rsfil, h2

for i = 0, nchan-1 do begin
	fxbwrite, unit, i, chan_index, i+1
	fxbwrite, unit, lower(i), enlo_index, i+1
	fxbwrite, unit, hiher(i), enhi_index, i+1
endfor

; close file
fxbfinish, unit

end
