pro write_rsp,outfname,esv,sv,rsp,f_chan,n_chan
;+ 
;*Name:
;     write_rsp - write a standard fits file of response matrices
;
;*Inputs:
; esv - vector contains the boundaries of expected source flux channels. 
; sv - flux bounaries of the x-axis bins (in units of counts/s)
; rsp - the response matrices: 1st dim - detected source flux; 2rd -
;       intrinsic source flux
; f_chan - starting x-axis index values of the non-zero rsp
; n_chan - index lengths of the non-zero rsp
; outfname - the output rsp file name

; Written by D. Smith and wqd, Oct 19, 2003
;-
if n_params() eq 0 then begin
print,'Calling Seq - write_rsp,outfname,esv,sv,rsp,f_chan,n_chan'
return
endif 

nenergy=n_elements(esv)-1
n_grp = replicate(1,nenergy)
elo=esv(0:nenergy-1)
ehi=esv(1:*)

;nchan=n_elements(minind)
nchan=n_elements(sv)-1
lower=sv(0:nchan-1)
hiher=sv(1:*)
e_chan=f_chan+((n_chan-1) > 0) ;ending channel indeces

; initialize RMF file with primary header
fxhmake, h0, /extend, /date
fxwrite,outfname,h0

; make extension table header
fxbhmake,h1,nenergy,'MATRIX'

fxbaddcol, elo_index, h1, elo(0), 'ENERG_LO', tunit='keV'
fxbaddcol, ehi_index, h1, ehi(0), 'ENERG_HI', tunit='keV'
fxbaddcol, ngp_index, h1, n_grp(0), 'N_GRP'
fxbaddcol, fch_index, h1, f_chan(0), 'F_CHAN'
fxbaddcol, nch_index, h1, n_chan(0), 'N_CHAN'
fxbaddcol, rsp_index, h1, rsp(*,0), 'MATRIX', VARIABLE=1.

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
fxbcreate, unit, outfname, h1

for i = 0, nenergy-1 do begin
	fxbwrite, unit, elo(i), elo_index, i+1
	fxbwrite, unit, ehi(i), ehi_index, i+1
	fxbwrite, unit, n_grp(i), ngp_index, i+1 
	fxbwrite, unit, f_chan(i), fch_index, i+1 
	fxbwrite, unit, n_chan(i), nch_index, i+1
	fxbwrite, unit, rsp(f_chan(i):e_chan(i),i), rsp_index, i+1 
endfor

; close file
fxbfinish, unit

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

fxbcreate, unit, outfname, h2

for i = 0, nchan-1 do begin
	fxbwrite, unit, i, chan_index, i+1
	fxbwrite, unit, lower(i), enlo_index, i+1
	fxbwrite, unit, hiher(i), enhi_index, i+1
endfor

; close file
fxbfinish, unit
return 
end
