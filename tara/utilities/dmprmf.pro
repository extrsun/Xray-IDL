;;; $Id: dmprmf.pro 2180 2005-04-26 11:38:20Z patb $
;;; Program to display an RMF (response matrix) file; similar to FTOOL by
;;; the same name.
;;;
;;; PRO dmprmf, filename, img, ENERGIES=energies
;;; ENERGIES in keV
;===========================================================================


PRO expand_row, row, num_channels, spectrum

spectrum = fltarr(num_channels)

i_start = 0
for ii =0,row.n_grp-1 do begin
  f_chan = (row.f_chan)[ii] - 1
  n_chan = (row.n_chan)[ii]
  
  group = (row.matrix)[i_start:i_start+n_chan-1]
  i_start = i_start + n_chan
  
  spectrum[f_chan:f_chan+n_chan-1] = spectrum[f_chan:f_chan+n_chan-1] + group
endfor
return
end


;===========================================================================
PRO dmprmf, filename, img, ENERGIES=energies, QUIET=quiet

fits_open, filename, fcb, /NO_ABORT, MESSAGE=error
if keyword_set(error) then begin
  print, 'Cannot open ', filename
  return
endif
  
ebounds_extno  = (where( strmatch(fcb.EXTNAME, 'EBOUNDS', /FOLD_CASE), count ))[0]
specresp_extno = (where( strmatch(fcb.EXTNAME, 'SPECRESP*', /FOLD_CASE), count ))[0]
if (count EQ 0) then $
  specresp_extno = (where( strmatch(fcb.EXTNAME, 'MATRIX', /FOLD_CASE), count ))[0]


fits_close, fcb

t1=mrdfits(filename, specresp_extno)
t2=mrdfits(filename, ebounds_extno)

num_energies = n_elements(t1)
num_channels = n_elements(t2)

img = fltarr(num_channels,num_energies)

peak = intarr(num_energies)

energy_chan = (t2.E_MAX+t2.E_MIN)/2.
energy_ebin = (t1.ENERG_HI+t1.ENERG_LO)/2.

for ii=0,num_energies-1 do begin
  expand_row, t1[ii], num_channels, spectrum
  img[*,ii] = spectrum
  
  dum = max(spectrum, imax)
  peak[ii] = imax
endfor

if keyword_set(quiet) then return

make_2d, 1+indgen(num_channels), energy_ebin, channel, photon_energy
N = n_elements(img)

dataset_3d,id5, reform(channel,N), reform(photon_energy,N), reform(img,N), STAT_CODE=0, XTIT='Channel # (1-based)', YTIT='Photon Energy', XBIN=1, YBIN=0.1, DATASET=filename

function_2d,id2, img, XTIT='Channel # (1-based)', YTIT='Photon Energy Bin (1-based)',  X0=1, DELTA_X=1, Y0=1, DELTA_Y=1, DATASET=filename, UNITY=0

dataset_2d,id1, peak+1, energy_ebin, XTIT='Channel # (1-based) at peak response',  YTIT='Photon Energy (keV)', DATASET=filename, PSYM=3

function_1d,id3, energy_ebin/energy_chan[peak], energy_ebin, XTIT='(Photon Energy)/(Nominal Energy of Channel at peak response)', YTIT='Photon Energy (keV)', DATASET=filename

for ii=0,n_elements(energies)-1 do begin
  e_bin = (where(energies[ii] LE t1.ENERG_HI))[0]
  function_1d,id4, 1+indgen(num_channels), img[*,e_bin], XTIT='Channel # (1-based)', YTIT='Response', DATASET=string(energies[ii]), TITLE=filename, COLOR='white', LINE=0
endfor

return
end
