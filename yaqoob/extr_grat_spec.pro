pro extr_grat_spec, plist, grating, elo, ehi, spec, orders=orders, $
        erange=erange, ypos=ypos, zpos=zpos, width=width, ll=ll, bin=bin
;+
; NAME:
;
; extr_grat_spec
;
; PURPOSE:
;
; Extracts a LETG, METG, or HETG spectrum from an events list
;
; CATEGORY:
;
; X-ray analysis
;
; CALLING SEQUENCE:
;
;
; 
; INPUTS:
;
; plist - events list
; grating - which grating to extract: H, M, L
;
; OPTIONAL INPUTS:
;
;
;	
; KEYWORD PARAMETERS:
;
; orders - array of which orders to extract
; width - distance from dispersion axis to go out to in extracting
;         spectra in mm, default = 1 mm ~ 20"
;
; erange - range in energies to extract, default = [0.4, 10.0]
; ll - returns list giving which photons were extracted
; bin - bin factor for output spectrum
;
; OUTPUTS:
;
; elo, ehi - low and high energy range of spectral bin
; spec - spectrum in counts
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
; Uses tg_extract to get the distances along and perp. to the
; dispersion axis for the photons from the desired grating.  Next
; discretizes the distances in units of 0.024 mm ~ 1 acis pixel.  Then
; the energy of each event is estimated from its pha value.  The
; wavelength of each event is computed assuming that they are all
; first order, and then the energy is estimated based on this.  The
; ratio of the two energy estimates then gives an estimate of the
; order of each event, used for order selection.  The bin size is
; constant in wavelength space (since the pixels are in units of
; wavelengths) which corresponds to a E^-2 bin size (hence the
; necessity for elo and ehi).
;
; EXAMPLE:
;
; extr_grat_spec, plist, 'H', elo, ehi, spec
;
; MODIFICATION HISTORY:
;
; Created by A. Ptak, ptak@astro.phys.cmu.edu, 12/97
; Added binning, output sorted by energy 1/98
;-

if n_params(0) eq 0 then begin
  print, 'extr_grat_spec, plist, grating, elo, ehi, spec, orders=orders, $'
  print, '  erange=erange, width=width, ll=ll, bin=bin'
  print, 'grating = "H", "M" or "L"'
  print, 'elo, ehi, spec - returned spectrum with each bin representing the
  print, '                 range elo to ehi'
  print, 'NB., the bin size is NOT constant because what is extracted are the
  print, 'counts in each pixel.  The size of each pixel is constant in
  print, 'wavelength space but goes as energy^-2'
  print, 'orders gives orders to select using PHA, default orders = [1]'
  print, 'erange gives the energy range to return, default = [0.4, 10.0]'
  print, 'width gives distance from dispersion axis to go out to, in mm (def.=1)'

;  print, 'Optionally ypos and zpos (as given by read_marx_data) can be given'
;  print, 'instead of plist'
  return
endif

gratings = "HML"
if n_elements(orders) eq 0 then orders = [1]
if n_elements(erange) eq 0 then erange = [0.4, 10.0]
if n_elements(width) eq 0 then width = 0.2424
if n_elements(bin)  eq 0 then bin = 1
grat = strpos(gratings, strupcase(grating))
if grat eq -1 then begin
  print, 'Error... grating must be H, M or L'
  retall
endif

temp = ['High', 'Medium', 'Low']
print, temp(grat) + '-Energy Grating'

print, 'Cross-dispersion annulus width = ' + strn(width) + ' mm'
maxmm = [45., 45., 70.]

if n_elements(ypos) gt 0 then tg_extract, grating, width, ypos, zpos, 0.0, $
  0.0, tg_r, tg_d, tg_l else $
  tg_extract, grating, width, plist.ypos, plist.zpos, 0.0, 0.0, tg_r, tg_d, tg_l

e = acis_pha_to_kev(plist(tg_l)) ; Energy estimate from PHA values

; Add a little bit of realism by converting units of distance along dispersion
; axis from mm to pixels.  This assumed that the conversion from pixels to
; distance in mm will be exact after launch (we'll see).
pix = fix(abs(tg_r)/0.024)
; Now compute conversion from pixels (in mm) to wavelength
fl = 8634d ; distance from grating to detector in mm.
periods = [2000.81, 4001.41, 9912.5] ; grating periods in Ang.
; m*lambda = p*sin(theta)
; m = order #
; lambda = wavelength
; p = grating period
; sin(theta) ~ (dist. along disp. axis)/fl = pix/fl
; Get wavelength assuming all events are 1st order, then use pha energy

pixsize =0.024*periods(grat)/fl
print, 'Pixel size = ' + strn(pixsize) + ' Angstroms/pix'
wave1 = pix*pixsize
w = where(wave1 gt 0, n)
if n eq 0 then begin
  print, 'Something is wrong... no positive wavelengths"
  stop
endif
E1 = 12.4/wave1(w)
m = e(w)/E1
o = -1

; Okay, now select orders
; given the dispersion eq. the maximum wavelength that can be detected
; is from the 1st order, at the most distant pixel
nspec = fix(maxmm(grat)/pixsize/bin)
spec = fltarr(nspec)
wave = (findgen(nspec)+1)*pixsize*bin
maxwave = max(wave)
res = figure_acis_res(plist(w), e=e(w))
; ll = list of selected photons
ll = -1
for i=0, n_elements(orders)-1 do begin
  w1 = where(m gt (1-res)*orders(i) and m lt (1+res)*orders(i), n)
  print, strn(n) + ' events in order ' + strn(orders(i))
  if n gt 0 then begin
      spec1 = histogram(wave1(w(w1))/orders(i), bin=pixsize*bin, min=pixsize, $
                        max=maxwave)
      if n_elements(spec1) ne n_elements(spec) then stop
      if total(spec1) ne n then begin
          print, 'Warning... ' + strn(n - total(spec1)) + ' counts "truncated" from'
          print, 'order ' + strn(orders(i))
          if i gt 0 then print, 'This may be due to counts lost in the first pixel'
       endif
;      print,total(spec1)
      spec = spec + spec1
      ll = [ll, tg_l(w(w1))]
  endif
endfor
n = total(spec)
print, 'Selected ' + strn(n) + '/' + strn(n_elements(plist)) + $
  ' events total, ' + strn(100.*n/n_elements(plist)) + '%'
if n gt 0 then ll = ll(1:*)
if n ne n_elements(ll) then print, strn(n_elements(ll) - n) + $
  ' counts truncated in total'
elo = 12.4/(wave + 0.5*pixsize*bin)
ehi = 12.4/(wave - 0.5*pixsize*bin)
ww = where(elo ge erange(0) and ehi le erange(1))
spec = spec(ww)
elo = elo(ww)
ehi = ehi(ww)
s = sort(elo)
elo = elo(s)
ehi = ehi(s)
spec = spec(s)
return
end
