;;; $Id: maxlik.pro 3000 2008-03-03 16:40:43Z patb $
;;; Simple wrapper around Max_Likelihood routine in AstroLib.
;;; data_filename is name of FITS file with data image in primary HDU.
;;; psf_filename is name of FITS file with PSF  image in primary HDU.
;;; Niter is number of Max_Likelihood iterations desired.  There is no automatic
;;; stopping criterion.

;;; Use /SAVE to write ~28 intermediate images to disk.
;;; Use /PLOT to display ~28 intermediate images via TARA function_2d tool.

;;; Final reconstruction image is returned in parameter maxlik.

PRO maxlik, data_filename, psf_filename, Niter, maxlik_img, maxlik_header, SAVE=save, PLOT=plot

creator_string = "maxlik.pro, $Revision: 3000 $"

; Force data to be single precision for speed on 32-bit processors.
data_img= float( readfits(data_filename, data_header) )
print, 'Total counts in data image:', total(data_img)

; Get ready to save intermediate images to disk if requested.
save_it = (indgen(Niter) mod ceil(Niter/28.)) EQ 0
save_it[Niter-1] = 1

if keyword_set(save) then begin
  files_to_remove = findfile( 'maxlik*_'+data_filename, COUNT=count)
  if (count GT 0) then begin
    print, 'removing ', files_to_remove
    file_delete, files_to_remove, /QUIET
  endif

  save_fn = string(1+indgen(Niter), F='(%"maxlik%3.3d_")') + data_filename
  save_ind = where(save_it)
endif


tit=string(data_filename,psf_filename,f='(A," maxlik deconvolved by ",A)')

if keyword_set(plot) then $
  function_2d,id,data_img, DATASET_NAME='data_img',WIDGET_TITLE=tit,/UNITY_ASPECT


psf_img = readfits(psf_filename, psf_header)

energy = sxpar(psf_header, 'ENERGY')
if keyword_set(energy) then print, energy, F='(%"PSF energy is %0.1f keV")' $
                       else print, 'PSF energy is unknown.'


; Set all the NaN values to zero to keep future computations happy.
ind = where(finite(psf_img) EQ 0, count)
if (count GT 0) then psf_img[ind] = 0


; Verify that data & PSF images have the same pixel sizes.
extast, data_header, data_astr
extast, psf_header,   psf_astr
arcsec_per_psfpixel = psf_astr.CDELT[1] * 3600

if (total([data_astr.CDELT, psf_astr.CDELT] EQ 0) GT 0) then begin
  print, '!!!!!!!!!!!!!!!!!!!!!!'
  print, 'WARNING!  Astrometry missing; cannot verify that data & PSF pixels sizes match.'
  print, '!!!!!!!!!!!!!!!!!!!!!!'
endif else begin
  mismatch = abs((data_astr.CDELT - psf_astr.CDELT) / data_astr.CDELT)
  if (max(mismatch) GT 0.01) then begin
  print, '!!!!!!!!!!!!!!!!!!!!!!'
  print, 'WARNING!  Data & PSF pixels sizes DO NOT match!'
  print, 'CDELT from data:', data_astr.CDELT
  print, 'CDELT from PSF :', psf_astr.CDELT
  print, '!!!!!!!!!!!!!!!!!!!!!!'
  endif
endelse

help, data_img, psf_img

;; The correlations performed in Max_Likelihood.pro have the property that the
;; position of the point source in the PSF image is assumed to be:
;; a) the center of the central pixel if the PSF dimension is odd
;; b) the center of the pixel N/2 if the PSF dimension is even
;; Conveniently the formula fix(N/2) produces the correct value in both cases.
;;
;; One way to think about what I mean by the above is demonstrated by reconstructing
;; the PSF image itself (as demonstrated by the test program test_maxlik.pro).
;; When the PSF dimension is odd the resulting delta function in the output falls
;; at the central pixel.
;; When the PSF dimension is even the resulting delta function in the output falls
;; at pixel N/2 (which is just past the "4-corners middle" of the image).
;;
;; The CIAO PSF images do not follow that convention however.  (They seem to 
;; usually have even dimensions, and the point source is on or near the
;; "4-corners middle" of the image.)
;;
;; Thus, to assign meaningful astrometry to the reconstructed image we just account
;; for the offset between the Max_Likelihood convention and the actual source position
;; in the PSF image.

; Find PSF pixel index coordinates of source position.
ra  = sxpar(psf_header, 'RA',  COUNT=count1)
dec = sxpar(psf_header, 'DEC', COUNT=count2)
if (count1+count2 NE 2) then begin
  ra  = 0D
  dec = 0D
  print, 'Enter RA and DEC (in decimal degrees) for which the PSF in '+psf_filename+' was constructed.'
  read, ra, dec
endif

ad2xy, ra, dec, psf_astr, src_column, src_row

psf_xdim = sxpar(psf_header, 'NAXIS1')
psf_ydim = sxpar(psf_header, 'NAXIS2')

xoffset = src_column - fix(psf_xdim/2)
yoffset = src_row    - fix(psf_ydim/2)

; Build a header for the reconstructed image.  WCS is taken from data image,
; and adjusted by offsets above.  I don't copy & adjust the PHYSICAL (sky)
; coordinate system because I can't figure out how LTV* & LTM* work.
mkhdr, maxlik_header, data_img, /IMAGE

extast, data_header, dataimg2wcs_astr
maxlik2wcs_astr    = dataimg2wcs_astr
maxlik2wcs_astr.CRPIX = maxlik2wcs_astr.CRPIX - [xoffset,yoffset]
putast, maxlik_header, maxlik2wcs_astr    


fxaddpar, maxlik_header, 'HDUNAME', 'Max_Likelihood'
fxaddpar, maxlik_header, 'CREATOR', creator_string
 
    
; Perform reconstruction iterations, saving & displaying intermediate results
; if desired.

;  The convolutions done in the recon wrap around the image edges.  To avoid artifacts we
;  pad the data image on the right and upper sides with a border equal in size to the PSF.
;
;  !!!!  
;  For reasons I cannot fathom, flux is NOT CONSERVED in Max_Likelihood.pro if the data image has odd dimensions.
;  !!!!  
;    
;  Make data_img & normalized_psf single precision to help speed in Max_Likelihood
;  on 32-bit processors.  On 64-bit processor it seems to make no difference.
img_xdim = (size(data_img, /DIM))[0]
img_ydim = (size(data_img, /DIM))[1]

padded_xdim = img_xdim + psf_xdim
padded_ydim = img_ydim + psf_ydim

padded_xdim += (padded_xdim MOD 2)
padded_ydim += (padded_ydim MOD 2)   

padded_img = fltarr(padded_xdim, padded_ydim)
padded_img[0,0] = data_img
  
; Normalize PSF.
; Force PSF to be single precision for speed on 32-bit processors.
normalized_psf = float(psf_img / total(psf_img, /DOUBLE))

print, Niter, F='(%"reconstructing with %d ML iterations ...")'
help, data_img, padded_img, normalized_psf
maxlik_img = 0
psf_ft     = 0
for ii=0,Niter-1 do begin
  print, 'iteration', 1+ii
  ; If you use the /NO_FT option it runs much faster but the result has a border of zeros.
  Max_Likelihood, padded_img, normalized_psf, maxlik_img, FT_PSF=psf_ft
  
  if (keyword_set(save) AND save_it[ii]) then begin
    fxaddpar, maxlik_header, 'ML_ITERS', 1+ii
    writefits, save_fn[ii], maxlik_img[0:img_xdim-1, 0:img_ydim-1], maxlik_header
    print, 'wrote ', save_fn[ii]
  endif
  
  if (keyword_set(plot) AND save_it[ii]) then $
    function_2d,id,maxlik_img[0:img_xdim-1, 0:img_ydim-1], DATASET_NAME=string(ii)
endfor

; Remove padding from recon image.
maxlik_img = maxlik_img[0:img_xdim-1, 0:img_ydim-1]
help, maxlik_img
print

; Verify that global flux has been preserved, since we've had trouble with this before!
data_sum  = total(data_img)
recon_sum = total(maxlik_img)
if (data_sum GT 0) && (abs(data_sum-recon_sum)/data_sum GT 0.10) then begin
  print, 'WARNING! Global flux in the data and recon images differs by >10%.'
  help, data_sum, recon_sum
endif 
  
print, [xoffset, yoffset] * arcsec_per_psfpixel, F='(%"\nReconstruction astrometry has been adjusted for the (%0.2f, %0.2f) arcsec offset between the center of the PSF image and the source position.")'
;help, xoffset, yoffset

if keyword_set(plot) then $
  function_2d,id,maxlik_img, DATASET_NAME=string(ii)

return
end
