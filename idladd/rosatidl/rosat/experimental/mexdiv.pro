;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;  mexdiv
;
;*PURPOSE:
; A procedure to divide a Rosat image by an exposure map, producing a
; flat fileded image
;
;*CALLING SEQUENCE:
; mexdiv,image,time,expmap,corrimg,avflux,mask,thresh=thresh,masklim=masklim
;
;*PARAMETERS:
; INPUTS:
;	image  - uncorrected Rosat image
;       time   - total exposure time, in seconds
;       expmap - mean exposure map (e.g., as calculated in make_emap)
;
; OPTIONAL INPUTS:
;       thresh  - Image will be corrected by exposure map only for those
;                 pixels where the exposure map is greater than or equal to
;                 thresh*time
;       masklim - masklim*time defines the exposure map limits which 
;                 correspond to the rib structure (used for defining avflux)
;
; OUTPUTS:
;       corrimg - Exposure map corrected image
;       avflux  - Average flux in image (averaged over pixels where
;                 exposure map is greater than or equal to masklim*time)
;       mask    - Map of pixels which were blocked out in calculating
;                 avflux
;
;*NOTES:
; The procedure first finds the maximum distance from the center for which
; the exposure map is greater than or equal to the thresh*time.
; The image is then corrected by the exposure map only for pixels within this
; distance, for which the exposure map is ge thresh*time. For pixels
; where the exposure is less than this limit, the values in the corrected
; image are set equal to image*thresh. This sets a maximum limit to the
; multiplicative correction of 1./thresh.
; The procedure also defines a mask, which is set equal to 3 inside this 
; maximum distance wherever the exposure map is less than masklim*time, and
; equal to zero everywhere else. Mask then defines where the rib structure
; and the outer edge of the exposure map are.
;
; The user may wish to experiment with the values of thresh and masklim.
;
;*MODIFICATION HISTORY:
;	written 12 Sep 1993 by GAR
;-
;-------------------------------------------------------------------------------
pro mexdiv,image,time,expmap,corrimg,avflux,mask,thresh=thresh,masklim=masklim
; 
npar = n_params(0)
if (npar eq 0) then begin
  print,' mexdiv,image,time,expmap,corrimg,avflux,mask,thresh=thresh,'+$
        ' masklim=masklim'
  retall
endif
if (n_elements(thresh) eq 0) then thresh = 0.05
if (n_elements(masklim) eq 0) then masklim = 0.17
;
s = size(image)
ns = n_elements(s)
if (ns lt 5) then begin
  print,' Image must be a 2-dimensional array. Check inputs. Returning.'
  retall
endif
nimgx = s(1)
nimgy = s(2)
;
s = size(expmap) 
nmap = s(1)
ns = n_elements(s)
if (ns lt 5) then begin
  print,' Exposure map must be a 2-dimensional array. Check inputs. Returning.'
  retall
endif
nmapx = s(1)
nmapy = s(2)
;
if ( (nimgx ne nmapx) or (nimgy ne nmapy) ) then begin
  print,' Image has dimensions ',nimgx,nimgy
  print,' Exposure map has dimensions ',nmapx,nmapy
  print,' Dimensions of image and exposure map must be the same. Returning.'
  retall
endif
;
maskmap = expmap 
izero = where(maskmap le thresh*time) 
maskmap(izero) = 0
izero = 0
izero = where(maskmap gt thresh*time) 
maskmap(izero) = 1
dist_circle,arrtemp,512,255.5,255.5 
radlim = max(maskmap*arrtemp) 
if (!debug gt 3) then print,' Limiting radius = ',radlim
maskmap = 0
;
izero = 0
izero = where(arrtemp lt radlim) 
arrtemp(izero) = 1
izero = 0
izero = where(arrtemp ge radlim) 
arrtemp(izero) = 0
maskmap = arrtemp*expmap
mask = where( (arrtemp ne 0) and (maskmap le masklim*time) )
arrtemp = 0
arrtemp = maskmap*0
arrtemp(mask) = 1
mask = 0
mask = arrtemp + rot(arrtemp,-45.) + rot(arrtemp,45.)
;
arrtemp = 0
izero = 0
izero = where(maskmap le thresh*time) 
arrtemp = maskmap 
arrtemp(izero) = time/thresh
if (!debug gt 3) then print,' Array limits: ',minmax(time/arrtemp)
;
corrimg = image*0.
corrimg = image*time/arrtemp
;
izero = 0
izero = where(maskmap le masklim*time)
arrtemp = 0
arrtemp = maskmap*0
arrtemp(izero) = 1
maskmap = 0
maskmap = arrtemp + mask
izero = 0
izero = where(maskmap eq 0)
avflux = avg(corrimg(izero))
;
return
end           ;pro mexdiv
