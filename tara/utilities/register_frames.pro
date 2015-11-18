;;; $Id: register_frames.pro 1148 2000-07-10 11:57:08Z patb $
;;; This program derives the relationship between two coordinate systems
;;; given the coordinates of a set of fiducial points.  
;;; The best-fit coordinate transformation is constrained to be a
;;; translation-plus-rotation.
;;; 
;;; USAGE:
;;; register_frames, x_ref, y_ref, x_skew, y_skew, mask, T
;;;
;;; (x_ref,y_ref) are the fiducial points in the reference coordinate system.
;;;
;;; (x_skew,y_skew) are the fiducial points in the "skew" system, the one
;;; whose data we want to adjust to match the reference system
;;;
;;; mask is a boolean vector indicating which of the fiducial points we wish
;;; to use in deriving the transformation.  The idea is to iteratively call
;;; register_frames, using the residual plot to toss out fiducial pairs that
;;; are outliers.  
;;; For conveneience, if mask is undefined when passed (e.g. on the first
;;; iteration) a vector of 1's will be generated and returned.
;;;
;;; T is the computed transformation, arranged such that
;;;   [x_ref,y_ref,1] = T # [x_skew, y_skew, 1]
;;; which means that
;;; T[0,*] = [cos(theta), -sin(theta), delta_x]
;;; T[1,*] = [sin(theta),  cos(theta), delta_y]
;;; T[2,*] = [         0,           0,       1]
;;; In IDL syntax one can construct T as
;;; T = [[ cos(theta),sin(theta),0],
;;;      [-sin(theta),cos(theta),0],
;;;      [    delta_x,   delta_y,1]]

;;; For catalogs stored as FITS binary tables this would be an example call:
;;;  c1 = mrdfits('catalog1', 1)
;;;  c2 = mrdfits('catalog2', 1)
;;;  register_frames, c1.x, c1.y, c2.x, c2.y, mask



FUNCTION S_stat, P

COMMON S_stat, xr, yr, xs, ys, x_residual, y_residual, square_error

delx = P[0] & dely = P[1] & theta = P[2]

x = (xs * cos(theta)) - (ys * sin(theta)) + delx
y = (xs * sin(theta)) + (ys * cos(theta)) + dely

x_residual = (xr-x)
y_residual = (yr-y)

square_error = x_residual^2 + y_residual^2
return, total(square_error, /DOUBLE)
end



PRO register_frames, x_ref, y_ref, x_skew, y_skew, mask, T

COMMON S_stat, xr, yr, xs, ys, x_residual, y_residual, square_error

;; If mask omitted, generate one.
if (n_elements(mask) EQ 0) then mask = replicate(1,n_elements(x_ref))

;; Make sure all the vectors have the same length.
len = [n_elements(x_ref),n_elements(y_ref),$
       n_elements(x_skew),n_elements(y_skew),n_elements(mask)]
N = min( len, MAX=maxN )
if (N NE maxN) then message, 'vectors must all have the same length'

;; Apply the mask.
index = where(mask)
xr = x_ref [index]
yr = y_ref [index]
xs = x_skew[index]
ys = y_skew[index]


;; Compute transformation:
P = amoeba(1E-5, FUNCTION_NAM='S_stat', P0=[0.,0.,0.], SCALE=[0.2, 0.2, 0.005])
if (n_elements(P) EQ 1) then message, 'Fit failed to converge'
delx = P[0] & dely = P[1] & theta = P[2]

print, 'THETA=', theta
print, cos(theta), -sin(theta), delx, sin(theta), cos(theta), dely
print, 'STDEV (PIXELS): ', S_stat(P)/(N-1)

function_1d, id1, index, x_residual, LI=6,PSYM=2, DATA='X'
function_1d, id1, index, y_residual, LI=6,PSYM=4, DATA='Y'
function_1d, id1, index, sqrt(square_error), LI=6,PSYM=5, DATA='distance', $
	     XTIT='fiducial point number', YTIT='fit residual'

dataset_2d, id2, xr, yr, WEIGHT=sqrt(square_error), $
	    TIT='Map of Fit Residuals'
return
end


PRO test_register_frames, x_ref, y_ref, x_skew, y_skew

theta   = 2*!PI/360. ;(1 degree)
delta_x = 0.5
delta_y = 0.5
N = 10

x_ref = 10*random(N,/UNIF)
y_ref = 10*random(N,/UNIF)

x_skew = cos(theta)*x_ref - sin(theta)*y_ref + delta_x + 0.01*random(N)
y_skew = sin(theta)*x_ref + cos(theta)*y_ref + delta_y + 0.01*random(N)
return
end
