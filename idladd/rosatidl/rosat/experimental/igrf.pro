PRO igrf,height,tiq,lati,longi,bn,be,bd
;+
; NAME:
;    IGRF
; PURPOSE:
;	Calculate the International Geomagnetic Reference Field for given
;	input conditions
; CALLING SEQUENCE:
;	IGRF,height,tiq,lat,long,bn,be,bd
; INPUTS:
;	height	- height above the Earth of the point in question in kilometers
;	tiq	- decimal year of time in question (e.g. 1991.2)
;	lati	- lattitude of point in question in decimal degrees
;	longi	- longitude of point in question in decimal degrees
; OUTPUTS:
;	bn	- B field strength in Gauss units along north vector
;	be	- B field strength along east vector
;	bd	- B field strength along radial vector toward center of Earth
; RESTRICTIONS:
;	Outputs will always be returned as vectors even if inputs were scalars.
;
; REFERENCES:	Wertz, James R., ed., "Spacecraft Attitude Determination and 
;		Control", D. Reidel Publishing Co., Holland, 1984.
;		"Spacecraft Magnetic Torques", NASA Space Vehicle Design
;		Criteria (guidance and Control), NASA SP-8018, March 1969.
;		Dessler,A.J., chapter on magnetic fields.
;		Smith, Robert E. and West, George S., compilers, " Space
;		and Planetary environment Criteria Guidelines for use in 
;		Space Vehicle Development, 1982 Revision (vol.1)" NASA
;		Technical Memorandum 82478, January 1983.
;
; HISTORY:
;	converted from fortran program by Don Neill (ACC) March, 1991.
;-
; check inputs
;
	on_error,1
	nel=[n_elements(height),n_elements(tiq),n_elements(lati), $
		n_elements(longi)]
	if max(nel) ne min(nel) then $
		message,'Input vectors must have same number of elements.'
	nel=nel(0)
	if nel eq 0 then $
		message,'Input vectors are empty.'
;
; declare arrays
	bn	=fltarr(nel)
	be	=fltarr(nel)
	bd	=fltarr(nel)
	k	=10
	g	=fltarr(k,k)
	h	=fltarr(k,k)
	s	=fltarr(k,k)
	 p	=fltarr(k,k)
	dp	=fltarr(k,k)
	cosmphi	=fltarr(k)
	sinmphi	=fltarr(k)
	k	=9
;
; constants
	ere	=6371.2	; Earth radius (km)
	pi	=3.1415
;
; spherical coef's
g0    =[[0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],$
	[0., 0., -30186., -1898., 1299., 951., -204., 46., 66., 11.],$
	[0., 0., -2036., 2997., -2144., 807., 368., 57., -57., 13.],$
	[0., 0., 0., 1551., 1296., 462., 275., 15., -7., 3.],$
	[0., 0., 0., 0., 805., -393., -20., -210., 7., -12.],$
	[0., 0., 0., 0., 0., 235., -161., -1., -22., -4.],$
	[0., 0., 0., 0., 0., 0., -38., -8., -9., 6.],$
	[0., 0., 0., 0., 0., 0., 0., -114., 11., -2.],$
	[0., 0., 0., 0., 0., 0., 0., 0., -8., 9.],$
	[0., 0., 0., 0., 0., 0., 0., 0., 0., 1.]]
;
gdot  =[[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
	[0.0, 0.0, 25.6, -24.9, -3.8, -0.2, 0.3, 0.2, 0.0, 0.2],$
	[0.0, 0.0, 10.0, 0.7, -10.4, -2.0, -0.7, 0.5, 0.0, 0.3],$
	[0.0, 0.0, 0.0, 4.3, -4.1, -3.9, 1.1, 2.0, 0.0, 0.0],$
	[0.0, 0.0, 0.0, 0.0, -4.2, -2.1, -1.6, 2.8, 0.6, 0.2],$
	[0.0, 0.0, 0.0, 0.0, 0.0, -3.1, -0.5, 0.0, 0.9, -0.4],$
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.9, 0.3, -0.3],$
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.1, 0.3, 0.6],$
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.5, -0.3],$
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.1]]
;
h0    =[[0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],$
	[0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],$
	[0., 0., 5735., -2124., -361., 148., 39., -23., -68., 4.],$
	[0., 0., 0., -37., 249., -264., 142., 102., -24., -15.],$
	[0., 0., 0., 0., -253., 37., -147., 88., -4., 2.],$
	[0., 0., 0., 0., 0., -307., -99., -43., 11., -19.],$
	[0., 0., 0., 0., 0., 0., 74., -9., 27., 1.],$
	[0., 0., 0., 0., 0., 0., 0., -4., -17., 18.],$
	[0., 0., 0., 0., 0., 0., 0., 0., -14., -6.],$
	[0., 0., 0., 0., 0., 0., 0., 0., 0., -19.]]
;
hdot  =[[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
	[0.0, 0.0, -10.2, -3.0, 6.9, 5.0, 1.2, -0.5, -1.4, -0.2],$
	[0.0, 0.0, 0.0, -18.9, 2.5, 0.8, 2.3, -0.1, -0.1, -0.4],$
	[0.0, 0.0, 0.0, 0.0, -5.0, 1.7, -2.0, -0.2, 0.3, -0.2],$
	[0.0, 0.0, 0.0, 0.0, 0.0, -1.0, 1.3, -1.3, 0.3, -0.3],$
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.1, 0.7, -0.7, 0.4],$
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.7, 0.1, -0.3],$
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.8, -0.6],$
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3]]
;
; get radius
	rt = [height + ere]
;
; years since 1975.0
	t1975 = [tiq - 1975.0]
;
; convert coords to radians
	lat = [lati * pi / 180.0]
	long= [longi* pi / 180.0]
;
; use recursion formula to calculate s(n,m)
      s(1,1) = -1.0
      for n = 2, k do begin
	s(n,1) = s(n-1,1) * (2.0*n-3.0)/(n-1.0)
	s(n,2) = s(n,1) * sqrt( (n-1.0)*2.0/n )
	for m = 3, n do s(n,m) = s(n,m-1) * sqrt( (n-m+1.0)/(n+m-2.0) )
      endfor
;
; calculate sines and cosines
	costheta = cos(-(lat) + (pi/2.0))
	sintheta = sin(-(lat) + (pi/2.0))
;
	cosphi = cos(long)
	sinphi = sin(long)
;
; loop over input vectors
	for ii=0,nel-1 do begin
;
; calculate field coefficients with secular corrections and Schmidt
; normalization
	g=g-g & h=h-h	; init arrays
      for n = 1, k do $
      for m = 1, n do begin
	g(n,m) = (g0(n,m) + t1975(ii)*gdot(n,m)) * s(n,m)
	h(n,m) = (h0(n,m) + t1975(ii)*hdot(n,m)) * s(n,m)
      endfor
;
; Use recursion formula to calculate cos(m*phi), sin(m*phi)
	cosmphi=cosmphi-cosmphi & sinmphi=sinmphi-sinmphi	; init arrays
	cosmphi(1) = 1.0
	sinmphi(1) = 0.0
      for m = 2, k do begin
	cosmphi(m) = cosmphi(m-1)*cosphi(ii) - sinmphi(m-1)*sinphi(ii)
	sinmphi(m) = sinmphi(m-1)*cosphi(ii) + cosmphi(m-1)*sinphi(ii)
      endfor
;
; Use recursion formula to calculate P(n,m) and derivative.
	p=p-p & dp=dp-dp	; init arrays
       p(1,1) = 1.0
      dp(1,1) = 0.0
      for n = 2, k do $
      for m = 1, n do begin
	if (n eq m) then begin
           p(n,n) = sintheta(ii) *  p(n-1,n-1)
          dp(n,n) = sintheta(ii) * dp(n-1,n-1) + costheta(ii)*p(n-1,n-1)
	endif else begin
          knm = ( (n-2.0)^2 - (m-1.0)^2 ) $
		/ ( (2.0*(n-1.0)-1.0) * (2.0*(n-1.0)-3.0) )
          if (n-1 eq 1) then knm = 0.0
           p(n,m) = costheta(ii) *  p(n-1,m) - knm*p(n-2,m)
          dp(n,m) = costheta(ii) * dp(n-1,m) - sintheta(ii)*p(n-1,m) - $
			knm*dp(n-2,m)
	endelse
      endfor
;
      br     = 0.0
      btheta = 0.0
      bphi   = 0.0
      for n = 2, k do begin
	sigma1 = 0.0
	sigma2 = 0.0
	sigma3 = 0.0
	for m = 1, n do begin
          sigma1 = sigma1 + (g(n,m)*cosmphi(m) + h(n,m)*sinmphi(m)) * p(n,m)
          sigma2 = sigma2 + (g(n,m)*cosmphi(m) + h(n,m)*sinmphi(m)) * dp(n,m)
          sigma3 = sigma3+(m-1.0)*(-g(n,m)*sinmphi(m)+h(n,m)*cosmphi(m))*p(n,m)
	endfor
;
	br     = br     - (ere/rt(ii))^(n+1) * sigma1 * n 
	btheta = btheta + (ere/rt(ii))^(n+1) * sigma2
	if (sintheta(ii) ne 0) then $
	bphi   = bphi   + (1.0/sintheta(ii)) * (ere/rt(ii))^(n+1) * sigma3
;
      endfor
;
; Geocentric Inertial Coordinates
;	bn(ii) = (br*cos(lat(ii))+btheta*sin(lat(ii)))*cos(long(ii)) - $
;		bphi*sin(long(ii))
;	be(ii) = (br*cos(lat(ii))+btheta*sin(lat(ii)))*sin(long(ii)) + $
;		bphi*cos(long(ii))
;	bd(ii) = (br*sin(lat(ii))-btheta*cos(lat(ii)))
;
; nano Tesla to Gauss
	bn(ii) = -btheta * 1e-05
	be(ii) = bphi * 1e-05
	bd(ii) = -br * 1e-05
;
; end loop over input vectors (next ii)
	endfor
;
	return
	end ; IGRF
