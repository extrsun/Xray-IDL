pro gal,nden=nden,rmin=rmin,rmax=rmax,isrc=isrc,nh=nh, $
  time=time,phor=phor,frho=frho,effm=effm
;
;Calculate flux from sources distributed in a volume in the galactic plane.
;This program can be used to simulate an image of the galactic plane in
;the soft x-ray band taken by for example, the ROSAT PSPC detector.
;Volume will be defined in spherical coordinates, with field of view area
;darea that is circular (defined with coordinates \theta and \rho), and
;minimum and maximum in the radial coordinate defined by rmin and rmax (or
;by optical depth \tau1 and \tau2).
;
;Size of volume in spherical coordinates in inputted, as well as volume
;density of sources nden.  A poisson random number generator is used to
;the actual number of sources in a particular simulation.  A random de-
;viate from the radial distribution of sources is used to randomly de-
;termine the distance of each source from the observer.  (The position
;of each source in the field is also randomly determined, albeit in a
;far simpler way.)
;
;In this simple program, the flux from each source is assumed to be
;constant, isrc.  Given an average absorption cross section of Hydrogen
;and averaged volume density of absorbers in space, the flux from each
;source is attenuated as a function of distance to the observer.  This
;simulated image of sources and flux is then multiplied by an effective
;exposure map (containing all instrumental corrections, etc.), and then
;the flux is converted to photon counts.  The poisson generator is used
;again to randomly determine the actual number of photons that the de-
;tector sees.
;
;INPUTS:
;  nden == Number volume density of sources in parsec^-3.
;  rmin == Minimum distance of volume to observer.
;  rmax == Maximum distance of volume to observer.
;  isrc == Constant luminosity from each source [(erg or ct)/sec].
;  nh   == Volume density of interstellar medium in units of 10^22.
;  time == Exposure time of detector image.
;  phor == Photon to energy (erg) ratio.
;  frho == Radius of field of view in degrees.
;  effm == Effective exposure map of detector.
;
;Written by kachun May 8, 1993.
;
;if n_elements(nden) eq 0 then $
;  read,'Input volume density of sources [pc^-3]: ',nden
nden = .1
;if n_elements(rmin) eq 0 then $
;  read,'Input minimum distance to volume of sources [pc]: ',rmin
rmin = 0.
;if n_elements(rmax) eq 0 then $
;  read,'Input minimum distance to volume of sources [pc]: ',rmax
rmax = 3000.
;if n_elements(isrc) eq 0 then $
isrc = 1.e28
;if n_elements(nh) eq 0 then $
;nh = 1.
nh = 0.3
;if n_elements(phot) eq 0 then $
phot = 0
;if n_elements(time) eq 0 then $
time = 1.d4
;if n_elements(phor) eq 0 then $
phor = (1.2d-11)^(-1)
;if n_elements(effm) eq 0 then $
effm = dblarr(480,480)
;if n_elements(frho) eq 0 then $
frho = .25
;** Calculate area of field of view in steradians:
darea = double( (!pi*frho^2)*(!pi/180.)^2 )
;** Convert distances and density from parsecs to cm:
rmin = double(3.0856d18*rmin)
rmax = double(3.0856d18*rmax)
nden = double(nden/(3.0856d18)^3)

;** Find number of sources in volume (and also the normalization for
;the random deviate from distribution).
nexp = double( nden*darea*(rmax^3-rmin^3)/3. )

;** Use poisson generator or the IDL normal random number generator
;** to randomly determine actual number of sources given the mean number
;** of sources nexp:
if nexp le 100 then begin
  poisson,nexp,nsrc,seed
endif else begin
  rand = max(randomn(seed,1))
  nsrc = rand * sqrt(nexp) + nexp
endelse
nsrc = nint(nsrc,/long)

;** Randomly determine distance to each source:
r = (rmin^3 + randomu(seed,nsrc)*(rmax^3-rmin^3))^(1./3.)
stop

;** Randomly determine location of each source within image field
;** with coordinates in polar coordinates:
;rndr = double(randomu(seed,nsrc)*frho)
;rnda = double(randomu(seed,nsrc)*360.)
;x = rndr*cos(rnda)
;y = rndr*sin(rnda)

;** Assuming constant flux isrc from each source, calculate attenuation
;** due to absorption by interstellar medium.  The data files created
;** from the shell script phoctrp.sh are read in and the information is
;** used in interpolating an attenuation given a particular value for
;** the column density.  (Note that other data files can be substituted
;** for the ones created by phoctrp.sh.)
phoctread,isrc,r,nh,phot,iflx

;** Multiply by effective exposure map of the detector:
;effmap,iflx,x,y,map

;** Multiply by exposure time and by photons/erg ratio, and divide by
;** distance squared:
iflx = iflx*time*phor/(4.*!pi*r*r)
stop

;** Use poisson random number generator to calculate number of photons
;** seen at detector from each source:
poisson,iflx,iphot,seed

;** Renormalize iphot to flux in ergs (instead of photon counts) and
;** calculate sum of flux and sum of the flux squared:
iphot = iphot/phor
ipho2 = iphot^2
sumi1 = total(iphot)
sumi2 = total(ipho2)
print,'Sum of flux = ',sumi1
print,'Sum of flux squared = ',sumi2

stop
end
