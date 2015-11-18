pro rtau,tau,phot,nh,rmax
;** Subroutine for gal.pro.  This takes in as input:
;**  tau  == Maximum optical depth to integrate to;
;**  phot == Photon bin number; for deciding whether to use XSPEC
;**          channels 3-5 (phot=0), 13-18 (phot=1), or 19-30 (phot=2);
;**  nh   == Volume density of space in cm^-3.
;** This subroutine returns:
;**  rmax == Maximum distance to integrate to.
;** Absorption data is read in from one of three files depending on
;** the value of phot.  The absorption due to tau is interpolated, and
;** with the volume density of space nh, the maximum distance rmax is
;** returned.  The read and interpolation statements are directly from
;** phoctread.pro.
;** The data in the files are the column densities (first column) followed
;** by the expected count rate (second column).  Proper normalization can
;** be had by dividing by the count rate at 0. column density (i.e., the
;** first line of data in each file).
;** Written by kachun 17 June 1993.
nel  = 94                   ;Size of N_H vector in data files.
nhar = dblarr(nel)          ;Arrays for filling with N_H vector and count
crar = dblarr(nel)          ;rates from data files.
file = ['phoct_dat.rp.3_5','phoct_dat.rp.13_18','phoct_dat.rp.19_30']

;** First, read in data from the called data file:
print,'Loading data ...'
txt = ''
openr,99,file(phot)
for i = 0,6 do readf,99,txt
readf,99,format = '(a13,f15.8)',txt,normal
norm = normal
for i = 0,4 do readf,99,txt
for i = 0,nel-1 do begin
  readf,99,nhj,crj
  crar(i)=crj
  nhar(i)=nhj
endfor
close,99
 
;** Use normalization constants taken from file to renormalize count
;** arrays so that the largest number of counts is set to 1.00.
crar = crar/norm
crar = crar/max(crar)

;** Create column density vector and use linterp routine to find
;** interpolated values of attenuation from data read-in above:
absp = exp(-tau)
linterp,crar,nhar,absp,cden
rmax = cden*1.d22/nh
return
end
