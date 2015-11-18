pro phoctread,isrc,r,nh,phot,iflx
;Subroutine for gal.pro.  This program reads in the information from
;data files created by the shell script phoctrp.sh.  It then inter-
;polates given the inputted column density the amount of attenuation
;that the flux in the iflx vector should receive after passing through
;the interstellar medium of the given column density.
;  isrc = Constant flux from each source.
;  iflx = Output vector containing flux of each source.
;  r    = Vector containing distances to each source.
;  nh   = Volume density of interstellar medium.
;  phot = Photon bin number (decides whether to use data from file
;         3_5 (phot = 0), 13_18 (phot = 1), or 19_30 (phot = 2)).
;The data in the files are the column densities (first column) followed
;by the expected count rate (second column).  Proper normalization can
;be had by dividing by the count rate at 0. column density (i.e., the
;first line of data in each file).

;nel  = 66                   ;Size of N_H vector in data files.
nel  = 94                   ;Size of N_H vector in data files.
nhar = dblarr(nel)          ;Arrays for filling with N_H vector and count
crar = dblarr(nel)          ;rates from data files.
file = ['phoct_dat.rp.3_5','phoct_dat.rp.13_18','phoct_dat.rp.19_30']

;** First, read in data from the called data file:
print,'Loading data ...'
txt = ''
print,''
openr,99,file(phot)
for i = 0,6 do begin
  readf,99,txt
endfor
readf,99,format = '(a13,f15.8)',txt,normal
norm = normal
for i = 0,4 do begin
  readf,99,txt
endfor
print,''
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
rcol = r*nh/1.d22
linterp,nhar,crar,rcol,iflx
iflx = iflx*isrc

return
end
