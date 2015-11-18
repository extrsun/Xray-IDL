pro xstats,pfix,pnames,statnames,parfnames,outname=outname,strue,smin,bfits,delstat
if n_params(0) eq 0 then begin
 print,' xstats,pfix,pnames,statnames,parfnames,outname=outname,strue,smin,bfits,delstat'
 print,' Calcluate statistics from muliple runs of XSPEC. It is assumed that the XSPEC '
 print,' log file has been read by the program RDXLOG which produces two output results '
 print,' files, *.stat and *.pars . '
 print,' **INPUTS** '
 print,' PNAMES	- a string array you create containing the model parameter names '
 print,' 		- it MUST have the same number of elements as there were model pars '
 print,' PFIX	- a vector (size=no. of params) containing non-zero values for the ith '
 print,'		- element only if the ith parameter was free in the spectral fitting '
 print,' STATNAMES   - a string array containing the names of the *.stat files you want to '
 print,' 		- include in the analysis. Size must be exactly equal to the number of files '
 print,' PARFNAMES  	- a string array containing the names of the *.pars files you want to '
 print,'		- include in the analsysis. Size must be exactly equal to the number of files '
 print,' **OUTPUT** '
 print,' OUTNAME 	- rootname of output file to write analysis results to. '
 print,' results written to rootname.xstats & delt-stat distribution to rootname.qdp '
 retall
endif
if n_elements(outname) eq 0 then begin
 outname='  '
 read,' Enter the desired rootname of the output results file ',outname
endif
openw,1,outname+'.xstats'
npars=n_elements(pnames) & nsfiles=n_elements(statnames) & npfiles=n_elements(parfnames)
;first read in the statistics [*.stat] files
nslines=0l
for i=0,nsfiles-1 do begin
 readcol,statnames(i),s1,n1,s2,n2
 if i eq 0 then begin
	stat1=s1  & npha1=n1 & stat2=s2 & npha2=n2
 endif
 if i gt 0 then begin
  stat1=[stat1,s1] & npha1=[npha1,n1] & stat2=[stat2,s2] & npha2=[npha2,n2]
 endif
endfor
nslines=(size(stat1))(1)
print,' Read a total of ',nslines,' from *.stat file '
ipick=1+2*indgen((nslines/2))
strue=stat1 & smin=stat2 & delstat=strue-smin
nfree=(size(where(pfix gt 0)))(1) 
printf,1,' The names of the *.stat and *.pars files used were :'
for kk=0,nsfiles-1 do begin
 printf,1,statnames(0:nsfiles-1),'   ',parfnames(0:npfiles-1)
endfor
printf,1,' There were ',nfree,' free parameters in the spectral fitting '
nsims=(size(strue))(1) 
printf,1,' The number of simulations was ',nsims
dof_true=npha2(nslines-1) & dof_min=dof_true-nfree 
printf,1,' Degrees of freedom for STRUE, SMIN and STRUE-SMIN : '
printf,1, dof_true, dof_min, nfree
strue_lo=min(strue) & strue_hi=max(strue) & smin_lo=min(smin) & smin_hi=max(smin)
delstat_lo=min(delstat) & delstat_hi=max(delstat)
printf,1,' min & max values of STRUE: ',strue_lo,strue_hi
printf,1,' min & max values of SMIN: ',smin_lo,smin_hi
printf,1,' min & max values of STRUE-SMIN: ',delstat_lo, delstat_hi
print,' min & max values of STRUE-SMIN: ',delstat_lo, delstat_hi
printf,1,' Predicted mean STRUE-SMIN: ',nfree
calcmnsd,delstat,delstat_mn,delstat_sd
printf,1,' Observed mean STRUE-SMIN: ',delstat_mn,' +/- ',delstat_sd
psd_delstat=sqrt(2.*float(nfree))
printf,1,' Predicted S.D. of STRUE-SMIN: ',psd_delstat
osd_delstat=total(sqrt(2.*delstat))/float(nsims)
osd_delstat_err=0.5*sqrt(2./(float(nsims-1)))
printf,1,' Observed S.D. of STRUE-SMIN: ',osd_delstat,' +/- ',osd_delstat_err
ndelc=4 & delc=fltarr(ndelc) & fdelc=delc & frac=delc & fracerr=delc
fdelc=[0.05,0.10,0.32,0.80]
for i=0,ndelc-1 do begin
 delc(i) = chi_sqr(fdelc(i),nfree)
 printf,1,' Predicted fraction with STRUE-SMIN < ',delc(i),' = ',1.0-fdelc(i)
 frac(i)=float((size(where(delstat lt delc(i))))(1))/float(nsims)
 if frac(i) gt 0.0 then fracerr(i)=frac(i)*sqrt((frac(i)*(1.0-frac(i)))/float(nsims))
 printf,1,'Observed fraction with STRUE-SMIN < above val: ',frac(i),' +/- ',fracerr(i)
endfor
nbfits=0l
bfits=fltarr(100000,npars) & tempbfits=fltarr(npars) & p0=tempbfits 
pmean=p0 & psdev=p0
for i=0l,npfiles-1 do begin
 openr,2,parfnames(i)
 while not eof(2) do begin
	readf,2,p0
	readf,2,tempbfits
	bfits(nbfits,0:npars-1)=tempbfits
	nbfits=nbfits+1l
 endwhile
 close,2
endfor
printf,1,'  '
printf,1,' Read in ',nbfits,' best-fitting sets of parameters '
printf,1,' Input model parametrs were :'
for k=0l,npars-1 do printf,1,pnames(k),' = ',p0(k)
printf,1, '  '
printf,1,' Observed values of the best-fitting parameters: '
printf,1,'  '
for k=0l,npars-1 do begin
 tempvec=bfits(0:nbfits-1,k:k)
 calcmnsd, bfits(0:nbfits-1,k:k),a,b & pmean(k)=a & psdev(k)=b
 printf,1,pnames(k),' = ',pmean(k),' +/- ',psdev(k)
endfor
ncval=21 & cval=fltarr(ncval) & cumchi=cval & cumerr=cval
cval=min(delstat)+findgen(ncval)*(max(delstat)-min(delstat))/float(ncval-1)
for k=0,ncval-1 do begin
 cumchi(k)=float((size(where(delstat le cval(k))))(1))
 if cumchi(k) gt 0.0 then cumerr(k)=sqrt(cumchi(k)) else cumerr(k) =0.0
endfor
cumchi=cumchi/float(nsims) & cumerr=cumerr/float(nsims)
npts = 500l & cmin=delstat_lo*0.9 & cmax=delstat_hi*1.1
;plotchi,nfree,npts,cmin,cmax,xchi,dist & nds=(size(xchi))(1)
plotchi,nfree,npts,0.0,cmax,xchi,dist & nds=(size(xchi))(1)
oplot,cval,cumchi,psym=1
close,1
;write delta-statistic distribution to QDP file
openw,3,outname+'.qdp'
printf,3,'SKIP SINGLE'
printf,3,'MARKER 17 ON 1'
printf,3,'LAB X delta-statistic '
printf,3,'LAB Y cumulative probability of obtaining delta-statistic '
printf,3,'LA OT comparsion of delta-stat with chi-square dist; dof= ',nfree
for i=0l,nfree-1 do printf,3,'LINE ON ',i+2
dumerr=0.0
for i=0l,ncval-1 do printf,3,cval(i),cumchi(i)
printf,3,'NO 	NO'
for j=0l,nfree-1 do begin
 for i=0l,nds-1 do begin
  printf,3,xchi(i),dist(i,j)
 endfor
printf,3,'NO	NO'
endfor
close,3
return
end
