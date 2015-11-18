pro ascabatch,rootname,mode,winst=winst,seqfile=seqfile,dirfile=dirfile,sisrad=sisrad,gisrad=gisrad,tbin=tbin,fext=fext,mkflist=mkflist,tmfile=tmfile,sisfile=sisfile,grdstr=grdstr,tcor=tcor
if n_params(0) eq 0 then begin
print,'ascabatch,rootname,mode,winst=winst,seqfile=seqfile,dirfile=dirfile,sisrad=sisrad,gisrad=gisrad,tbin=tbin,fext=fext,mkflist=mkflist,tmfile=tmfile,sisfile=sisfile,grdstr=grdstr,tcor=tcor'
 print,'Analyse A whole bunch of ASCA observations automatically'
 print,'ROOTNAME - ALL output files will be named '
 print,'	  ROOTNAME_SEQNO_INSRUMENT_* '
 print,'MODE 	- If mode eq 1 do everything '
 print,'          If mode eq 0 do not run sispi, sisrmg, ascalin'
 print,'WINST	- 4 element integer array [s0,s1,s2,s3] '
 print,'	- a 0 means don t do that instrument '
 print,'SEQFILE - An ascii file containing a list of the sequence
 print,'	  numbers to process '
 print,'DIRFILE - If the last character of DIRFILE is a / then this '
 print,'	  string is the path to where all the data is. '
 print,'	  Otherwise the string is taken as a file name
 print,'          containing a dirrectory path for each sequence. '
 print,'SISRAD  - Array(3) Radii in arcmin of SIS extraction regions'
 print,'          sisrad(0) - source extraction radius '
 print,'          sisrad(1) - inner radius of bgd extraction '
 print,'          sisrad(2) - outer radius of bgd extraction '
 print,'	  If sisrad(2)= -1 then the whole chip outside'
 print,' 	  sisrad(1) is used for bgd. If sisrad(2)=0. then '
 print,'	  this means the background is taken fron a separate '
 print,'	  circle not centered on the source and the center for'
 print,'	  each sequence must be read from a file. '
 print,'GISRAD  - Array(3) Radii in arcmin of GIS extraction regions'
 print,'	  Meaning exactly analagous to SISRAD '
 print,'	- this separate circular region: array(3): Xc,Yc,r1'
 print,'TBIN	- Bin size for lightcurves. Default 512s if omitted ' 
 print,'FEXT	- Extension of events files to look for. Defualt .evt'
 print,'MKFLIST - Name of ascii file containing list of MKF selection'
 print,'	  expressions'
 print,'TMFILE  - Optional time intervals ascii file for filtering '
 print,'SISFILE - Full path to SIS pha->pi file'
 print,'	**OUTPUTS** '
 print,'	  Are dumped in the current directory. There will be '
 print,'	  spectra + lightcurves (no bgd subtraction) '
 print,'	  and SIS rmf files.'
 print,'GRDSTR	- grades string for sisrmg, defualt is 0234'
 print,'TCOR    - Apply GIS temporal gain correction if >0 (default 1)'
 print,'  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
 print,' The quickest way to call this routine is '
 print,'ASCABATCH,rootname,mode,seqfile=seqfile,dirfile=dirfile,fext=fext,mkflist=mkflist,winst=winst,sisfile=sisfile'
 print,'This call has the following defaults: '
 print,'1. Process all instruments '
 print,'2. Use time binsize of 512 s '
 print,'3. Look for .evt (screened) data files '
 print,'4. For SIS extract a 4 arcmin circle centered on source '
 print,'   centroid. For background, draw a circle with radius 5.5 '
 print,'   arcmin and use the entire chip excluding this circle '
 print,'5. For GIS extract a 6 arcmin radius circle centered on the '
 print,'   source centroid. For background use an annulus of radii '
 print,'   8 and 12 arcmin  centered on the source. '
 print,'6. The default MKF selection expressions are equivalent to the'
 print,'   the MKFLIST file having the following contents:'
 print,'	200
 print,' 	saa.eq.0'
 print,'	elv.gt.5'
 print,'        cor.gt.7'
 print,'	fov.gt.0'	
 print,'   Note the SISCLEAN is always used on SIS data''
 retall
endif
if n_elements(tcor) eq 0 then tcor =1
if n_elements(grdstr) eq 0 then begin
 grdstr='0234'
endif
;first set up defaults for sisrad and gisrad if necessary 
if n_elements(sisrad) eq 0 then begin
 sisrad=[4.,5.5,-1.] 
endif 
if sisrad(2) gt -1. then begin
 print,'** This type of SIS region not yet supported - ABORTING ** '
 return
end
if n_elements(gisrad) eq 0 then begin
 gisrad=[6.,8.,12.] 
endif
if gisrad(2) le 0. then begin
 print,'** This type of GIS region not yet supported - ABORTING ** '
 return
end
if n_elements(seqfile) eq 0 then begin
 seqfile=' '
 read,'Enter name of ascii file containing the list of sequence Nos.',seqfile
endif
openr,1,seqfile
seqname=strarr(1000l)
dirc=seqname
dummy=' '
iseq=0l
while not eof(1) do begin
 readf,1,dummy & seqname(iseq)=dummy
 iseq=iseq+1l
endwhile
close,1
if n_elements(dirfile) eq 0 then begin
 dirfile=' '
read,'Enter dir path  or name of ascii file containing list of paths',$
dirfile
endif
if strmid(dirfile,strlen(dirfile)-1,1) eq '/' then begin
 dirc(0:iseq-1l)=dirfile 
 idir=iseq
endif else begin
 openr,1,dirfile 
 idir=0l 
 while not eof(1) do begin
  readf,1,dummy & dirc(idir)=dummy
  idir=idir+1l
 endwhile
 close,1
endelse
if iseq ne idir then begin
 print,'**ABORTING ** # of paths does not match # of sequences '
 return
end
;set up parameters for automatic running of QLASCA
;      
;parameter for box-car smoothing of image before centroid is found
cens=4.
;following specifies grades 0,2,3,4 for SIS 
;gradeflt='grade.ne.1.and.grade.le.4'
;following is lightcurve bin size in seconds
if n_elements(tbin) eq 0 then tbin=512.
;defualt event file extension
if n_elements(fext) eq 0 then fext='evt'
if fext eq 'fits' then instid=['S0','S1','G2','G3'] else $
 instid=['s0','s1','g2','g3']
srche=fext
;following default uses all four instruments
if n_elements(winst) eq 0 then winst=[1,1,1,1] 
for j=0l,iseq-1l do begin
 print,'Processing Sequence ',seqname(j)
 srcho=seqname(j)
 dir=dirc(j)
 for k=0,3 do begin 
  if winst(k) gt 0 then begin
   print,'Doing ',instid(k)
   srchi=instid(k)
    if k le 1 then begin
     srcreg=[470.,470.,sisrad(0),sisrad(1),sisrad(2)]
    endif else begin
     srcreg=[108.,114.,gisrad(0),gisrad(1),gisrad(2)]
    endelse
    qlasca,srcho,srchi,srche,k,rootname=rootname,/cmpcen,cens=cens, $
    srcreg=srcreg,bgdreg=bgdreg,dir=dir,/arf,rmf=mode, $
    sispi=mode, tbin=tbin,mkflist=mkflist,tmfile=tmfile,sisfile=sisfile,grdstr=grdstr,tcor=tcor
  endif
 endfor
endfor
return
end
