pro optaxsgrd,seqfile,inst,e1,e2,xb,yb,map=map,fudge=fudge,grd=grd,seqlog=seqlog,ctslog=ctslog,nx=nx,ny=ny,x1=x1,x2=x2,y1=y1,y2=y2,tmpgrd=tmpgrd
if n_params(0) eq 0 then begin
 print,'MEASURE OPTICAL AXES OF ASCA TELESCOPES'
 print,'This routine takes the ARF files made by ARFGRID for a'
 print,'particular instrument and set of observation sequences given'
 print,'in the ascii file seqfile to make a 2-D grid whose X and Y '
 print,'dimensions correspond to the telescope optical axes positions.'
 print,'Pixels in the grid consist of the ABSOLUTE VALUE of   '
 print,'PRODUCT (i=2,NSEQ) { cts/s(i)* sum [ARF efficiency (i)] -
 print,'                    cts/s(1) *sum[ARF efficiency (1)]}'
 print,'where NSEQ is the number of sequences (pointings) and cts/s(i)'
 print,'is the count rate for sequence (i) over the energy range E1-E2'
 print,'and sum [ARF efficiency (i)] is the integrated telescope '
 print,'efficiency over the same energy range (i.e. SPECRESP).'  
 print,'If the grid (GRD) already exists, GRD is modified in a'
 print,'cumulative way or else a new grid is created. Thus the pixel '
 print,'in GRD which has the smallest value corresponds to the best '
 print,'estimate of the optical axis. Adding more and more sequences '
 print,'will thus produce a progressively better estimate of the'
 print,'optical axis. The sequences for each run of OPTAXSGRD can '
 print,'can correspond to two or more pointings at  a CONSTANT'
 print,'source OR a source which does not vary during the period of the'
 print,'different pointings.'
 print,'*INPUTS* '
 print,'SEQFILE	- name of ascii file which contains list of 2 or more '
 print,'	  sequences to process'
 print,'	  NOTE if your source is NOT very constant then '
 print,'	  your sequence list MUST BE IN CHRONOLOGICAL order'
 print,'INST	- 0,1,2,3 for s0,s1,g2,g3 respectively'
 print,'E1,E2	- minimum and maximum energies within which to perform'
 print,'	  calculations.'
 print,'NX,NY	    - Dimensions and
 print,'X1,X2,Y1,Y2 - lower and upper DETX and DETY boundaries for '
 print,'	      optical axis grid (GRD) MUST be the same as was '
 print,'	      used in ARFGRID. Non-specification invokes'
 print,'FUDGE	  An array of fudge factors (dimensions=# seq) to '
 print,'	  investigate the effect of systematic error and '
 print,'	  of a non-constant source. If not specified the '
 print,'	  default is fudge(*)=1.0. The factors directly '
 print,'	  multiply the count-rates'
 print,'	      default values. All these are array(4)'
 print,'MAP	  array(ngrd,2) where NGRD = number of pairs of '
 print,'	  sequences to use and this pair of sequences is '
 print,'	  specified by map(i,0) and map(i,1) where map has the '
 print,'	  value equal position in seqfile, starting from 0'
 print,'	  If map is not specified then NGRD=number of seq. in '
 print,'	  seqfile and map(0,0)=0, map(0,1)=1, map(1,0)=1, '
 print,'	  map(1,1)=2 etc; i.e. map(i,0)=i and map(i,1)=i+1'
 print,'INPUT/OUPTUT:	- GRD(NX(inst),NY(inst))'
 print,'XB(nx(inst),2) & YB(ny(inst),2) are lower and upper boundaries'
 print,'	 of the X and Y values of the grid.'
 print,' ACTION: '
 print,'	1. For given instrument s0 s1 g2 g3 (=detid)
 print,'	 search for spectral files of type *seq#*detid*src*.p*'
 print,'	2. For spectral file, run through a grid of'
 print,'	 optical axis values with Ox ranging from x1 to x2 in '
 print,'	 nx steps and values of Oy ranging from y1 to y2 in ny'
 print,'	 steps using ARF files created by ARFGRID.'
 print,'	x and y (array(4)) are in DET coords. Defaults are: '
 print,' '
 print,'			x1	x2	y1 	y2
 print,'	SIS0		625	700	521	596
 print,'	SIS1		580	656	696     772
 print,'	GIS2		128	138	126	136
 print,'	GIS3		115 	125	129	139	
 print,'
 print,' 	DEFAULTS for NX: [8,8,10,10]'
 print,'		     NY: [8,8,10,10]'
 print,' All files are searched for in the current directory '
 print,'optaxsgrd,seqfile,inst,e1,e2,xb,yb,map=map,fudge=fudge,grd=grd,seqlog=seqlog,ctslog=ctslog,nx=nx,ny=ny,x1=x1,x2=x2,y1=y1,y2=y2,tmpgrd=tmpgrd'
; print,'RMFNAMES(4) is a string array '
; print,'The defualt RMF filenames are:'
; print,'/FTP/caldb/data/asca/sis/cpf/94nov9/s0c1g0234p40e1_512v0_8i.rmf'
; print,'/FTP/caldb/data/asca/sis/cpf/94nov9/s1c3g0234p40e1_512v0_8i.rmf'
; print,'/FTP/caldb/data/asca/gis/cpf/95mar06/gis2v4_0.rmf'
; print,'/FTP/caldb/data/asca/gis/cpf/95mar06/gis3v4_0.rmf'
; print,' '
; print,'ARFNAMES(nx,ny,nseq,4) is an output grid of arf filenames '
 retall
end
;first set up input defaults
if n_elements(x1) eq 0 then x1=[625.,580.,128.,115.]
if n_elements(x2) eq 0 then x2=[700.,656.,138,125.]
if n_elements(y1) eq 0 then y1=[521.,696.,126.,129.]
if n_elements(y2) eq 0 then y2=[596.,772.,136.,139.]
if n_elements(nx) eq 0 then nx=[8,8,10,10]
if n_elements(ny) eq 0 then ny=[8,8,10,10]
if n_elements(grd) eq 0 then grd=1.+fltarr(nx(inst),ny(inst))
instid=['s0','s1','g2','g3']
tmpgrd1=grd & tmpgrd2=grd
;set up the x and y axis values
xb=fltarr(nx(inst),2) & yb=fltarr(ny(inst),2)
dx=(x2(inst)-x1(inst))/float(nx(inst)) 
dy=(y2(inst)-y1(inst))/float(ny(inst))
xb(*,0)=x1(inst)+dx*findgen(nx(inst)) & xb(*,1)=xb(*,0)+dx
yb(*,0)=y1(inst)+dy*findgen(ny(inst)) & yb(*,1)=yb(*,0)+dy
;step through each sequence
openr,1,seqfile
openw,3,'cts.log'
iseq=0l
dummy=' '
seqname=strarr(10000l)
while not eof(1) do begin
 readf,1,dummy
 seqname(iseq)=dummy 
 iseq=iseq+1l
endwhile
close,1
if n_elements(fudge) eq 0 then fudge=1.+fltarr(iseq)
if n_elements(map) eq 0 then begin
 map=lonarr(iseq-1l,2)
 map(0:iseq-2l,0)=lindgen(iseq-1l)
 map(0:iseq-2l,1)=lindgen(iseq-1l)+1l
endif
ngrd=(size(map))(1)
tmpgrd=fltarr(iseq,nx(inst),ny(inst))
ctspsec=fltarr(iseq)
dot=strpos(seqfile,'.')
rootname=strmid(seqfile,0,dot)
;for each instrument and sequence find the spectral file
  print,'Doing ',instid(inst)
  for i=0l,iseq-1l do begin
   seqname(i)=strtrim(seqname(i),2)
   print,'Doing sequence # ',i,seqname(i)
  specfiles=findfile('*'+seqname(i)+'*'+instid(inst)+'*src*.p*',count=nspec)
 if nspec gt 1 then begin
  print,'More than 1 spectral file found: taking 1st one'
 endif
  specname=specfiles(0)
  print,'Spectral file : ',specname
;now get the corresponding background file
  specsep=str_sep(specname,'src')
  bgdname=specsep(0)+'bgd'+specsep(1)
  print,'Using Background file :',bgdname
;get some stuff from the spectral file
  fxbopen,unit1,specname,1,hs
  fxbread,unit1,ctss,'counts',nanvalue=nanval
  fxbclose,unit1
  srcbscl=sxpar(hs,'BACKSCAL')
  stexp=sxpar(hs,'EXPOSURE')
  fxbopen,unit2,bgdname,1,hb
  fxbread,unit2,ctsb,'counts',nanvalue=nanval
  fxbclose,unit2 
  bgdbscl=sxpar(hb,'BACKSCAL')
  btexp=sxpar(hb,'EXPOSURE')
;now fill in the grid, reading the header of the first ARF
  for j=0l,nx(inst)-1l do begin
    sj=strtrim(string(j),2)
   for k=0l,ny(inst)-1l do begin
    sk=strtrim(string(k),2)
    arfname=specsep(0)+'src_x'+sj+'_y'+sk+'.arf'
      print,'Sequence & ARF filename '
      print,i,seqname(i),' ',arfname
      taba=readfits(arfname,ha,ext=1)
      specresp=tbget(ha,taba,'specresp')
    if i eq 0 and j eq 0 and k eq 0 then begin 
      rmfname=sxpar(ha,'RESPFILE')
      print,'Using response: ',rmfname
      elo=tbget(ha,taba,'energ_lo')
      ehi=tbget(ha,taba,'energ_hi')
      ewid=ehi-elo
      fxbopen,unit3,rmfname,2,hr
      fxbread,unit3,chan2e,'channel',nanvalue=nanval
      fxbread,unit3,eclo,'e_min',nanvalue=nanval
      fxbread,unit3,echi,'e_max',nanvalue=nanval
      fxbclose,unit3
;find the channel numbers corresponding to e1 and e2
      ier1=max(where(e1 ge elo and e1 lt ehi))
      ier2=min(where(e2 ge elo and e2 lt ehi))
      ic1=max(where(e1 ge eclo and e1 lt echi))
      ic2=min(where(e2 ge eclo and e2 lt echi))
      print,'Energy bound channels of RMF file: ',ier1,ier2
      print,'Actual energy bounds of RMF file: ',elo(ier1),ehi(ier2) 
      print,'SPECTRAL FILE channel boundaries: ',ic1,ic2
     endif
;count rate in spectral file computed only once for each sequence
      if j eq 0 and k eq 0 then begin
	ctspsec(i)=((total(ctss(ic1:ic2)))/stexp) -$
((total(ctsb(ic1:ic2)))*srcbscl/btexp/bgdbscl)
        print,'Count rate: ',ctspsec(i)
        printf,3,'File # ',i
        printf,3,'source counts ',total(ctss(ic1:ic2))
	printf,3,'Exposure 	',stexp
	printf,3,'bgd counts 	',total(ctsb(ic1:ic2))
	printf,3,'bgd exposure  ',btexp
	printf,3,'src scale factor ',srcbscl
	printf,3,'bgd scale factor ',bgdbscl
	printf,3,'Count rate	',ctspsec(i) 
      endif
     f1=specresp*ewid
     f2=fudge(i)*ctspsec(i)/total(f1(ier1:ier2))
	tmpgrd(i,j,k)=f2 
   endfor
  endfor
endfor
;now add to the cumulative difference grid
for kk=0l,ngrd-1l do begin
        ix=nx(inst)-1 & iy=ny(inst)-1
grd=grd*1000.*abs(tmpgrd(map(kk,1),0:ix,0:iy)-tmpgrd(map(kk,0),0:ix,0:iy))
endfor
;now add the sequence names and count rates to the running log
if n_elements(seqlog) eq 0 then begin
 seqlog=seqname(0:iseq-1l)
endif else begin
 seqlog=[seqlog,seqname(0:iseq-1l)]
endelse
if n_elements(ctslog) eq 0 then begin
 ctslog=ctspsec
endif else begin
 ctslog=[ctslog,ctspsec]
endelse
grd=grd*1000.
close,3
return
end
