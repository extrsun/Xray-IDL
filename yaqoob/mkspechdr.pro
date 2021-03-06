pro mkspechdr,totc,gti,texp,mission,instr,ibgd,npha,x0,y0,wbin,wmap,evtfile,h0,h1,h2,rsp=rsp,bck=bck,anc=anc
;Author T. Yaqoob - Oct 1995
; TO DO: MJD START TIME KEYWORD to be computed properly
if n_params(0) eq 0 then begin
print,'mkspechdr,geo,plist,gti,texp,mission,instr,ibgd,npha,x0,y0,wbin,wmap,evtfile,h0,h1,h2,rsp=rsp,bck=bck,anc=anc'
 print,'Create Headers for Making FITS spectral files '
 print,'** INPUTS ** '
 print,'GEO	- the geometrical info structure '
 print,'PLIST	- Photon List-Needed to compute start and stop times'
 print,'	- and the TOTCTS parameter in the header '
 print,'GTI	- The Good Time Intervals Array'
 print,'TEXP 	- Exposure Time in seconds '
 print,'MISSION - Name of Mission '
 print,'INSTR 	- Instrument ID (string) '
 print,'IBGD	- =0 if source spectrum, =1 if background spectrum '
 print,'NPHA 	- Number of output channels'
 print,'	- PHA channels if NPHA <0 and PI channels if NPHA>0' 
 print,'X0,Y0	- Bottom Left hand coords of WMAP '
 print,'WMAP	- WMAP'
 print,'EVTFILE - The name of one of the original Eventfiles for '
 print,'	- additional header information'  
 print,'RSP	- Name of .rmf file to be used (full path)'
 print,'BCK	- Name of background file '
 print,'ANC 	- Name of associated ARF file '
 print,' ** OUTPUTS ** '
 print,'H0,H1,H2 - Headers for the primary , 1st and 2nd extensions' 
 retall
end
;first read event file header
hevt=headfits(evtfile,ext=1)
;the primary header
fxhmake,hdr0,wmap,/extend,/date
nh0=where(strmid(hdr0,0,8) eq 'END     ',nend)
hdr0=hdr0(0:nh0(0)-1)
s1=strarr(57)
s1(0)='INSTRUME= ''GIS2    ''           / Instrument name'
dest=s1(0) & strput,dest,strmid(instr,0,8),11 & s1(0)=dest
s1(1)='ORIGIN  = ''NASA/GSFC''          / origin of fits file'
s1(2)='OBJECT  = ''M81/SN1993J''        / Name of observed object'
object=sxpar(hevt,'object') & dest=s1(2)
object=object+'                    '
strput,dest,strmid(object,0,11),11&s1(2)=dest
s1(3)='OBSERVER= ''ASCA Team             ''/ Principal Investigator'
obsvr=sxpar(hevt,'observer') & dest=s1(3)&strput,dest,obsvr,11&s1(3)=dest
s1(4)='RA_NOM  =           1.4906E+02 / Nominal R.A.'
ra=sxpar(hevt,'RA_NOM') & dest=s1(4)
strput,dest,string(format='(E10.4)',ra),20 & s1(4)=dest 
s1(5)='DEC_NOM =           6.8989E+01 / Nominal declination'
dest=s1(5)
dec=sxpar(hevt,'DEC_NOM') & strput,dest,string(format='(E10.4)',dec),20
s1(5)=dest
s1(6)='TELESCOP= ''ASCA    ''           / Telescope (mission) name'
dest=s1(6)&strput,dest,strmid(mission,0,8),11 & s1(6)=dest
s1(7)='MJDREF  =              48988.0 / MJD corresponding to SC clock start (1993.0)'
s1(8)='TIMEREF = ''LOCAL   ''           / Barycentric correction not applied to times'
s1(9)='TIMESYS = ''1993.0  ''           / Time measured from 1993 Jan 1 00:00 UT'
s1(10)='TIMEUNIT= ''s       ''           / unit for time related keywords'
s1(11)='TSTART  =  1.19116828803011998534E+07 / time start'
tstart=min(gti)
dest=s1(11)&strput,dest,string(format='(E26.20)',tstart),11&s1(11)=dest
s1(12)='TSTOP   =  1.19970425839000009000E+07 / time stop'
tstop=max(gti)
dest=s1(12)&strput,dest,string(format='(E26.20)',tstop),11&s1(12)=dest
s1(13)='TELAPSE =  8.53597035988010466099E+04 / Elapsed time'
telapse=tstop-tstart
dest=s1(13)&strput,dest,string(format='(E26.20)',telapse),11&s1(13)=dest
s1(14)='ONTIME  =  4.18819142923839390278E+04 / ontime'
dest=s1(14)&strput,dest,string(format='(E26.20)',texp),11
s1(14)=dest
get_asca_date,tstart,mo,day,year,hr,min,sec,dateobs,timeobs
s1(15)='DATE-OBS= ''18/05/93''           / date start'
s1(16)='TIME-OBS= ''20:48:02''           / time start'
dest=s1(15)&strput,dest,strmid(dateobs,0,8),11 & s1(15)=dest
dest=s1(16)&strput,dest,strmid(timeobs,0,8),11 & s1(16)=dest
s1(17)='DATE-END= ''19/05/93''           / date end'
s1(18)='TIME-END= ''20:30:42''           / time end'
get_asca_date,tstop,mo,day,year,hr,min,sec,dateend,timeend
dest=s1(17)&strput,dest,strmid(dateend,0,8),11 & s1(17)=dest
dest=s1(18)&strput,dest,strmid(timeend,0,8),11 & s1(18)=dest
s1(19)='MJD-OBS =     4.9125866700E+04 / Modified Julian date of the data start time'
s1(20)='DATAMODE= ''PH      ''           / Datamode' 
dmode=sxpar(hevt,'DATAMODE')
dest=s1(20)&strput,dest,strmid(dmode,0,8),11 & s1(20)=dest
s1(21)='CREATOR = ''ASCA IDL    ''       / Extractor'
s1(22)='ORBITBEG= ''93051813''           / value of ORBIT0 - applies to input file only'
obeg=sxpar(hevt,'ORBITBEG')
dest=s1(22)&strput,dest,strmid(obeg,0,8),11 & s1(22)=dest
s1(23)='ORBITEND= ''93051907''           / value of ORBIT1 - applies to input file only'
obend=sxpar(hevt,'ORBITEND') & dest=s1(23)&strput,dest,strmid(obend,0,8),11
s1(23)=dest
s1(24)='RADECSYS= ''FK5     ''           / World Coordinate System'
s1(25)='EQUINOX =          2000.000000 / Equinox for coordinate system'
s1(26)='RA_PNT  =            00.000000 / File average value of RA (degrees)'
s1(27)='DEC_PNT =            00.000000 / File average value of DEC (degrees)'
s1(28)='PA_PNT  =            00.000000 / File average value of ROLL (degrees)'
s1(29)='RA_PNTE =             0.000000 / File standard deviation of RA (degrees)'
s1(30)='DEC_PNTE=             0.000000 / File standard deviation of DEC (degrees)'
s1(31)='PA_PNTE =             0.000000 / File standard deviation of ROLL (degrees)'
s1(32)='RA__MEAN=            00.000000 / Mean pointing RA (degrees)'
s1(33)='DEC_MEAN=            00.000000 / Mean pointing DEC (degrees)'
rapnt=sxpar(hevt,'RA_PNT')
rapnte=sxpar(hevt,'RA_PNTE')
decpnt=sxpar(hevt,'DEC_PNT')
decpnte=sxpar(hevt,'DEC_PNTE')
papnt=sxpar(hevt,'PA_PNT')
papnte=sxpar(hevt,'PA_PNTE')
ramean=sxpar(hevt,'RA__MEAN')
decmean=sxpar(hevt,'DEC_MEAN')
timezero=sxpar(hevt,'timezero')
dest=s1(26)& strput,dest,string(format='(F19.6)',rapnt),11&s1(26)=dest
dest=s1(27)&strput,dest,string(format='(F19.6)',decpnt),11&s1(27)=dest
dest=s1(28)&strput,dest,string(format='(F19.6)',papnt),11&s1(28)=dest
dest=s1(29)&strput,dest,string(format='(F19.6)',rapnte),11&s1(29)=dest
dest=s1(30)&strput,dest,string(format='(F19.6)',decpnte),11&s1(30)=dest
dest=s1(31)&strput,dest,string(format='(F19.6)',papnte),11&s1(31)=dest
dest=s1(32)&strput,dest,string(format='(F19.6)',ramean),11&s1(32)=dest
dest=s1(33)&strput,dest,string(format='(F19.6)',decmean),11&s1(33)=dest
s1(34)='TIMEZERO=  0.00000000000000000000E+00 / Time Zero'
s1(35)='EXPOSURE=  4.18819142923839390278E+04 / Exposure time'
dest=s1(34)&strput,dest,string(format='(E26.20)',timezero),11&s1(34)=dest
dest=s1(35)&strput,dest,string(format='(E26.20)',texp),11&s1(35)=dest
s1(36)='HDUCLASS= ''ogip    ''           / Format conforms to OGIP/GSFC conventions'
s1(37)='HDUCLAS1= ''IMAGE   ''           / Extension contains an image'
s1(38)='HDUCLAS2= ''WMAP    ''           / Extension contains a weighted map'
s1(39)='CONTENT = ''SPECTRUM''           / spectrum file contains time intervals and events'
s1(40)='X-OFFSET=  9.90000000000000000000E+01 / X offset for map'
s1(41)='Y-OFFSET=  1.32000000000000000000E+02 / Y offset for map'
s1(42)='WMREBIN =                    1 / Weighted Map rebinning'
dest=s1(40)&strput,dest,string(format='(E26.20)',x0),11 & s1(40)=dest
dest=s1(41)&strput,dest,string(format='(E26.20)',y0),11 & s1(41)=dest
dest=s1(42)&strput,dest,string(format='(I19)',wbin),11 & s1(42)=dest
s1(43)='OBS-MODE=                  -10 / observing mode 1=point,2=slew,3=calibration'
s1(44)='CRPIX1  =  1.00000000000000000000E+00 / X axis reference pixel'
s1(45)='CRPIX2  =  1.00000000000000000000E+00 / Y axis reference pixel'
s1(46)='CRVAL1  =  9.90000000000000000000E+01 / coord of X ref pixel'
s1(47)='CRVAL2  =  1.32000000000000000000E+02 / coord of Y ref pixel'
dest=s1(46)&strput,dest,string(format='(E26.20)',x0),11 & s1(46)=dest
dest=s1(47)&strput,dest,string(format='(E26.20)',y0),11 & s1(47)=dest
s1(48)='OPTIC1  =  1.33300003049999986615E+02 / X Optical axis' 
s1(49)='OPTIC2  =  1.31699996950000013385E+02 / Y Optical axis'
if strmid(instr,0,4) eq 'SIS0' then begin
	opx0=662.72d0  & opy0=559.02d0
	backnorm=1280.d0*1280.d0
endif
if strmid(instr,0,4) eq 'SIS1' then begin
	opx0=618.28d0 & opy0=773.83d0
	backnorm=1280.d0*1280.d0
endif
if strmid(instr,0,4) eq 'GIS2' then begin
	opx0=133.30d0 & opy0=131.70d0
	backnorm=256.d0*256.d0
endif
if strmid(instr,0,4) eq 'GIS3' then begin
	opx0=119.70d0 & opy0=132.90d0
	backnorm=256.d0*256.d0
endif
dest=s1(48)&strput,dest,string(format='(E26.20)',opx0),11&s1(48)=dest
dest=s1(49)&strput,dest,string(format='(E26.20)',opy0),11&s1(49)=dest
s1(50)='CDELT1  =  2.50000000000000000000E-01 / X axis increment' 
s1(51)='CDELT2  =  2.50000000000000000000E-01 / Y axis increment' 
if strmid(instr,0,3) eq 'SIS' then pix = float(wbin)*0.027d0
if strmid(instr,0,3) eq 'GIS' then pix = float(wbin)*0.25d0
dest=s1(50)&strput,dest,string(format='(E26.20)',pix),11 &s1(50)=dest
dest=s1(51)&strput,dest,string(format='(E26.20)',pix),11 &s1(51)=dest
s1(52)='TOTCTS  =                      / Total pixel count'
ctotc=0l+(size(plist))(1)
dest=s1(52)&strput,dest,string(format='(I19)',totc),11&s1(52)=dest
s1(53)='EXTNAME = ''WMAP    ''           / Weighted Map Image' 
s1(54)='BACKSCAL=     4.9285888672E-03 / background scale factor'
;if ibgd eq 0 then backscl=geo.s1(4)/backnorm else $
;backscl=geo.s2(4)/backnorm
backscl=1.
dest=s1(54)&strput,dest,string(format='(E19.10)',backscl),11&s1(54)=dest
s1(55)='END'
h0=[hdr0,s1]
s2=strarr(39)
s2(0)='HDUCLASS= ''OGIP    ''           / format conforms to OGIP standard'
s2(1)='HDUCLAS1= ''SPECTRUM''           / PHA dataset (OGIP memo OGIP-92-007)'
s2(2)='HDUVERS1= ''1.1.0   ''           / Version of format (OGIP memo OGIP-92-007a)'
s2(3)='HDUCLAS2= ''        ''           / WARNING This is NOT an OGIP-approved value'
s2(4)='HDUCLAS3= ''COUNT   ''           / PHA data stored as Counts (not count/s)'
s2(5)='TLMIN1  =                    1 / Lowest legal channel number'
s2(6)='TLMAX1  =                    0 / Highest legal channel number'
nchan=abs(npha)+0l
dest=s2(6)&strput,dest,string(format='(I19)',nchan),11 &s2(6)=dest
s2(7)='POISSERR=                    T / Poissonian errors to be assumed'
s2(8)='STAT_ERR=                    0 / no statistical error specified'
s2(9)='SYS_ERR =                    0 / no systematic error specified'
s2(10)='DETCHANS=                    0 / Total No. of Detector Channels available'
dest=s2(10)&strput,dest,string(format='(I19)',nchan),11 &s2(10)=dest
s2(11)=s1(52) & s2(12)=s1(6) & s2(13)=s1(20) & s2(14)=s1(0)
s2(15)='FILTER  = ''none    ''           / Instrument filter in use'
s2(16)=s1(35) 
s2(17)='AREASCAL=  0.1000000000000E+01  /nominal effective area'
s2(18)='CORRSCAL=  0.1000000000000E+01  /correlation scale factor'
s2(19)=s1(54)
if n_elements(bck) eq 0 then bck='none    '
if n_elements(rsp) eq 0 then rsp='none    ' 
if n_elements(anc) eq 0 then anc='none    '
s2(20)='BACKFILE= '+"'"+bck+"'"+'           '+'/ background filename'
s2(21)='CORRFILE= ''none    ''           / correlation FITS file for'
s2(22)='RESPFILE= '+"'"+rsp+"'"+'           '+'/ response filename'
s2(23)='ANCRFILE= '+"'"+anc+"'"+'           '+'/ ARF filename '
s2(24)='XFLT0001= ''none    ''           / XSPEC selection filter description'
if npha lt 0 then $
s2(25)='CHANTYPE= ''PHA      ''           / Channels assigned by detector electronics' else $
s2(25)='CHANTYPE= ''PI      ''           / Channels assigned by detector electroincs'
s2(26)=s1(2) & s2(27)=s1(1) & s2(28)=s1(25) & s2(29)=s1(24)
s2(30)='PHAVERSN= ''1992a   ''           / OGIP classification of FITS format style'
s2(31)=s1(15) & s2(32)=s1(16) & s2(33)=s1(17) & s2(34)=s1(18)
s2(35)=s1(26) & s2(36)=s1(27) & s2(37)=s1(28)
s2(38)=s1(21) 
;s2(39)='END'
h1=s2
s3=strarr(15)
s3(0)='HDUCLASS= ''ogip    ''           / File conforms to OGIP/GSFC conventions'
s3(1)='HDUCLAS1= ''GTI     ''           / File contains Good Time Intervals'
s3(2)='HDUCLAS2= ''STANDARD''           / File contains Good Time Intervals'
s3(3)=s1(6) & s3(4)=s1(20) & s3(5)=s1(0) 
s3(6)='FILTER  = ''none    ''           / Instrument filter in use'
s3(7)=s1(35) & s3(8)=s1(2) & s3(9)=s1(1)
s3(10)=s1(15) & s3(11)=s1(16) & s3(12)=s1(17) & s3(13)=s1(18)
s3(14)=s1(21)
;s3(15)='END' 
h2=s3
return
end
