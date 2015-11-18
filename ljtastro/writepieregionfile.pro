function convertCcoor,imagedata,xrange,yrange,xedge,yedge,centralposi

Pi=3.1415926535

numberx=long(xrange[1]-xrange[0]+1)
numbery=long(yrange[1]-yrange[0]+1)
centralx=long(centralposi[0])
centraly=long(centralposi[1])
number=long(numberx*numbery)

Ccoorimg=fltarr(3,number)

k=long(0)
for i=long(0),numberx-1 do begin
for j=long(0),numbery-1 do begin
xcoor=long(xedge+i)
ycoor=long(yedge+j)
radius=sqrt(float((xcoor-centralx)^2+(ycoor-centraly)^2))
degreehudu=atan(float(ycoor-centraly)/(xcoor-centralx))
if (ycoor-centraly) ge 0 and (xcoor-centralx) ge 0 then begin 
degreehudu=degreehudu
endif
if (ycoor-centraly) ge 0 and (xcoor-centralx) lt 0 then begin
degreehudu=Pi+degreehudu
endif
if (ycoor-centraly) lt 0 and (xcoor-centralx) lt 0 then begin 
degreehudu=Pi+degreehudu
endif
if (ycoor-centraly) lt 0 and (xcoor-centralx) ge 0 then begin
degreehudu=2*Pi+degreehudu
endif

Ccoorimg[0,k]=radius
Ccoorimg[1,k]=degreehudu
Ccoorimg[2,k]=imagedata[i,j]

k=k+1

endfor
endfor

return,Ccoorimg

end



function variablebinpieregion,imagedata,mincounts,xrange,yrange,path,xedge,yedge,centralposi,hradius,lradius,hdegree,ldegree

Pi=3.1415926535

hdegreehudu=hdegree/180.*Pi
ldegreehudu=ldegree/180.*Pi
numberx=long(xrange[1]-xrange[0]+1)
numbery=long(yrange[1]-yrange[0]+1)
number=long(numberx*numbery)
Ccoorimg=convertCcoor(imagedata,xrange,yrange,xedge,yedge,centralposi)

lowradius=lradius

filenumber=0

BEGINPOSI:

for width=1,100 do begin

  highradius=lowradius+width

  if highradius gt hradius then begin

    goto,NEXT

  endif

  totalcounts=long(0)

  for i=long(0),number-1 do begin
    
    pixradius=Ccoorimg[0,i]
    pixdegreehudu=Ccoorimg[1,i]
    pixcounts=Ccoorimg[2,i]

    if pixradius lt highradius and pixradius ge lowradius and pixdegreehudu lt hdegreehudu and pixdegreehudu ge ldegreehudu then begin

       totalcounts=totalcounts+pixcounts

    endif

  endfor

  if totalcounts ge mincounts then begin

    filename=path+'Dl'+strtrim(string(fix(ldegree)+1000),2)+'h'+$
    strtrim(string(fix(hdegree)+1000),2)+'Rl'+strtrim(string(fix(lowradius)+10000),2)+$
    'h'+strtrim(string(fix(highradius)+10000),2)+'Cts'+strtrim(string(fix(totalcounts)+1000000),2)+'.reg'

    region='pie('+strtrim(string(float(centralposi[0])),2)+','+strtrim(string(float(centralposi[1])),2)+$
    ','+strtrim(string(float(lowradius)),2)+','+strtrim(string(float(highradius)),2)+$
    ','+strtrim(string(float(ldegree)),2)+','+strtrim(string(float(hdegree)),2)+')'

    openw,1,filename
    printf,1,'# Region file format: CIAO version 1.0'
    printf,1,region
    close,1
    free_lun,1

    filenumber=filenumber+1
    
    lowradius=highradius
    
    goto,BEGINPOSI 

  endif 

endfor

NEXT:

return,filenumber

end




pro writepieregionfile
mincounts=5000
;10000
xrange=[3871,3999]
yrange=[3996,4124]
xedge=3871
yedge=3996
centralposi=[3935.,4060.]
hradius=40
lradius=20
hdegree=360
ldegree=0


path='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/radial/regions/'
;'/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/radialprofile/regions3/'
fitsfile='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/aidfile/acisf00118_source_bin1.fits'
;'/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/radialprofile/Kepler_source_bin1.fits'

imagedata=mrdfits(fitsfile,0,fitshead)
help,imagedata

result=variablebinpieregion(imagedata,mincounts,xrange,yrange,path,xedge,yedge,centralposi,hradius,lradius,hdegree,ldegree)

print,result

end
