function writeps1,filename,loadctnumber,Xsizenumber,Ysizenumber,XOffsetnumber,YOffsetnumber
thisDevice=!D.Name
Set_Plot,'PS'
Device,Filename=filename,Xsize=Xsizenumber,Ysize=Ysizenumber,XOffset=XOffsetnumber,YOffset=YOffsetnumber,/Inches,/Encapsulated,/Preview
Device,Color=1,Bits_Per_Pixel=8
loadct,loadctnumber
return,thisDevice
end

pro writeps2,thisDevice
Device,/Close_File
Device,Encapsulated=0,Preview=0
Set_Plot,thisDevice
end



function variablebinregion,imagedata,mincounts,xrange,yrange,initialbin,minmincounts,path,xedge,yedge

labelimage=intarr(xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1)
positionlabel=intarr(3)
labelimage[*,*]=3
filenumber=0

for binlabel=0,100 do begin

  binnumber=initialbin/2^binlabel

  if binnumber lt 1 then begin

    goto,NEXT2

  endif

  xnumber=(xrange[1]-xrange[0]+1)/binnumber
  ynumber=(yrange[1]-yrange[0]+1)/binnumber

  for i=0,xnumber-1 do begin
  for j=0,ynumber-1 do begin

    totalcounts=0

    for k=0,binnumber-1 do begin
    for t=0,binnumber-1 do begin

       if  labelimage[i*binnumber+k,j*binnumber+t] eq 0 or labelimage[i*binnumber+k,j*binnumber+t] eq 1 $
       or labelimage[i*binnumber+k,j*binnumber+t] eq 2 then begin

         goto,NEXT1

       endif

       totalcounts=totalcounts+imagedata[i*binnumber+k,j*binnumber+t]

    endfor
    endfor

    if totalcounts lt minmincounts then begin
       labelimage[i*binnumber:(i+1)*binnumber-1,j*binnumber:(j+1)*binnumber-1]=0
    endif

    if totalcounts lt mincounts and totalcounts ge minmincounts then begin

       labelimage[i*binnumber:(i+1)*binnumber-1,j*binnumber:(j+1)*binnumber-1]=1
;we can select one of the two command lines below.the first line is to set the background to be 0, the second line is to set the background
;to be an average value.
       positionlabel[0]=i*binnumber+binnumber/2+xedge
       positionlabel[1]=j*binnumber+binnumber/2+yedge
       positionlabel[2]=binnumber

       filename=path+'X'+strtrim(string(fix(positionlabel[0])+10000),2)+'Y'+$
       strtrim(string(fix(positionlabel[1])+10000),2)+'bin'+strtrim(string(fix(positionlabel[2])+100),2)+$
       'Cts'+strtrim(string(fix(totalcounts)+100000),2)+'.reg'

       region='rotbox('+strtrim(string(float(positionlabel[0])),2)+','+strtrim(string(float(positionlabel[1])),2)+$
       ','+strtrim(string(fix(positionlabel[2])),2)+','+strtrim(string(fix(positionlabel[2])),2)+',0)'

       openw,1,filename
        printf,1,'# Region file format: CIAO version 1.0'
        printf,1,region
       close,1
       free_lun,1

       filenumber=filenumber+1

    endif

    if totalcounts lt 4*mincounts and totalcounts ge mincounts then begin

       labelimage[i*binnumber:(i+1)*binnumber-1,j*binnumber:(j+1)*binnumber-1]=2
       positionlabel[0]=i*binnumber+binnumber/2+xedge
       positionlabel[1]=j*binnumber+binnumber/2+yedge
       positionlabel[2]=binnumber

       filename=path+'X'+strtrim(string(fix(positionlabel[0])+10000),2)+'Y'+$
       strtrim(string(fix(positionlabel[1])+10000),2)+'bin'+strtrim(string(fix(positionlabel[2])+100),2)+$
       'Cts'+strtrim(string(fix(totalcounts)+100000),2)+'.reg'

       region='rotbox('+strtrim(string(float(positionlabel[0])),2)+','+strtrim(string(float(positionlabel[1])),2)+$
       ','+strtrim(string(fix(positionlabel[2])),2)+','+strtrim(string(fix(positionlabel[2])),2)+',0)'

       openw,1,filename
        printf,1,'# Region file format: CIAO version 1.0'
        printf,1,region
       close,1
       free_lun,1

       filenumber=filenumber+1

    endif

    if totalcounts ge 4*mincounts then begin

       labelimage[i*binnumber:(i+1)*binnumber-1,j*binnumber:(j+1)*binnumber-1]=3

    endif

    NEXT1:

  endfor
  endfor

endfor

NEXT2:

result={label:labelimage,number:filenumber}

return,result

end




pro writeregionfile
mincounts=800
xrange=[4124,4380]
yrange=[4030,4286]
xedge=4124
yedge=4030
initialbin=32
minmincounts=500
path='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/regions2/'
fitsfile='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/aidfile/source_bin1.fits'
outputeps='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/aidfile/labelimg.eps'

imagedata=mrdfits(fitsfile,0,fitshead)

result=variablebinregion(imagedata,mincounts,xrange,yrange,initialbin,minmincounts,path,xedge,yedge)

print,result.number

window,1,retain=2,xsize=512,ysize=512
tvscl,result.label

thisDevice=writeps1(outputeps,0,7,5,0,0)
tvscl,result.label
writeps2,thisDevice

end
