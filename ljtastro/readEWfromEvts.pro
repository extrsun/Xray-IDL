function readpara,filename,paranumber
parameters=fltarr(paranumber)
str1="ljt"

openr,lun,filename,/get_lun
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[0]=float(strmid(str1,11,4))
  parameters[1]=float(strmid(str1,17,4))
  parameters[2]=float(strmid(str1,25,2))
  parameters[3]=float(strmid(str1,31,5))
   for n1=0,6 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[4]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[5]=float(strmid(str1,0,16))
   for n1=0,3 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[6]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[7]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[8]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[9]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[10]=float(strmid(str1,0,16))
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[11]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[12]=float(strmid(str1,0,16))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[13]=float(strmid(str1,0,16))
    readf,lun,str1
;useless lines  
  readf,lun,str1
  parameters[14]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[15]=float(strmid(str1,0,16))
   for n1=0,3 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[16]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[17]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[18]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[19]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[20]=float(strmid(str1,0,16))
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[21]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[22]=float(strmid(str1,0,16))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[23]=float(strmid(str1,0,16))
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[24]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[25]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[26]=float(strmid(str1,0,16))

close,lun
free_lun,lun

return,parameters

end




function calcEW,parameters

returnpara=fltarr(3)
left=fltarr(2)
right=fltarr(2)
line=fltarr(2)
left[0:1]=parameters[0:1]
right[0:1]=parameters[2:3]
line[0:1]=parameters[4:5]
CtsL=parameters[6]
CtsR=parameters[7]
Ctsline=parameters[8]

rightposition=(right[1]+right[0])/2.
leftposition=(left[1]+left[0])/2.
lineposition=(line[1]+line[0])/2.

rightCts=CtsR/(right[1]-right[0])
leftCts=CtsL/(left[1]-left[0])
lineCts=Ctsline/(line[1]-line[0])

continuumvalue=(rightposition-lineposition)*(leftCts-rightCts)/(rightposition-leftposition)+rightCts
lineintensity=(lineCts-continuumvalue)*(line[1]-line[0])
EWvalue=lineintensity/(continuumvalue*(line[1]-line[0]))

;continuumvalue=(CtsL/(left[1]-left[0])-((line[1]+line[0])/2.-(left[1]+left[0])/2.)/((right[1]+right[0])/2.-(left[1]+left[0])/2.)$
;*(CtsL/(left[1]-left[0])-CtsR/(right[1]-right[0])))
;lineintensity=Ctsline/(line[1]-line[0])-continuumvalue
;EWvalue=lineintensity/continuumvalue

returnpara[0]=continuumvalue
returnpara[1]=lineintensity
returnpara[2]=EWvalue

return,returnpara

end



function EWofRegion,filepara,regionpara,bandpara

parameters=fltarr(9)
leftfile=filepara[0]
rightfile=filepara[1]
linefile=filepara[2]
xlow=regionpara[0]
xhigh=regionpara[1]
ylow=regionpara[2]
yhigh=regionpara[3]
xedge=regionpara[4]
yedge=regionpara[5]
xbin=xhigh-xlow+1
ybin=yhigh-ylow+1

leftimage=mrdfits(leftfile,0,fitsheadl)
rightimage=mrdfits(rightfile,0,fitsheadr)
lineimage=mrdfits(linefile,0,fitsheadline)

CtsL=0
CtsR=0
Ctsline=0

for i=0,xbin-1 do begin
for j=0,ybin-1 do begin
 CtsL=CtsL+leftimage[xlow-xedge+i,ylow-yedge+j]
 CtsR=CtsR+rightimage[xlow-xedge+i,ylow-yedge+j]
 Ctsline=Ctsline+lineimage[xlow-xedge+i,ylow-yedge+j]
endfor
endfor

parameters[0:5]=bandpara[0:5]
parameters[6]=CtsL
parameters[7]=CtsR
parameters[8]=Ctsline

EW=calcEW(parameters)

return,EW

end


pro readEWfromEvts
;0: FeL:        left:       500-800eV
;1:             right:      1000-1150eV
;2:             line:       800-1000eV
;3: SiK1.85:    left:       1410-1680eV
;4:             right:      1950-2100eV
;5:             line:       1710-1950eV
;6: Si2.2:      left:       1950-2100eV
;7:             right:      2580-2770eV
;8:             line:       2100-2300eV
;9: S2.4:       left:       1950-2100eV
;10:            right:      2580-2770eV
;11:            line:       2300-2520eV
;12:S2.88:      left:       2580-2770eV
;13:            right:      3200-3670eV
;14:            line:       2770-2990eV
;15:Ar3.1:      left:       2580-2770eV
;16:            right:      3200-3670eV
;17:            line:       2990-3200eV
;18:Ca3.8:      left:       3200-3670eV
;19:            right:      3940-6190eV
;20:            line:       3670-3940eV
;21:FeK:        left:       3940-6190eV
;22:            right:      6700-7600eV
;23:            line:       6240-6700eV


;0:X                        0:X
;1:Y                        1:Y
;2:bin                      2:bin  
;3:counts                   3:counts   
;4:nH                       4:nH
;5:TL                       5:TL
;6:OL                       6:OL
;7:NeL                      7:NeL
;8:MgL                      8:MgL
;9:SiL                      9:SiL
;10:SL                      10:SL
;11:CaL                     11:CaL
;12:FeL                     12:FeL
;13:Tau_uL                  13:Tau_uL
;14:normL                   14:normL
;15:TH                      15:TH
;16:OH                      16:OH
;17:NeH                     17:NeH
;18:MgH                     18:MgH
;19:SiH                     19:SiH
;20:SH                      20:SH
;21:CaH                     21:CaH
;22:FeH                     22:FeH
;23:Tau_uH                  23:Tau_uH
;24:normH                   24:normH
;25:PwrId                   25:PwrId
;26:PwrNorm                 26:PwrNorm
;                           27:FeLcontinuum
;                           28:FeLline
;                           29:FeLEW
;                           30:SiKcontinuum
;                           31:SiKline
;                           32:SiKEW
;                           33:Si2.2continuum
;                           34:Si2.2line
;                           35:Si2.2EW
;                           36:S2.4continuum
;                           37:S2.4line
;                           38:S2.4EW
;                           39:S2.88continuum
;                           40:S2.88line
;                           41:S2.88EW
;                           42:Ar3.1continuum
;                           43:Ar3.1line
;                           44:Ar3.1EW
;                           45:Ca3.8continuum
;                           46:Ca3.8line
;                           47:Ca3.8EW
;                           48:FeKcontinuum
;                           49:FeKline
;                           50:FeKEW

outputparafile='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/Kepler_parameters'
;fitsfile='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/aidfile/Kepler_source.fits'
files1='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/spectra/X13*_SNR3_0.3-10keV.xcm'
thesefiles1=findfile(files1,count=numfiles1)
files2='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/spectra/X14*_SNR3_0.3-10keV.xcm'
thesefiles2=findfile(files2,count=numfiles2)
totalnumfiles=numfiles1+numfiles2
xedge=3814
yedge=3950
paranumber=27
Ebandnumber=24
Eband=[0.50,0.80,1.00,1.15,0.80,1.00,1.41,1.68,1.95,2.10,1.71,1.95,1.95,2.10,2.58,2.77,2.10,2.30,1.95,2.10,2.58,2.77,2.30,2.52,$
       2.58,2.77,3.20,3.67,2.77,2.99,2.58,2.77,3.20,3.67,2.99,3.20,3.20,3.67,3.94,6.19,3.67,3.94,3.94,6.19,6.70,7.60,6.24,6.70]
fileforEW=strarr(Ebandnumber)
fileforEW[0]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_0.50-0.80keV_bin1.fits'
fileforEW[1]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_1.00-1.15keV_bin1.fits'
fileforEW[2]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_0.80-1.00keV_bin1.fits'
fileforEW[3]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_1.41-1.68keV_bin1.fits'
fileforEW[4]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_1.95-2.10keV_bin1.fits'
fileforEW[5]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_1.71-1.95keV_bin1.fits'
fileforEW[6]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_1.95-2.10keV_bin1.fits'
fileforEW[7]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_2.58-2.77keV_bin1.fits'
fileforEW[8]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_2.10-2.30keV_bin1.fits'
fileforEW[9]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_1.95-2.10keV_bin1.fits'
fileforEW[10]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_2.58-2.77keV_bin1.fits'
fileforEW[11]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_2.30-2.52keV_bin1.fits'
fileforEW[12]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_2.58-2.77keV_bin1.fits'
fileforEW[13]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_3.20-3.67keV_bin1.fits'
fileforEW[14]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_2.77-2.99keV_bin1.fits'
fileforEW[15]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_2.58-2.77keV_bin1.fits'
fileforEW[16]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_3.20-3.67keV_bin1.fits'
fileforEW[17]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_2.99-3.20keV_bin1.fits'
fileforEW[18]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_3.20-3.67keV_bin1.fits'
fileforEW[19]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_3.94-6.19keV_bin1.fits'
fileforEW[20]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_3.67-3.94keV_bin1.fits'
fileforEW[21]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_3.94-6.19keV_bin1.fits'
fileforEW[22]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_6.70-7.60keV_bin1.fits'
fileforEW[23]='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/foraddEWpara/source_6.24-6.70keV_bin1.fits'
EWcounts=fltarr(Ebandnumber)
outputparanumber=51
parafile=fltarr(outputparanumber,totalnumfiles)
thesefiles=strarr(totalnumfiles)
thesefiles[0:numfiles1-1]=thesefiles1[0:numfiles1-1]
thesefiles[numfiles1:totalnumfiles-1]=thesefiles2[0:numfiles2-1]

print,totalnumfiles
;help,Eband
;print,n_elements(Eband)


;data=mrdfits(fitsfile,1,fitshead)
;help,data
;print,data[0].energy
;print,data[0].x
;print,data[0].y
;print,n_elements(data)
;subdata=fltarr(3,n_elements(data))
;subdata[0,*]=data.x
;subdata[1,*]=data.y
;subdata[2,*]=data.energy
;print,subdata[*,0]

;calcEWparameters=fltarr(9)
filepara=strarr(3)
regionpara=fltarr(6)
bandpara=fltarr(6)

;openw,lun,outputparafile,/get_lun

for i=0,totalnumfiles-1 do begin

 parafile[0:paranumber-1,i]=readpara(thesefiles[i],paranumber)

 ;for k=0,Ebandnumber-1 do begin
 ; EWcounts[k]=n_elements(where(subdata[0,*] lt parafile[0]+parafile[2]/2. and subdata[0,*] ge parafile[0]-parafile[2]/2.$
 ;   and subdata[1,*] lt parafile[1]+parafile[2]/2. and subdata[1,*] ge parafile[1]-parafile[2]/2.$
 ;   and subdata[2,*] ge Eband[k*2]*1000 and subdata[2,*] lt Eband[k*2+1]*1000))
 ;endfor

 for k=0,fix(Ebandnumber/3)-1 do begin
  filepara[0:2]=fileforEW[k*3:(k+1)*3-1]
  regionpara[0]=parafile[0,i]-parafile[2,i]/2.
  regionpara[1]=parafile[0,i]+parafile[2,i]/2.
  regionpara[2]=parafile[1,i]-parafile[2,i]/2.
  regionpara[3]=parafile[1,i]+parafile[2,i]/2.
  regionpara[4]=xedge
  regionpara[5]=yedge
  bandpara[0:5]=Eband[k*6:(k+1)*6-1]
  EW=EWofRegion(filepara,regionpara,bandpara)
  ;calcEWparameters[0:5]=Eband[k*6:(k+1)*6-1]
  ;calcEWparameters[6:8]=EWcounts[k*3:(k+1)*3-1]
  ;EW=calcEW(calcEWparameters)
  parafile[paranumber+k*3,i]=EW[0]
  parafile[paranumber+k*3+1,i]=EW[1]
  parafile[paranumber+k*3+2,i]=EW[2]
 endfor

 printf,lun,parafile[*,i]
 print,i
endfor

;close,lun
;free_lun,lun

;print,parafile[*,0]
help,parafile


end
