pro rewriteevtfits,xoffset,yoffset,evtfile,outputfile
;xoffset=2
;yoffset=1
;evtfile="/home/ljt/ljt/data/AboutSNR/SNR/Kepler/6718/6718/secondary/source_clean1.fits"
;outputfile="/home/ljt/ljt/data/AboutSNR/SNR/Kepler/6718/6718/secondary/source_clean1_calcoffset.fits"
data0=mrdfits(evtfile,0,fitshead0)
data1=mrdfits(evtfile,1,fitshead1)
data2=mrdfits(evtfile,2,fitshead2)
data3=mrdfits(evtfile,3,fitshead3)
data4=mrdfits(evtfile,4,fitshead4)
data5=mrdfits(evtfile,5,fitshead5)
data6=mrdfits(evtfile,6,fitshead6)
data7=mrdfits(evtfile,7,fitshead7)
;help,data[0].x
data1.x=data1.x-xoffset
data1.y=data1.y-yoffset
mwrfits,data0,outputfile,fitshead0
mwrfits,data1,outputfile,fitshead1
mwrfits,data2,outputfile,fitshead2
mwrfits,data3,outputfile,fitshead3
mwrfits,data4,outputfile,fitshead4
mwrfits,data5,outputfile,fitshead5
mwrfits,data6,outputfile,fitshead6
mwrfits,data7,outputfile,fitshead7

end
