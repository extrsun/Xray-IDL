pro truecolor
zoomx=4
zoomy=4
redf="/home/ljt/ljt/data/AboutAGN/NGC2403/2014/primary/truecolor/0.3-1.3keV_bin4.fits"
greenf="/home/ljt/ljt/data/AboutAGN/NGC2403/2014/primary/truecolor/1.3-3.0keV_bin4.fits"
bluef="/home/ljt/ljt/data/AboutAGN/NGC2403/2014/primary/truecolor/3.0-9.0keV_bin4.fits"
outputname="/home/ljt/ljt/data/AboutAGN/NGC2403/2014/primary/truecolor/idltrue_bin4b.jpg"
redd=mrdfits(redf,0,redh)
greend=mrdfits(greenf,0,greenh)
blued=mrdfits(bluef,0,blueh)

xsize=n_elements(redd[*,0])
ysize=n_elements(redd[0,*])

zoomredd=congrid(redd,xsize*zoomx,ysize*zoomy,/INTERP)
zoomgreend=congrid(greend,xsize*zoomx,ysize*zoomy,/INTERP)
zoomblued=congrid(blued,xsize*zoomx,ysize*zoomy,/INTERP)

image=fltarr(xsize*zoomx,ysize*zoomy,3)
image[*,*,0]=zoomredd*100.
image[*,*,1]=zoomgreend*100.
image[*,*,2]=zoomblued*200.

write_jpeg,outputname,image,true=3,quality=100

end
