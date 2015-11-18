pro writenewspec2, skyfile=skyfile,insfile=insfile,srcinsbkgfile=srcinsbkgfile,sourcefile=sourcefile,outspec=outspec,headerfile=headerfile

;this is a procedure used for double subtraction of the spectrum, refer to any spec_steps. it needs to be rewritten in a standard format.

;skyfile='skybckgrnd_halonew_new.fak'
;insfile='bckgrnd.pi'

;srcinsbkgfile='halonew.pi'
;sourcefile='halonew_src1.pi'

;outspec='halonew_new_bkg.pi'
;headerfile='bckgrnd_src1.pi'            ;only used for outputheader

primary=mrdfits(skyfile,0,h0)
skyspec=mrdfits(skyfile,1,h1)
exp=sxpar(h1,'EXPOSURE')

insspec=mrdfits(insfile,1,hb)
bexp=sxpar(hb,'EXPOSURE')

bckscale=sxpar(hb,'BACKSCAL')

chan=skyspec.channel

srcinsbkgspec=mrdfits(srcinsbkgfile,1,srcinsbkgh)
srcinsbkgexp=sxpar(srcinsbkgh,'EXPOSURE')

srcprimary=mrdfits(sourcefile,0,srch0)
srcspec=mrdfits(sourcefile,1,srch)
srcscale=sxpar(srch,'BACKSCAL')
srcexp=sxpar(srch,'EXPOSURE')
subctrate=(double(srcinsbkgspec.counts)+double(skyspec.counts)/bckscale*srcscale*srcinsbkgexp/srcexp)/srcinsbkgexp                 
subcts=long(subctrate*srcinsbkgexp)
totalbinnumber=n_elements(subctrate)
;for i=0,totalbinnumber-1 do begin
;	if subctrate[i]*srcinsbkgexp-subcts[i] ge 0.5 then begin
;		subcts[i]=subcts[i]+1
;	endif
;endfor

chan=srcspec.channel
parapi=srcspec.pi
fxwrite,outspec,srch0,srcprimary
fxbcreate,unit,outspec,srch
fxbwritm,unit,['CHANNEL','PI','COUNTS','COUNT_RATE'],chan,parapi,subcts,subctrate
fxbfinish,unit

plot,chan,subcts-subctrate*srcexp

end
