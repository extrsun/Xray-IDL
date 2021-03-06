rosatlib
defsysv,'!debug',0
;defsysv,'!block',3
defsysv,'!seq_no',''
defsysv,'!data_dir',''
;defsysv,'!idl_dir','/net/xray/pub/xrayidl'
defsysv,'!idl_dir','$xidl'
defsysv,'!proc','US'
defsysv,'!instr','aciss'
;defsysv,'!imapdir','$home/free_software/snowden/cals/'
defsysv,'!group',intarr(34,2)
defsysv,'!ebnds',fltarr(34,2)
low=[7,9,11,14,17,20,24,28,32,37,42,47,52,58,64,70,77,84,91,99,107,115]
low=[low,123,132,141,150,160,170,180,191,202,213,224,236]
hi=[8,10,13,16,19,23,27,31,36,41,46,51,57,63,69,76,83,90,98,106,114]
hi=[hi,122,131,140,149,159,169,179,190,201,212,223,235,247]
!group(0,0)=low
!group(0,1)=hi
low=[.07,.09,.11,.14,.17,.20,.24,.28,.32,.37,.42,.47,.52,.58,.64]
low=[low,.70,.77,.84,.91,.99,1.07,1.15,1.23,1.32,1.41,1.5,1.6,1.7]
low=[low,1.8,1.91,2.02,2.13,2.24,2.36]
hi=[.09,.11,.14,.17,.20,.24,.28,.32,.37,.42,.47,.52,.58,.64,.70,.77]
hi=[hi,.84,.91,.99,1.07,1.15,1.23,1.32,1.41,1.5,1.6,1.7,1.8,1.91,2.02]
hi=[hi,2.13,2.24,2.36,2.48]
!ebnds(0,0)=low
!ebnds(0,1)=hi
delvar,low,hi
defsysv,'!bandchb',intarr(8,2)
!bandchb(0,0)=[7,11,20,52,70,91,132,202] ;bandchb(1,0) is changed to 11 from
					;8 on July 26, 1993
!bandchb(0,1)=[7,19,51,69,90,131,201,247] ;R1 and R2 is considered to be band 3
defsysv,'!bandch',intarr(8,2)
!bandch(0,0)=[11,20,42,52,70,91,132,202]
!bandch(0,1)=[19,41,51,69,90,131,201,247]
defsysv,'!bandgroup',intarr(8,2)
!bandgroup(0,0)=[-1,2,5,10,12,15,18,23]
!bandgroup(0,1)=[-1,4,9,11,14,17,22,29]
defsysv,'!size_pixel',0.491  ;1 pixel=0''.5
defsysv,'!rapix',648000./!pi/!size_pixel
defsysv,'!sampix',14400.
defsysv,'!ampix',120
defsysv,'!annulus_out',4.
defsysv,'!annulus_in',1.5
defsysv,'!core_size',1.
;defsysv,'!annulus_out',6.
;defsysv,'!annulus_in',3.5
;defsysv,'!core_size',3.
defsysv,'!threshold',3.5
defsysv,'!star_area',12.5/60.
@fits_m.setup
;@specsysv
;device,true=24
;device,decompose=0 ;so that the true color mode will actually show color
;device,retain=2
;set_plot,'x'

; PAN's requirement
!PATH = !PATH+':'+expand_path('<IDL_DEFAULT>' + ':' + '+/Users/sunwei/ProgramFiles/addition/pub/pan')

device,true_color=24
device,decomposed=0
device,retain=2

;set colours 0 to 7 to be black, red, green, blue, cyan,
;magenta, yellow and white, in that order.
red=  [0,1,0,0,0,1,1,1]*255
green=[0,0,1,0,1,0,1,1]*255
blue= [0,0,0,1,1,1,0,1]*255
tvlct,red,green,blue
