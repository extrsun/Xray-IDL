pro issa_plot,ir0,ir0h,field=field,issa=issa,greymin=greymin,greymax=greymax $
,levels=levels,c_colors=c_colors,ntickx=ntickx,radius=radius,star_ra=star_ra, $
  star_dec=star_dec,starsym=starsym,star_label=star_label,fra=fra,fdec=fdec, $
	fequi=fequi,scolor=scolor,fcolor=fcolor,pcs=pcs,pthick=pthic
;**
;** Program to make cont_grey and imlabelmmm plot of ISSA plates used
;** in shadowing research.
;**
;** INPUTS:
;**		field	 == ROSAT PSPC observation number;
;**		issa	 == ISSA plate number;
;**		greymax	 == maximum value of ISSA plate in greyscale plot;
;**		levels	 == values of contour levels to plot;
;**		c_colors == colors of contour levels;
;**		ntickx	 == approximate number of x-axis tick;
;**		star_ra	 == RA (default B1950) of stars to show positions of;
;**		star_dec == DEC (default B1950) of stars to show positions of;
;**		starsym	 == symbols to plot stars with;
;**		radius	 == radius in arcminutes of PSPC field circle to draw;
;**
;** KEYWORD INPUTS:
;**
;** Written by kachun 22 July, 1994.

if n_elements(field) eq 0 then field='600045'
if n_elements(issa) eq 0 then issa='012'
if n_elements(greymax) eq 0 then greymax = 7.
if n_elements(levels) eq 0 then levels = [2.,4.,6.,8.]
;if n_elements(c_colors) eq 0 then c_colors = [241,200,100,50]
if n_elements(c_colors) eq 0 then c_colors = [50,50,50,1]
;if n_elements(ntickx) eq 0 then ntickx = 15
if n_elements(ntickx) eq 0 then ntickx = 9
if n_elements(ntickx) eq 0 then nticky = 8
if n_elements(radius) eq 0 then radius = 60
if n_elements(fequi) eq 0 then fequi='!6(B1950.)'
if n_elements(star_ra) eq 0 then star_ra = [52.5850,60.6439,62.373741,50.21768]
if n_elements(star_dec) eq 0 then star_dec = [-66.6587,-70.4093,-71.42265,-53.93254]
if n_elements(scolor) eq 0 then scolor = replicate(1,1,1,241)
if n_elements(star_label) eq 0 then star_label = ['HD 22252','HD 26109','VW Hyi','LB 1663']
;if fequi ne 1950 then precess,star_ra,star_dec,1950,fequi
nstar = n_elements(star_ra)
if n_elements(starsym) eq 0 then begin
;  plotsym,5,/fill
;  starsym = star_ra*0.+8
  starsym = star_ra*0.+7
endif

;env2,field
if n_elements(ir0) eq 0 then begin
	setenv,field
	 ir0 = readfits('i'+issa+'b4h0.fit',ir0h)
endif

sxaddpar,ir0h,'CROTA2',0
if n_elements(greymin) eq 0 then greymin=0.01
;** Plot ISSA plate plus grid:
!grid = 1
;cont_grey,ir0,ir0h,ir0,greymax=greymax,nbotpix=0, $
;  /pscolor,c_colors=c_colors,colors=2,levels=levels,corner=corner1, $
;  equi=fequi,ntickx=ntickx ;/putinfo
cont_grey,ir0,ir0h,ir0,greymax=greymax,nbotpix=0, $
  /pscolor,c_colors=c_colors,colors=2,levels=levels,corner=corner1, $
  equi=fequi,ntickx=ntickx,nticky=nticky,greymin=greymin,sub='',pcs=pcs,pthick=pthic,/greylog,xunit=0.25,barf=0 ;,greylog=2 ;/putinfo,/greylog

;** Plot stars:
plot_stars,ir0h,star_ra,star_dec,corner1,starsym,star_label=star_label, $
  color=scolor,charthick=1.5,charsize=1.
 

;** Get coordinates of PSPC field:
if n_elements(fra) eq 0 then $
	image_center,fra,fdec,/deg        ;** Coordinates in degrees.
if n_elements(fcolor) eq 0 then fcolor = replicate(241,n_elements(fra))
for k=0,n_elements(fra)-1 do $
tv_iras_x0,fra(k)/!radeg,fdec(k)/!radeg,ir0h,radius=radius,corner=corner1,fequi=fequi,color=fcolor(k)

!grid = 0

return
end
