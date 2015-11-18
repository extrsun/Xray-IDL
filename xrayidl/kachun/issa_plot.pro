pro issa_plot,field=field,issa=issa,greymax=greymax,levels=levels, $
  c_colors=c_colors,ntickx=ntickx,radius=radius,star_ra=star_ra, $
  star_dec=star_dec,starsym=starsym,star_label=star_label,l_colors=l_colors
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
if n_elements(c_colors) eq 0 then c_colors = [241,200,100,50]
if n_elements(l_colors) eq 0 then l_colors = 255
if n_elements(ntickx) eq 0 then ntickx = 15
if n_elements(radius) eq 0 then radius = 55
if n_elements(star_ra) eq 0 then star_ra = [52.5850,60.6439,62.373741]
if n_elements(star_dec) eq 0 then star_dec = [-66.6587,-70.4093,-71.42265]
if n_elements(star_label) eq 0 then star_label = ['HD 22252','HD 26109','VW Hyi']

nstar = n_elements(star_ra)
if n_elements(starsym) eq 0 then begin
;  plotsym,5,/fill
;  starsym = star_ra*0.+8
  starsym = star_ra*0.+1
endif

;env2,field
env,field

ir0 = readfits('i'+issa+'b4h0.fit',ir0h)

sxaddpar,ir0h,'CROTA2',0

;** Plot ISSA plate plus grid:
!grid = 1
cont_grey,ir0,ir0h,ir0,greymax=greymax,/putinfo,nbotpix=0, $
  /pscolor,c_colors=c_colors,colors=2,levels=levels,corner=corner1, $
  equi='!6(B1950.)',ntickx=15

;** Plot stars:
plot_stars,ir0h,star_ra,star_dec,corner1,starsym,star_label=star_label, $
  color=l_color

;** Get coordinates of PSPC field:
image_center,ra,dec,/deg         ;** Coordinates in degrees.
tv_iras_x0,ra/!radeg,dec/!radeg,ir0h,radius=radius,corner=corner1,color=255

yesno = ''
read,'Output to printer? ',yesno
if yesno eq 'y' or yesno eq 'yes' then begin
  !p.thick=2.
  !p.charsize=1.4
  !p.charthick=1.4
  landplot
  cont_grey,ir0,ir0h,ir0,greymax=greymax,/putinfo,nbotpix=0, $
    /pscolor,c_colors=c_colors,colors=2,levels=levels,corner=corner1, $
    equi='!6(B1950.)',ntickx=15
  tv_iras_x0,ra/!radeg,dec/!radeg,ir0h,radius=radius,corner=corner1,color=255
  !p.thick=1.
  !p.charsize=1.
  !p.charthick=1.
  plot_stars,ir0h,star_ra,star_dec,corner1,starsym,star_label=star_label, $
    color=l_color
  lx
endif

!grid = 0

return
end
