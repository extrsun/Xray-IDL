pro cluster_acf,blow,bhigh,angle0,angle,acf,cluster=cluster,beta=beta,norm=norm
;+
; get a cluster ACF
;*INPUTS
; anlge0 - one parameter in the cluster beta model in units of arcmin
;-
; 
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - cluster_acf,blow,bhigh,angle0,angle,acf'
print,',cluster=cluster'
return
end
;
radius=40.
length=20.
dim=141
;--------------------------------------
; get a cluster image
dist_circle,angle,dim,(dim-1.)/2.,(dim-1)/2.
angle=angle/4. ;in units of arcminutes
;
; using beta model for the flux distribution
if n_elements(norm) eq 0 then norm=2*!pi*angle0*angle0
cluster=(1. + (angle/angle0)^2)^(3.*beta-0.5)
cluster=norm/cluster
;-------------------------------------
; get psf image
modelrate,'po17.dat',!bandgroup(blow,0),!bandgroup(bhigh,1),rate,group,dir=$
	'~/rosat/analysis/spectral/'

calcpsfp,61,15,rate,offangle,prof,psf,group=group,binrat=7
;
; convolve the cluster image with the psf image
get_conv,cluster,psf,conv
;
; get the acf of the convolved image
dir='~/rosat/analysis/cor/'
outfile='cl'+ strtrim(fix(angle0*10),2) + '_acf'+ $
 	strtrim(blow,2)+strtrim(bhigh,2) +  '.dat'
get_acf,radius,conv,conv,length,angle,acf,acfe,nbin,countm,coutrms,outfile=$
	outfile,dir=dir
cluster=conv
if !debug eq 1 then stop
end