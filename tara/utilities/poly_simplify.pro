;+
; NAME:
;	POLY_SIMPLIFY
;
; PURPOSE:
;	This function simplifies the vertices of an n-dimenstional polyline.
;	Vertices are removed if they are within a tolerance tangential distance from
;	an approximating line segment.
;
; CATEGORY:
;	Utility.
;
; CALLING SEQUENCE:
;	Result = POLY_SIMPLIFY(Vertices)
;
; INPUTS:
;	Vertices:	An array of vertices representing the polyline. Must
;				be a [m,n] array, where m is the dimensionality and
;				n is the number of vertices. m>1 and n>1.
;
; KEYWORD PARAMETERS:
;	TOL:	Set this keyword to the tolerance value to use. If Tol is
;			not set, negative or zero, then the tolerance will be set
;			automatically to the minimum average spacing along any
;			dimension between points.
;			Choice of tolerance is key to the amount of simplification.
;			The routine approximates the polyline with line segments that
;			are no further than tolerance from any vertices. Vertices
;			that are within tolerance from the approximating lines are
;			removed. If no tolerance is specified, the routine uses the
;			minimum distance in each dimension between vertices, and
;			multiplies this by 'factor' as a tolerance.
;
;	FACTOR:	Set this keyword instead of TOL to scale the automatic
;			tolerance. E.g FACTOR=10 will use 10x the minimum average
;			spacing between points as the tolerance. Ignored if TOL is set.
;
; OUTPUTS:
;	This function returns the simplified array of vertices. If an error
;	occurs, the output vertices will all be -1.
;
; PROCEDURE:
;	POLY_SIMPLIFY uses the Douglas-Peucker (DP) approximation algorithm
;	that is used extensively for both computer graphics and geographic
;	information systems. See Geometryalgorithms.com

; EXAMPLE:
;
;	see the test procedure at the end of this file
;
; MODIFICATION HISTORY:
; 	Written by:	Brad Gom, April 2004.
;-

function ps_dot,x,y		;dot product
	return, total(x*y)
end

function ps_norm2,x		;squared length of vector
;	return,ps_dot(x,x)	;this is slower (?)
	return,total(x^2)
end

function ps_d2,x,y			;distance squared of difference between points x and y
;	return, ps_norm2(x-y)
;	return, ps_dot(x-y,x-y)		;this is slower (?)
	return,total((x-y)^2)
end

pro simplifyDP,tol,vertices,j,k,mk
; This is the Douglas-Peucker recursive simplification routine
; It just marks vertices that are part of the simplified polyline
; for approximating the polyline subchain vertices[j] to vertices[k].
; Input: tol = approximation tolerance
; vertices[] = polyline array of vertex points
; j,k = indices for the subchain vertices[j] to vertices[k]
; Output: mk[] = array of markers matching vertex array vertices[]

;these shold already be long from poly_simplify
;	j=long(j)
;	k=long(k)

	if (k le j+1) then return ; there is nothing to simplify

	; check for adequate approximation by segment S from vertices[j] to vertices[k]
	maxi = j ; index of vertex farthest from S
	maxd2 = 0. ; distance squared of farthest vertex
	S = [[vertices[*,j]], [vertices[*,k]]]  ; segment from vertices[j] to vertices[k]
	u = S[*,1]-S[*,0] ; segment direction vector
	cu = ps_dot(u,u); segment length squared

	;test each vertex vertices[i] for max distance from S
	;compute using the Feb 2001 Algorithm's dist_Point_to_Segment()
	;Note: this works in any dimension (2D, 3D, ...)

	;Pb = base of perpendicular from vertices[i] to S
	;dv2 = distance vertices[i] to S squared

	for i=j+1,k-1 do begin
		;compute distance squared
		w = vertices[*,i] - S[*,0]
		cw = ps_dot(w,u)
		if cw le 0 then begin
			dv2 = ps_d2(vertices[*,i], S[*,0]);
			endif else begin
			if cu le cw then begin
				dv2 = ps_d2(vertices[*,i], S[*,1])
				endif else begin
				b = cw / cu;
				Pb = S[*,0] + b * u;
				dv2 = ps_d2(vertices[*,i], Pb);
				endelse
			endelse
		;test with current max distance squared
		if dv2 le maxd2 then continue
		;vertices[i] is a new max vertex
		maxi = i
		maxd2 = dv2
		endfor

	if (maxd2 gt tol^2) then begin ;// error is worse than the tolerance
;		split the polyline at the farthest vertex from S
		mk[maxi] = 1	; mark vertices[maxi] for the simplified polyline
;		recursively simplify the two subpolylines at vertices[*,maxi]
		simplifyDP, tol, vertices, j, maxi, mk ; // polyline vertices[j] to vertices[maxi]
		simplifyDP, tol, vertices, maxi, k, mk ; // polyline vertices[maxi] to vertices[k]
		endif
;		else the approximation is OK, so ignore intermediate vertices
	return
end


function poly_simplify,vertices,tol=tol,factor=factor

	if n_elements(factor) eq 0 then factor=1.
	;don't do any simplification if factor is 0 or -ve
	if factor lt 1 then return,vertices

	;vertices is a 2 or 3 (or more) by n array
	dim=size(vertices,/dimensions)
	n=dim[1]	;number of points
	if dim[0] lt 2 then begin
		message,'Vertices must be at least 2-D!',/cont
		return,vertices*0-1
		endif

	if n lt 2 then begin
		message,'There must be at least 2 Vertices!',/cont
		return,vertices*0-1
		endif

	if n_elements(tol) eq 0 then tol=0

	if tol le 0 then begin	;automatically set tolerance
		diff=abs((vertices-shift(vertices,0,1))[*,1:*])
		;minimum distance in x or y.. between adjacent points
		inds=where(diff ne 0)
		if inds[0] eq -1 then begin
			message,'Vertices has no unique points!',/cont
			return,vertices*0-1
			endif
		;tolerance is the minimum difference in x or y beteween adjacent points times the factor
		tol=min(diff[inds])*factor
		endif

	vt=vertices*0 ;  vertex buffer
	mk=bytarr(n)  ;  marker buffer

	; Mark vertices that will be in the simplified polyline
	;step 1: Initially Mark V0 and Vn
	;step 2: Recursively simplify by selecting vertex furthest away

;	STAGE 1. Vertex Reduction within tolerance of prior vertex cluster
	vt[*,0] = vertices[*,0];  start at the beginning
	k=1L
	pv=0L
	for i=1L,n-1 do begin
		if (ps_d2(vertices[*,i], vertices[*,pv]) lt tol^2) then continue
		vt[*,k++] = vertices[*,i];
		pv = i
		endfor

	if (pv lt n-1) then vt[*,k++] = vertices[*,n-1];  finish at the end

	; STAGE 2. Douglas-Peucker polyline simplification
	mk[0] = 1
	mk[k-1] = 1  ;  mark the first and last vertices

	simplifyDP, tol, vt, 0L, k-1, mk

	return, vt[*,where(mk)];  return simplified polyline
end


pro test
	RED = 	[0,	220,	255,	255,	255,	0,		0,		255,	160,	255]
	GREEN = 	[0,	140,	0,		127,	255,	255,	0,		0,		160,	255]
	BLUE = 	[0,	127,	0,		0,		0,		0,		255,	255,	160,	255]
	TVLCT,red,green,blue


	x=findgen(500)
	y=sin(x/100*30)/5+sin(x/1000*30)

	vertices=transpose([[x],[y]])

	window,0,xsize=1000,ysize=700
	plot,[min(x),max(x)],[min(y),max(y)],/nodata
	plots,vertices,color=9


	;choice of tolerance is key to the amount of simplification. The routine
	;approximates the polyline with line segments that are no further than
	;tolerance from any vertices. Vertices that are within tolerance from the
	;approximating lines are removed. If no tolerance is specified, the
	;routine uses the minimum distance in each dimension between vertices, and
	;multiplies this by 'factor' as a tolerance.

	;distance between adjacent points
	d=(sqrt(total((vertices-shift(vertices,0,1))^2,1)))[1:*]
	;this doesn't work so well if the scale of one dimension is much larger than another.

	;minimum distance in x or y.. between adjacent points
	dmin=min(abs((vertices-shift(vertices,0,1))[*,1:*]))

	;avg distance in x or y.. between points, whichever is smaller
	davg=(moment(abs((vertices-shift(vertices,0,1))[0,1:*])))[0]  <  (moment(abs((vertices-shift(vertices,0,1))[1,1:*])))[0]

	factor=100
	n=1
	t=systime(1)
	for i=0,n-1 do begin
		a=poly_simplify(vertices,tol=dmin)
		endfor
	ta=systime(1)-t
	t=systime(1)
	for i=0,n-1 do begin
		b=poly_simplify(vertices,tol=davg)
		endfor
	tb=systime(1)-t
	t=systime(1)
	for i=0,n-1 do begin
		c=poly_simplify(vertices,factor=factor)
		endfor
	tc=systime(1)-t
	t=systime(1)

	print,'Original number of output vertices: ',n_elements(vertices)/2

	wait,2
	print,'(RED) After simplifying with tolerance = minimum distance in x or y',dmin,'   number of output vertices: ',$
		n_elements(a)/2,' time: '+strtrim(ta/n)
	plots,a,psym=-4,color=2

	wait,2
	print,'(GREEN) After simplifying with tolerance = lesser of the average distace in x or y:',davg,'   number of output vertices: ',$
		n_elements(b)/2,' time: '+strtrim(tb/n)
	plots,b,psym=-4,color=5

	wait,2
	print,'(BLUE)  After simplifying with factor='+strtrim(factor,2)+':  number of output vertices: ',$
		n_elements(c)/2,' time: '+strtrim(tc/n)
	plots,c,psym=-4,color=6

end