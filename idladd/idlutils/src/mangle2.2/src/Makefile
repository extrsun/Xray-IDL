#Makefile for Darwin i386 real8 generated with configure.

###############################################################################
# LOCAL CONFIGURATION: EDIT AS DESIRED
###############################################################################

#------
# Gnu

#CC = gcc
#CFLAGS = -g -O3 -Wall -DMACOSX -DGCC -m32 -D_FILE_OFFSET_BITS=64
CFLAGS = $(X_CFLAGS) -D$(UNAME)

# gnu g77 (which assumes no SAVE)
#F77 = g77
#FFLAGS = -Wimplicit -fbounds-check -g -O3 -DG77 -m32 -D_FILE_OFFSET_BITS=64
FFLAGS = $(X_FFLAGS) -DF2C -fPIC

#MAKE=gmake

all: mangle

#-----------------
# starlink library

# path to search for the starlink library
ISLIB = 
#-L./lib/starlink -L$(HOME)/lib/starlink

# starlink library should be called libstar.a
# Edit as desired,
# or make a soft link from the actual starlink library to libstar.a:
#    ln -s <name_of_starlink_library> libstar.a
SLIB = 
#-lstar

###############################################################################
# END OF LOCAL CONFIGURATION
###############################################################################

BIN=$(IDLUTILS_DIR)/bin
ILIB = -L$(IDLUTILS_DIR)/lib/
LLIB = -lmangle

PROGS = $(BIN)/balkanize $(BIN)/drangle $(BIN)/harmonize $(BIN)/map $(BIN)/pixelize $(BIN)/pixelmap $(BIN)/polyid $(BIN)/poly2poly $(BIN)/ransack $(BIN)/rasterize $(BIN)/snap $(BIN)/unify $(BIN)/weight test
#ddcount rotate rrcoeffs

COBJ = advise_fmt.o braktop_.o cmminf.o convert.o copy_format.o copy_poly.o drandom.o drangle_polys.o dranglepolys_.o dump_poly.o findtop_.o get_pixel.o garea.o gcmlim.o gphbv.o gphi.o gptin.o gspher.o gsphr.o gvert.o gvlim.o gvphi.o harmonize_polys.o harmonizepolys_.o healpix_ang2pix_nest.o healpixpolys.o ikrand.o msg.o new_poly.o new_vert.o partition_poly.o places.o poly_id.o poly_sort.o prune_poly.o rasterize.o rdangle.o rdline.o rdmask.o rdmask_.o rdspher.o rrcoeffs.o scale.o sdsspix.o search.o snap_poly.o split_poly.o strcmpl.o strdict.o vmid.o weight_fn.o which_pixel.o wrangle.o wrho.o wrmask.o wrrrcoeffs.o wrspher.o

FOBJ = azel.s.o azell.s.o braktop.s.o felp.s.o fframe.s.o findtop.s.o garea.s.o gaream.s.o gcmlim.s.o gphi.s.o gphim.s.o gphbv.s.o gptin.s.o gsphera.s.o gspher.s.o gsubs.s.o gvert.s.o gvlim.s.o gvphi.s.o iylm.s.o pix2vec_nest.s.o twodf100k.o twodf230k.o twoqz.o wlm.s.o wrho.s.o

# objects not already present (will try to extract them from $(IDLUTILS_DIR)/lib/libmangle.a)
OBJ := $(strip $(foreach objfile, $(COBJ) $(FOBJ), $(shell if [ ! -r $(objfile) ]; then echo $(objfile); fi)))

mangle:
	@ if [ -r $(IDLUTILS_DIR)/lib/libmangle.a -a "$(OBJ)" ] ; then \
		echo "extracting files from $(IDLUTILS_DIR)/lib/libmangle.a" ; \
		ar xo $(IDLUTILS_DIR)/lib/libmangle.a $(OBJ) >/dev/null 2>&1 ; \
		rm -f __.SYMDEF ; \
	fi
	@ $(MAKE) mangler
	@ rm -f $(COBJ) $(FOBJ)

mangler:
	$(MAKE) $(IDLUTILS_DIR)/lib/libmangle.a $(PROGS)

install:
	if [ -d "$(BIN)" ] ; then mv $(PROGS) $(BIN) ; fi
	-@ mkdir -p $(IDLUTILS_DIR)/src/mangle2.2/src
	-@ cp ../CHANGES $(IDLUTILS_DIR)/src/mangle2.2
	-@ cp ../COPYRIGHT $(IDLUTILS_DIR)/src/mangle2.2
	-@ cp ../INSTALL $(IDLUTILS_DIR)/src/mangle2.2
	-@ cp ../README* $(IDLUTILS_DIR)/src/mangle2.2
	-@ cp *.c $(IDLUTILS_DIR)/src/mangle2.2/src
	-@ cp *.f $(IDLUTILS_DIR)/src/mangle2.2/src
	-@ cp *.h $(IDLUTILS_DIR)/src/mangle2.2/src
	-@ cp Makefile $(IDLUTILS_DIR)/src/mangle2.2/src

clean:
	rm -f *.o core $(IDLUTILS_DIR)/lib/libmangle.a $(PROGS)
	if [ -d "$(BIN)" ] ; then cd $(BIN) ; rm -f core $(PROGS) ; fi

static:
	$(MAKE) FFLAGS="$(FFLAGS) $(STATICFLAGS)"

$(IDLUTILS_DIR)/lib/libmangle.a: $(COBJ) $(FOBJ) Makefile
	@ if [ -r $(IDLUTILS_DIR)/lib/libmangle.a ] ; then echo "updating $(IDLUTILS_DIR)/lib/libmangle.a" ; else echo "creating $(IDLUTILS_DIR)/lib/libmangle.a" ; fi
	ar ru $(IDLUTILS_DIR)/lib/libmangle.a $(COBJ) $(FOBJ)
	ranlib $(IDLUTILS_DIR)/lib/libmangle.a

$(BIN)/balkanize: balkanize.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/balkanize balkanize.o $(ILIB) $(LLIB)

$(BIN)/ddcount: ddcount.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/ddcount ddcount.o $(ILIB) $(LLIB)

$(BIN)/drangle: drangle.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/drangle drangle.o $(ILIB) $(LLIB)

$(BIN)/harmonize: harmonize.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/harmonize harmonize.o $(ILIB) $(LLIB)

$(BIN)/map: map.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/map map.o $(ILIB) $(LLIB)

$(BIN)/pixelize: pixelize.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/pixelize pixelize.o $(ILIB) $(LLIB)

$(BIN)/pixelmap: pixelmap.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/pixelmap pixelmap.o $(ILIB) $(LLIB)

$(BIN)/polyid: polyid.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/polyid polyid.o $(ILIB) $(LLIB)

$(BIN)/poly2poly: poly2poly.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/poly2poly poly2poly.o $(ILIB) $(LLIB)

$(BIN)/ransack: ransack.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/ransack ransack.o $(ILIB) $(LLIB)

$(BIN)/rasterize: rasterize.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/rasterize rasterize.o $(ILIB) $(LLIB)

#rotate: rotate.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
#$(F77) $(FFLAGS) -o rotate rotate.o $(ILIB) $(LLIB) $(ISLIB) $(SLIB)

$(BIN)/rrcoeffs: rrcoeffs.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/rrcoeffs rrcoeffs.o $(ILIB) $(LLIB)

$(BIN)/snap: snap.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/snap snap.o $(ILIB) $(LLIB)

$(BIN)/unify: unify.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/unify unify.o $(ILIB) $(LLIB)

$(BIN)/weight: weight.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o $(BIN)/weight weight.o $(ILIB) $(LLIB)

test: test.o $(IDLUTILS_DIR)/lib/libmangle.a Makefile
	$(F77) $(FFLAGS) -o test test.o $(ILIB) $(LLIB)


advise_fmt.o: angunit.h manglefn.h advise_fmt.c
	$(CC) $(CFLAGS) -c advise_fmt.c
balkanize.o: parse_args.c defaults.h manglefn.h usage.h balkanize.c
	$(CC) $(CFLAGS) -c balkanize.c
braktop_.o: manglefn.h braktop_.c
	$(CC) $(CFLAGS) -c braktop_.c
cmminf.o: manglefn.h cmminf.c
	$(CC) $(CFLAGS) -c cmminf.c
convert.o: manglefn.h convert.c
	$(CC) $(CFLAGS) -c convert.c
copy_format.o: manglefn.h copy_format.c
	$(CC) $(CFLAGS) -c copy_format.c
copy_poly.o: manglefn.h copy_poly.c
	$(CC) $(CFLAGS) -c copy_poly.c
ddcount.o: parse_args.c angunit.h defaults.h inputfile.h manglefn.h usage.h ddcount.c
	$(CC) $(CFLAGS) -c ddcount.c
drandom.o: drandom.c
	$(CC) $(CFLAGS) -c drandom.c
drangle.o: parse_args.c angunit.h defaults.h inputfile.h manglefn.h usage.h drangle.c
	$(CC) $(CFLAGS) -c drangle.c
drangle_polys.o: manglefn.h pi.h drangle_polys.c
	$(CC) $(CFLAGS) -c drangle_polys.c
dranglepolys_.o: manglefn.h dranglepolys_.c
	$(CC) $(CFLAGS) -c dranglepolys_.c
dump_poly.o: manglefn.h dump_poly.c
	$(CC) $(CFLAGS) -c dump_poly.c
findtop_.o: manglefn.h findtop_.c
	$(CC) $(CFLAGS) -c findtop_.c
get_pixel.o: manglefn.h get_pixel.c
	$(CC) $(CFLAGS) -c get_pixel.c
garea.o: logical.h manglefn.h garea.c
	$(CC) $(CFLAGS) -c garea.c
gcmlim.o: manglefn.h gcmlim.c
	$(CC) $(CFLAGS) -c gcmlim.c
gphbv.o: manglefn.h gphbv.c
	$(CC) $(CFLAGS) -c gphbv.c
gphi.o: manglefn.h gphi.c
	$(CC) $(CFLAGS) -c gphi.c
gptin.o: manglefn.h gptin.c
	$(CC) $(CFLAGS) -c gptin.c
gspher.o: manglefn.h pi.h gspher.c
	$(CC) $(CFLAGS) -c gspher.c
gsphr.o: manglefn.h pi.h gsphr.c
	$(CC) $(CFLAGS) -c gsphr.c
gvert.o: manglefn.h gvert.c
	$(CC) $(CFLAGS) -c gvert.c
gvlim.o: manglefn.h gvlim.c
	$(CC) $(CFLAGS) -c gvlim.c
gvphi.o: manglefn.h gvphi.c
	$(CC) $(CFLAGS) -c gvphi.c
harmonize.o: parse_args.c defaults.h manglefn.h usage.h harmonize.c
	$(CC) $(CFLAGS) -c harmonize.c
harmonize_polys.o: manglefn.h pi.h harmonize_polys.c
	$(CC) $(CFLAGS) -c harmonize_polys.c
harmonizepolys_.o: manglefn.h harmonizepolys_.c
	$(CC) $(CFLAGS) -c harmonizepolys_.c
healpix_ang2pix_nest.o: manglefn.h pi.h  healpix_ang2pix_nest.c
	$(CC) $(CFLAGS) -c healpix_ang2pix_nest.c
healpixpolys.o:  manglefn.h pi.h healpixpolys.c
	$(CC) $(CFLAGS) -c healpixpolys.c
ikrand.o: manglefn.h ikrand.c
	$(CC) $(CFLAGS) -c ikrand.c
map.o: parse_args.c angunit.h defaults.h inputfile.h manglefn.h usage.h map.c
	$(CC) $(CFLAGS) -c map.c
msg.o: manglefn.h msg.c
	$(CC) $(CFLAGS) -c msg.c
new_poly.o: manglefn.h new_poly.c
	$(CC) $(CFLAGS) -c new_poly.c
new_vert.o: manglefn.h new_vert.c
	$(CC) $(CFLAGS) -c new_vert.c
partition_poly.o: manglefn.h partition_poly.c
	$(CC) $(CFLAGS) -c partition_poly.c
pixelize.o: parse_args.c defaults.h manglefn.h usage.h pixelize.c
	$(CC) $(CFLAGS) -c pixelize.c
pixelmap.o: parse_args.c defaults.h manglefn.h usage.h pixelmap.c
	$(CC) $(CFLAGS) -c pixelmap.c
places.o: manglefn.h places.c
	$(CC) $(CFLAGS) -c places.c
poly2poly.o: parse_args.c defaults.h manglefn.h usage.h poly2poly.c
	$(CC) $(CFLAGS) -c poly2poly.c
poly_id.o: manglefn.h poly_id.c
	$(CC) $(CFLAGS) -c poly_id.c
polyid.o: parse_args.c angunit.h defaults.h inputfile.h manglefn.h usage.h polyid.c
	$(CC) $(CFLAGS) -c polyid.c
poly_sort.o: manglefn.h poly_sort.c
	$(CC) $(CFLAGS) -c poly_sort.c	
prune_poly.o: manglefn.h prune_poly.c
	$(CC) $(CFLAGS) -c prune_poly.c
ransack.o: parse_args.c angunit.h defaults.h manglefn.h usage.h ransack.c
	$(CC) $(CFLAGS) -c ransack.c
rasterize.o: parse_args.c pi.h defaults.h manglefn.h usage.h rasterize.c
	$(CC) $(CFLAGS) -c rasterize.c
rdangle.o: manglefn.h rdangle.c
	$(CC) $(CFLAGS) -c rdangle.c
rdline.o: inputfile.h rdline.c
	$(CC) $(CFLAGS) -c rdline.c
rdmask_.o: defaults.h manglefn.h rdmask_.c
	$(CC) $(CFLAGS) -c rdmask_.c
rdmask.o: inputfile.h manglefn.h rdmask.c
	$(CC) $(CFLAGS) -c rdmask.c
rdspher.o: manglefn.h rdspher.c
	$(CC) $(CFLAGS) -c rdspher.c
rotate.o: parse_args.c parse_fopt.c angunit.h defaults.h inputfile.h manglefn.h usage.h rotate.c
	$(CC) $(CFLAGS) -c rotate.c
rrcoeffs.o: parse_args.c defaults.h manglefn.h usage.h rrcoeffs.c
	$(CC) $(CFLAGS) -c rrcoeffs.c
scale.o: manglefn.h scale.c
	$(CC) $(CFLAGS) -c scale.c
sdsspix.o: manglefn.h sdsspix.c
	$(CC) $(CFLAGS) -c sdsspix.c
search.o: manglefn.h search.c
	$(CC) $(CFLAGS) -c search.c
snap.o: parse_args.c defaults.h manglefn.h usage.h snap.c
	$(CC) $(CFLAGS) -c snap.c
snap_poly.o: manglefn.h snap_poly.c
	$(CC) $(CFLAGS) -c snap_poly.c
split_poly.o: manglefn.h split_poly.c
	$(CC) $(CFLAGS) -c split_poly.c
strcmpl.o: manglefn.h strcmpl.c
	$(CC) $(CFLAGS) -c strcmpl.c
strdict.o: manglefn.h strdict.c
	$(CC) $(CFLAGS) -c strdict.c
test.o: manglefn.h test.c
	$(CC) $(CFLAGS) -c test.c
unify.o: parse_args.c defaults.h manglefn.h usage.h unify.c
	$(CC) $(CFLAGS) -c unify.c
vmid.o: manglefn.h vmid.c
	$(CC) $(CFLAGS) -c vmid.c
which_pixel.o: manglefn.h which_pixel.c
	$(CC) $(CFLAGS) -c which_pixel.c
weight.o: parse_args.c defaults.h manglefn.h usage.h weight.c
	$(CC) $(CFLAGS) -c weight.c
weight_fn.o: inputfile.h manglefn.h weight_fn.c
	$(CC) $(CFLAGS) -c weight_fn.c
wrangle.o: manglefn.h wrangle.c
	$(CC) $(CFLAGS) -c wrangle.c
wrho.o: manglefn.h wrho.c
	$(CC) $(CFLAGS) -c wrho.c
wrmask.o: manglefn.h wrmask.c
	$(CC) $(CFLAGS) -c wrmask.c
wrrrcoeffs.o: manglefn.h wrrrcoeffs.c
	$(CC) $(CFLAGS) -c wrrrcoeffs.c
wrspher.o: manglefn.h wrspher.c
	$(CC) $(CFLAGS) -c wrspher.c

azell.s.o: azell.s.f
	$(F77) $(FFLAGS) -c azell.s.f
azel.s.o: azel.s.f
	$(F77) $(FFLAGS) -c azel.s.f
braktop.s.o: braktop.s.f
	$(F77) $(FFLAGS) -c braktop.s.f
felp.s.o: frames.par felp.s.f
	$(F77) $(FFLAGS) -c felp.s.f
fframe.s.o: frames.par radian.par fframe.s.f
	$(F77) $(FFLAGS) -c fframe.s.f
findtop.s.o: heapsort.inc findtop.s.f
	$(F77) $(FFLAGS) -c findtop.s.f
gaream.s.o: pi.par gaream.s.f
	$(F77) $(FFLAGS) -c gaream.s.f
garea.s.o: pi.par garea.s.f
	$(F77) $(FFLAGS) -c garea.s.f
gcmlim.s.o: pi.par gcmlim.s.f
	$(F77) $(FFLAGS) -c gcmlim.s.f
gphbv.s.o: pi.par gphbv.s.f
	$(F77) $(FFLAGS) -c gphbv.s.f
gphim.s.o: pi.par gphim.s.f
	$(F77) $(FFLAGS) -c gphim.s.f
gphi.s.o: pi.par gphi.s.f
	$(F77) $(FFLAGS) -c gphi.s.f
gptin.s.o: gptin.s.f
	$(F77) $(FFLAGS) -c gptin.s.f
gsphera.s.o: pi.par gsphera.s.f
	$(F77) $(FFLAGS) -c gsphera.s.f
gspher.s.o: pi.par gspher.s.f
	$(F77) $(FFLAGS) -c gspher.s.f
gsubs.s.o: pi.par gsubs.s.f
	$(F77) $(FFLAGS) -c gsubs.s.f
gvert.s.o: pi.par gvert.s.f
	$(F77) $(FFLAGS) -c gvert.s.f
gvlim.s.o: pi.par gvlim.s.f
	$(F77) $(FFLAGS) -c gvlim.s.f
gvphi.s.o: pi.par gvphi.s.f
	$(F77) $(FFLAGS) -c gvphi.s.f
iylm.s.o: pi.par iylm.s.f
	$(F77) $(FFLAGS) -c iylm.s.f
pix2vec_nest.s.o: pix2vec_nest.s.f
	$(F77) $(FFLAGS) -c pix2vec_nest.s.f
twodf100k.o: pi.par twodf100k.f
	$(F77) $(FFLAGS) -c twodf100k.f
twodf230k.o: pi.par twodf230k.f
	$(F77) $(FFLAGS) -c twodf230k.f
twoqz.o: mangdir.data mangdir.inc twoqz.f
	$(F77) $(FFLAGS) -c twoqz.f
wlm.s.o: pi.par wlm.s.f
	$(F77) $(FFLAGS) -c wlm.s.f
wrho.s.o: pi.par wrho.s.f
	$(F77) $(FFLAGS) -c wrho.s.f
