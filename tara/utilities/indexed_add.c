#include <stdio.h>
#include "idl_export.h"   /* IDL external definitions */


/* 
The best way to compile is probably to use the IDL utility make_dll, e.g.
   
  make_dll, 'indexed_add', 'indexed_add', ['indexed_add_double'], COMPILE_DIRECTORY='.', /VER, /SHOW

If your IDL installation wants 32-bit code and your C compiler defaults to 64-bit code, then add these options:
  EXTRA_CFLAGS='-m32', EXTRA_LFLAGS='-m32' 

The tara_install tool will perform this compilation for you.  
  
  
EXAMPLE USAGE: 

  vector    = dblarr(10)
  index     = [1,3,5,7L]
  increment = [1,2,3,4D]
  num_increments = n_elements(index)

  dummy = CALL_EXTERNAL( 'indexed_add.so', 'indexed_add_double', vector, index, increment, num_increments )
  print, vector
  
   VECTOR (long/float/double) is an initialized array.  
   INDEX (long) and INCREMENT (long/float/double) are arrays of length NUM_INCREMENTS (long).  
   The elements of VECTOR indexed by INDEX are incremented by INCREMENT.
   
   NOTE: The CALLER is responsible for making sure that the values in
   the array INDEX do not violate the bounds of the array VECTOR.


   To manually compile on a 32-bit Linux system you might try this:
        gcc -I/usr/local/rsi/idl/external/include -c  -fPIC -g -Wall indexed_add.c
        ld -shared -o indexed_add.so indexed_add.o
*/

IDL_LONG indexed_add_long( int argc, void *argv[] )
{
  IDL_LONG   *vector;
  IDL_LONG   *index;
  IDL_LONG   *increment;
  IDL_LONG   *num_increments;
  int     ii;

  vector         = (IDL_LONG   *)argv[0];
  index          = (IDL_LONG   *)argv[1];
  increment      = (IDL_LONG   *)argv[2];
  num_increments = (IDL_LONG   *)argv[3];

  for (ii=0; ii < *num_increments; ii++)
  {
    vector[ index[ii] ] += increment[ii];
  }
  
  return(0);
}

IDL_LONG indexed_add_float( int argc, void *argv[] )
{
  float  *vector;
  IDL_LONG   *index;
  float  *increment;
  IDL_LONG   *num_increments;
  int     ii;

  vector         = (float  *)argv[0];
  index          = (IDL_LONG   *)argv[1];
  increment      = (float  *)argv[2];
  num_increments = (IDL_LONG   *)argv[3];

  for (ii=0; ii < *num_increments; ii++)
  {
    vector[ index[ii] ] += increment[ii];
  }
  
  return(0);
}

IDL_LONG indexed_add_double( int argc, void *argv[] )
{
  double *vector;
  IDL_LONG   *index;
  double *increment;
  IDL_LONG   *num_increments;
  int     ii;

  vector         = (double *)argv[0];
  index          = (IDL_LONG   *)argv[1];
  increment      = (double *)argv[2];
  num_increments = (IDL_LONG   *)argv[3];

  for (ii=0; ii < *num_increments; ii++)
  {
    vector[ index[ii] ] += increment[ii];
  }
  
  return(0);
}

