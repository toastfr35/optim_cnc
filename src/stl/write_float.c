#include <stdio.h>
#include <stdlib.h>

FILE * f;

void create_stl (char * filename)
{
  unsigned char bytes[84] = {0};
  f = fopen (filename, "wb");
  fwrite (bytes, 1, 84, f);
}

void write_float (float v)
{
  fwrite (&v, 4, 1, f);
}

void write_0_byte (void)
{
  unsigned char z = 0;
  fwrite (&z, 1, 1, f);
}


void close_stl (void)
{
  int pos = ftell (f);
  unsigned int nb = (pos-84)/50;
  fseek (f, 80, SEEK_SET);
  fwrite (&nb, 4, 1, f);
  fclose (f);
}
