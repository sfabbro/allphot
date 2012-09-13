#include <fitsio.h>
#include <math.h>
#include <string.h>

typedef struct {
  float *data;
  float minval;
  float scale;
  int nbins;
} histo;

/* constructor */
histo* histo_init(int n, float minv, float maxv) {
  histo* hist = malloc(sizeof(histo));
  hist->nbins = n;
  hist->minval = minv;
  hist->scale = n / (maxv-minv);
  hist->data = malloc(n*sizeof(float));
  memset(hist->data, 0 , n*sizeof(float));
  return hist;
}

/* fill histogram with a value */
histo* histo_fill(histo* hist, float val) {
  int bin = (int)(floorf((val - hist->minval)*hist->scale));
  if (bin>0 && bin<hist->nbins)
    hist->data[bin] += 1;
}
  
/* return max value of histogram and its bin */
float histo_max(histo* hist, float* maxbin) {
  float *p = hist->data;
  int i,imax= 0 ;
  float maxval = -1e30;
  for (i=0 ; i<hist->nbins ; i++, p++)
    if (*p > maxval) { maxval = *p; imax = i; }
  *maxbin = hist->minval + ((float)imax)/hist->scale;
  return maxval;
}

/* zero out bins between minv and maxv */
void histo_zero(histo* hist, float minv, float maxv) {
  int minbin = (int)(floorf((minv - hist->minval)*hist->scale));
  int maxbin = (int)(floorf((maxv - hist->minval)*hist->scale));
  int bin;
  if (minbin<0 || maxbin>=hist->nbins) return;
  for (bin = minbin; bin < maxbin; bin++)
    hist->data[bin] = 0;
}

/* returns a guessed saturation level from data */  
float guess_saturation(float* data, long ndata) {
  
  float maxval = -1e-30;
  float *p;
  int i;
  long l;
  float frac = 0.05;
  int nval = (int) (1./ frac);
  float x, y, maxbin;
  float satur, cursatur;
  int count=1, n=0;
  histo* hist;
  
  /* create histogram */
  for (l=0, p=&data[0]; l<ndata; ++l, ++p)
    if (*p > maxval) maxval = *p;

  hist = histo_init((int)(maxval/500.), 0, maxval+1);

  /* fill it up with pixel values */
  for (l=0, p=&data[0]; l<ndata; ++l)
    histo_fill(hist, *p++);
  
  y = histo_max(hist, &maxbin);
  x = maxbin;
  satur = maxval;
  n = 0;

  for (l=0; l<nval; l++) {
    cursatur = x;
    /* zero out current explored bin */
    histo_zero(hist, maxval*0.05*l, maxval*0.05*(l+1));
    /* find max of zeroed-out histogram */
    y = histo_max(hist, &x);
    if (x == cursatur) {
      count++;
      if (count > n && cursatur != maxbin) {
	n = count;
	satur = cursatur;
      }
    } else
      count = 1;
  }

  free(hist->data);
  free(hist);

  return satur;
}

int fits_get_satur(fitsfile* fptr, float* satur, int* status) {
  float *pixels;
  long naxes[2], first[2] = {1,1};
  long npix;
  int naxis;

  /* bulletproof */
  if (!fptr || *status > 0) return *status;

  fits_get_img_dim(fptr, &naxis,  status);
  if (naxis<1) { *satur = 0; return *status; }

  fits_get_img_size(fptr, 2, naxes, status);
  npix = naxes[0]*naxes[1];
  pixels = (float*) calloc(npix, sizeof(float));
  fits_read_pix(fptr, TFLOAT, first, npix, NULL, pixels, NULL, status);  
  *satur = guess_saturation(pixels, npix);
  free(pixels);
  return *status;  
}
