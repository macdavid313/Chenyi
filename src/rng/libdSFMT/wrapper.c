/* Wrapper */
#include <dSFMT.h>

extern int DSFMT_MEXP_WRAP = DSFMT_MEXP;
extern int DSFMT_N_WRAP = DSFMT_N;
extern int DSFMT_N32_WRAP = DSFMT_N32;
extern int DSFMT_N64_WRAP = DSFMT_N64;

static inline dsfmt_t* dsfmt_get_global_data() {
  return &dsfmt_global_data;
}

void* dsfmt_get_global_data_wrap() {
  return dsfmt_get_global_data();
}

uint32_t dsfmt_genrand_uint32_wrap (dsfmt_t* dsfmt) {
  return dsfmt_genrand_uint32(dsfmt);
}

double dsfmt_genrand_close1_open2_wrap (dsfmt_t* dsfmt) {
  return dsfmt_genrand_close1_open2(dsfmt);
}

double dsfmt_genrand_close_open_wrap (dsfmt_t* dsfmt) {
  return dsfmt_genrand_close_open(dsfmt);
}

double dsfmt_genrand_open_close_wrap (dsfmt_t* dsfmt) {
  return dsfmt_genrand_open_close(dsfmt);
}

double dsfmt_genrand_open_open_wrap (dsfmt_t* dsfmt) {
  return dsfmt_genrand_open_open(dsfmt);
}

uint32_t dsfmt_gv_genrand_uint32_wrap () {
  return dsfmt_gv_genrand_uint32();
}

double dsfmt_gv_genrand_close1_open2_wrap () {
  return dsfmt_gv_genrand_close1_open2();
}

double dsfmt_gv_genrand_close_open_wrap () {
  return dsfmt_gv_genrand_close_open();
}

double dsfmt_gv_genrand_open_close_wrap()
{
  return dsfmt_gv_genrand_open_close();
}

double dsfmt_gv_genrand_open_open_wrap () {
  return dsfmt_gv_genrand_open_open();
}

void dsfmt_gv_fill_array_open_close_wrap (double* array, int size) {
  return dsfmt_gv_fill_array_open_close(array, size);
}

void dsfmt_gv_fill_array_close_open_wrap (double* array, int size) {
  return dsfmt_gv_fill_array_close_open(array, size);
}

void dsfmt_gv_fill_array_open_open_wrap (double* array, int size) {
  return dsfmt_gv_fill_array_open_open(array, size);
}

void dsfmt_gv_fill_array_close1_open2_wrap(double* array, int size)
{
  return dsfmt_gv_fill_array_close1_open2(array, size);
}

void dsfmt_gv_init_gen_rand_wrap (uint32_t seed) {
  return dsfmt_gv_init_gen_rand(seed);
}

void dsfmt_gv_init_by_array_wrap (uint32_t* init_key, int key_length) {
  return dsfmt_gv_init_by_array(init_key, key_length);
}

void dsfmt_init_gen_rand_wrap (dsfmt_t* dsfmt, uint32_t seed) {
  return dsfmt_init_gen_rand(dsfmt, seed);
}

void dsfmt_init_by_array_wrap (dsfmt_t* dsfmt, uint32_t* init_key, int key_length) {
  return dsfmt_init_by_array(dsfmt, init_key, key_length);
}
