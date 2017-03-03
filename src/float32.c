#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <stdio.h>
#include <math.h>

void hexOut(unsigned char c) {
	printf("%x", c & 0xF);
	printf("%x", (c >> 4) & 0xF);
}

void displayInHex(void * block, int size) {
	unsigned char *tmp = (unsigned char *) block;
	int i;
	for (i = size - 1; i >= 0; i--) {
		hexOut(*(tmp + i));
		if (i % 2 == 0)
			printf(" ");
	}
	printf("\n");
}

value copy_float (float f) {
	value float32 = caml_alloc(1, Abstract_tag);
	*((float *)&Field(float32, 0)) = f;
	return float32;
}

float Float_val (value v) {
	return *((float *) &Field(v, 0));
}

float float_of_bits(int32_t i) {
  union { float f; int32_t i; } u;
  u.i = i;
  return u.f;
}

value float32_of_int32 (value v) {
  float f = float_of_bits(Int32_val(v));
	return copy_float(f);
}

value float32_to_double (value v) {
	return copy_double((double) Float_val(v));
}

value float32_zero () {
	return copy_float(0);
}

value float32_one () {
	return copy_float(1);
}

value float32_add (value v1, value v2) {
	float val1 = Float_val(v1);
	float val2 = Float_val(v2);
	return copy_float(val1 + val2);
}

value float32_sub (value v1, value v2) {
	float val1 = Float_val(v1);
	float val2 = Float_val(v2);
	return copy_float(val1 - val2);
}

value float32_mul (value v1, value v2) {
	float val1 = Float_val(v1);
	float val2 = Float_val(v2);
	return copy_float(val1 * val2);
}

value float32_div (value v1, value v2) {
	float val1 = Float_val(v1);
	float val2 = Float_val(v2);
	return copy_float(val1 / val2);
}

value float32_rem (value v1, value v2) {
	float val1 = Float_val(v1);
	float val2 = Float_val(v2);

	return copy_float(fmodf(val1,val2));
}

value float32_equal (value v1, value v2) {
	float val1 = Float_val(v1);
	float val2 = Float_val(v2);
	return Bool_val(val1==val2);
}

value float32_neg (value v) {
	return copy_float(- Float_val(v));
}
