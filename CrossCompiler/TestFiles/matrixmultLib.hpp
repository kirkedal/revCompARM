#include <stdio.h>
#include <assert.h>
void crout_forward(long  *LDU, long  &n);
void crout_backwards(long  *LDU, long  &n);

void multLD_forward(long  *A, long  *LDU, long  &n);
void multLD_backwards(long  *A, long  *LDU, long  &n);

void multU_forward(long  *A, long  *LDU, long  &n);
void multU_backwards(long  *A, long  *LDU, long  &n);

void mult_forward(long  &x, long  &y);
void mult_backwards(long  &x, long  &y);
