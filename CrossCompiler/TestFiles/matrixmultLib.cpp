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

void crout_forward(long  *LDU, long  &n) {
for (long j = 0 ; j != n - 1 + 1 ; j += 1)
{
for (long i = j ; i != n - 1 + 1 ; i += 1)
{
for (long k = 0 ; k != j - 1 + 1 ; k += 1)
{
if(k == j) {
LDU[i * n + j] -= LDU[i * n + k];
assert(k == j);
} else {
LDU[i * n + j] -= LDU[i * n + k] * LDU[k * n + j];
assert(!(k == j));
}
}
}
for (long i = j + 1 ; i != n - 1 + 1 ; i += 1)
{
for (long k = 0 ; k != j - 1 + 1 ; k += 1)
{
if(k == i) {
LDU[j * n + j] -= LDU[j * n + k];
assert(k == j);
} else {
LDU[j * n + i] -= LDU[j * n + k] * LDU[k * n + i];
assert(!(k == j));
}
}
mult_backwards(LDU[j * n + i], LDU[j * n + j]);
}
}
}

void crout_backwards(long  *LDU, long  &n) {
for (long j = n - 1 ; j != 0 + 0 - 1 ; j += 0 - 1)
{
for (long i = n - 1 ; i != j + 1 + 0 - 1 ; i += 0 - 1)
{
mult_forward(LDU[j * n + i], LDU[j * n + j]);
for (long k = j - 1 ; k != 0 + 0 - 1 ; k += 0 - 1)
{
if(k == j) {
LDU[j * n + j] += LDU[j * n + k];
assert(k == i);
} else {
LDU[j * n + i] += LDU[j * n + k] * LDU[k * n + i];
assert(!(k == i));
}
}
}
for (long i = n - 1 ; i != j + 0 - 1 ; i += 0 - 1)
{
for (long k = j - 1 ; k != 0 + 0 - 1 ; k += 0 - 1)
{
if(k == j) {
LDU[i * n + j] += LDU[i * n + k];
assert(k == j);
} else {
LDU[i * n + j] += LDU[i * n + k] * LDU[k * n + j];
assert(!(k == j));
}
}
}
}
}

void multLD_forward(long  *A, long  *LDU, long  &n) {
for (long i = 0 ; i != n - 1 + 1 ; i += 1)
{
for (long j = 0 ; j != n - 1 + 1 ; j += 1)
{
mult_forward(A[j * n + i], LDU[i * n + i]);
for (long k = i + 1 ; k != n - 1 + 1 ; k += 1)
{
A[j * n + i] += LDU[k * n + i] * A[j * n + k];
}
}
}
}

void multLD_backwards(long  *A, long  *LDU, long  &n) {
for (long i = n - 1 ; i != 0 + 0 - 1 ; i += 0 - 1)
{
for (long j = n - 1 ; j != 0 + 0 - 1 ; j += 0 - 1)
{
for (long k = n - 1 ; k != i + 1 + 0 - 1 ; k += 0 - 1)
{
A[j * n + i] -= LDU[k * n + i] * A[j * n + k];
}
mult_backwards(A[j * n + i], LDU[i * n + i]);
}
}
}

void multU_forward(long  *A, long  *LDU, long  &n) {
for (long i = n - 1 ; i != 0 + -1 ; i += -1)
{
for (long j = 0 ; j != n - 1 + 1 ; j += 1)
{
for (long k = 0 ; k != i - 1 + 1 ; k += 1)
{
A[j * n + i] += LDU[k * n + i] * A[j * n + k];
}
}
}
}

void multU_backwards(long  *A, long  *LDU, long  &n) {
for (long i = 0 ; i != n - 1 + 0 - -1 ; i += 0 - -1)
{
for (long j = n - 1 ; j != 0 + 0 - 1 ; j += 0 - 1)
{
for (long k = i - 1 ; k != 0 + 0 - 1 ; k += 0 - 1)
{
A[j * n + i] -= LDU[k * n + i] * A[j * n + k];
}
}
}
}

void mult_forward(long  &x, long  &y) {
long t = x;
x += t * y - t;
//assert(t == x / y);
}

void mult_backwards(long  &x, long  &y) {
//long t = x / y;
long t = x / 1;
x -= t * y - t;
//assert(t == x);
}
