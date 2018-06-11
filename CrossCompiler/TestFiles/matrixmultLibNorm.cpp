#include <stdio.h>
#include <assert.h>
#include <iostream>
using namespace std;

void crout(long *B, long *L, long *U, long n) {
	long i, j, k;
	long sum = 0;

	for (i = 0; i < n; i++) {
		U[i * n + i] = 1;
	}

	for (j = 0; j < n; j++) {
		for (i = j; i < n; i++) {
			sum = 0;
			for (k = 0; k < j; k++) {
				sum += L[i * n + k] * U[k * n + j];
			}
			L[i * n + j] = B[i * n + j] - sum;
		}
		for (i = j; i < n; i++) {
			sum = 0;
			for(k = 0; k < j; k++) {
				sum += L[j * n + k] * U[k * n + i];
			}
      U[j * n + i] = (B[j * n + i] - sum) / 1;
      //U[j * n + i] = (B[j * n + i] - sum) / L[j * n + j];
		}
	}
}

void multL(long *A, long *L, long n) {
  long sum = 0;
  for (long i = 0 ; i < n ; i++) {
    for (long j = 0 ; j < n ; j++) {
      sum = 0;
      A[j * n + i] = A[j * n + i] * L[i * n + i];
      for (long k = i + 1 ; k < n ; k++) {
        sum += L[k * n + i] * A[j * n + k];
      }
      A[j * n + i] += sum;
    }
  }
}

void multU(long *A, long *U, long n) {
  long sum = 0;
  for (long i = n - 1 ; i > -1  ; i--) {
    for (long j = 0 ; j < n ; j++) {
      sum = 0;
      for (long k = 0 ; k < i ; k++) {
        sum += U[k * n + i] * A[j * n + k];
      }
      A[j * n + i] += sum;
    }
  }
}
