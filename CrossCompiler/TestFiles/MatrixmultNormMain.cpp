#include <cstdlib>
#include "matrixmultLibNorm.hpp"

int main(int argc, char* argv[]) {

long dimension = atoi(argv[1]);
long elements = dimension * dimension;
long* B = (long*) std::calloc(elements, sizeof(long));
long* A = (long*) std::calloc(elements, sizeof(long));
long* L = (long*) std::calloc(elements, sizeof(long));
long* U = (long*) std::calloc(elements, sizeof(long));

for (int i = 1 ; i < elements+1 ; i++) {
  A[i-1] = 1;
  B[i-1] = 1;
}
long  n = 0;
n += dimension;

crout(B, L, U, n);
multL(A, L, n);
multU(A, U, n);

free(B);
free(A);
free(L);
free(U);

return 0;
}
