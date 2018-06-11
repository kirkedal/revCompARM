#include <cstdlib>
#include "matrixmultLib.hpp"

int main(int argc, char* argv[]) {

long dimension = atoi(argv[1]);
long elements = dimension * dimension;
long* B = (long*) std::calloc(elements, sizeof(long));
long* A = (long*) std::calloc(elements, sizeof(long));
for (int i = 1 ; i < elements+1 ; i++) {
  A[i-1] = 1;
  B[i-1] = 1;
}
long  n = 0;
n += dimension;

crout_forward(B, n);
multLD_forward(A, B, n);
multU_forward(A, B, n);

free(B);
free(A);

return 0;
}
