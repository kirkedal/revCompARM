#include <stdio.h>
#include <assert.h>
#include "TestIter.hpp"
#include <iostream>
using namespace std;


int main() {
int a = 0;
a += 10;
cout << a << "\n";
iter_forward(a);
cout << a << "\n";
iter2_forward(a);
cout << a << "\n";
iter_backwards(a);
cout << a << "\n";
iter2_backwards(a);
cout << a << "\n";
return 0;
}
