#include <stdint.h>
#include <stdio.h>

int main() {
  int n = 92;
  int64_t prev = 0, curr = 1;

  printf("%ld\n%ld\n", prev, curr);

  for (int i = 2; i <= n; i++) {
    int64_t next = prev + curr;
    printf("%ld\n", next);
    prev = curr;
    curr = next;
  }
  return 0;
}
