#include <stdio.h>

double input_signal;
void step(void);

void handlerpropTestCopilot_001(void) {
  printf("Monitor condition violated\n");
}

int main (int argc, char** argv) {
  int i = 0;

  input_signal = 0;

  for (i=0; i<10; i++) {
    printf("Running step %d\n", i);
    input_signal += 1;
    step();
  }
  return 0;
}
