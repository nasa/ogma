#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "monitor.h"

uint8_t  input     = 150;
uint8_t  state     = 0;
uint64_t iteration = 0;

/**
 * Function called by the state machine to communicate its expected state.
 *
 * @param handler_arg0 The new state of the state machine.
 * @param handler_arg1 The previous state of the state machine.
 * @param handler_arg2 The input received that caused the transition.
 */
void handler(uint8_t handler_arg0, uint8_t handler_arg1, uint8_t handler_arg2)
{
  printf("[Calculated] Previous state: %d, input: %d, new state: %d\n",
         handler_arg1, handler_arg2, handler_arg0);

  iteration++;

  state = handler_arg0;
}

/**
 * Report the current, internally calculated input and state, and call the
 * state machine to calculate the new state. The function <code>step</code>
 * calls <code>handler</code> at each step.
 */
void next()
{
  printf("Executing step %ld: input %d, state %d\n", iteration, input, state);
  step();
  printf("--------------------------------------\n");
}

void main (int argc, char **argv)
{
  // We step with initial state 0 and input 150. The machine should remain in
  // state 0.
  next();

  // We step again with input greater than 180. The machine should now
  // transition to state 1.
  input = 185;
  next();

  // We step again with no changes. The machine should remain in state 1.
  next();

  // We step again with input lower than or equal to 180. The machine should
  // now transition to state 0.
  input = 110;
  next();

  // We step again with no changes. The machine should transition to state 2.
  next();

  // We step again with no changes. The machine should remain in state 2.
  next();

  // We step again with an input greater than 180. The machine should now
  // transition to state 0.
  input = 185;
  next();
}
