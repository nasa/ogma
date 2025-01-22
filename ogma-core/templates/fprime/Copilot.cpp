// ======================================================================
// \title  Copilot.cpp
// \author Ogma
// \brief  cpp file for Copilot component implementation class
// ======================================================================


#include <Ref/Copilot/Copilot.hpp>
#include "Fw/Types/BasicTypes.hpp"

#ifdef __cplusplus
extern "C" {
#endif

#include "copilot.h"
#include "copilot_types.h"

#ifdef __cplusplus
}
#endif

{{{implInputs}}}
{{{implMonitorResults}}}

namespace Ref {

  // ----------------------------------------------------------------------
  // Construction, initialization, and destruction
  // ----------------------------------------------------------------------

  Copilot ::
    Copilot(
        const char *const compName
    ) : CopilotComponentBase(compName)
  {

  }

  void Copilot ::
    init(
        const NATIVE_INT_TYPE queueDepth,
        const NATIVE_INT_TYPE instance
    )
  {
    CopilotComponentBase::init(queueDepth, instance);
  }

  Copilot ::
    ~Copilot()
  {

  }

  // ----------------------------------------------------------------------
  // Handler implementations for user-defined typed input ports
  // ----------------------------------------------------------------------

{{{implInputHandlers}}}

  // ----------------------------------------------------------------------
  // Command handler implementations
  // ----------------------------------------------------------------------

  void Copilot ::
    CHECK_MONITORS_cmdHandler(
        const FwOpcodeType opCode,
        const U32 cmdSeq
    )
  {
{{{implTriggerResultReset}}}
    step();
    this->cmdResponse_out(opCode,cmdSeq,Fw::CmdResponse::OK);
{{{implTriggerChecks}}}
  }

} // end namespace Ref

{{{implTriggers}}}
