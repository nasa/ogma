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

{{#copilot}}
#include "{{{copilot.specName}}}_types.h"
#include "{{{copilot.specName}}}.h"
{{/copilot}}

#ifdef __cplusplus
}
#endif

{{#variables}}
{{varDeclType}} {{varDeclName}};
{{/variables}}
{{#monitors}}
bool {{monitorName}}_result;
{{/monitors}}

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

{{#variables}}
  void Copilot ::
    {{varDeclName}}In_handler(
        const NATIVE_INT_TYPE portNum,
        {{varDeclType}} value
    )
  {
    {{varDeclName}} = ({{varDeclType}}) value;
  }

{{/variables}}
  // ----------------------------------------------------------------------
  // Command handler implementations
  // ----------------------------------------------------------------------

  void Copilot ::
    CHECK_MONITORS_cmdHandler(
        const FwOpcodeType opCode,
        const U32 cmdSeq
    )
  {
{{#monitors}}
    {{monitorName}}_result = false;
{{/monitors}}
    step();
    this->cmdResponse_out(opCode,cmdSeq,Fw::CmdResponse::OK);
{{#monitors}}
    if ({{monitorName}}_result) {
       this->log_ACTIVITY_HI_{{monitorUC}}_VIOLATION();
    }
{{/monitors}}
  }

} // end namespace Ref
{{#monitors}}

{{#monitorType}}
void {{monitorName}}({{.}} arg) {
  {{monitorName}}_result = true;
}
{{/monitorType}}
{{^monitorType}}
void {{monitorName}}() {
  {{monitorName}}_result = true;
}
{{/monitorType}}
{{/monitors}}
