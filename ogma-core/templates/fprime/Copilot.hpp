// ======================================================================
// \title  Copilot.hpp
// \author root
// \brief  hpp file for Copilot component implementation class
// ======================================================================

#ifndef Copilot_HPP
#define Copilot_HPP

#include "Ref/Copilot/CopilotComponentAc.hpp"

namespace Ref {

  class Copilot :
    public CopilotComponentBase
  {

    public:

      // ----------------------------------------------------------------------
      // Construction, initialization, and destruction
      // ----------------------------------------------------------------------

      //! Construct object Copilot
      //!
      Copilot(
          const char *const compName /*!< The component name*/
      );

      //! Initialize object Copilot
      //!
      void init(
          const NATIVE_INT_TYPE queueDepth, /*!< The queue depth*/
          const NATIVE_INT_TYPE instance = 0 /*!< The instance number*/
      );

      //! Destroy object Copilot
      //!
      ~Copilot();

    PRIVATE:

      // ----------------------------------------------------------------------
      // Handler implementations for user-defined typed input ports
      // ----------------------------------------------------------------------

{{{hdrHandlers}}}

    PRIVATE:

      // ----------------------------------------------------------------------
      // Command handler implementations
      // ----------------------------------------------------------------------

      //! Implementation for CHECK_MONITORS command handler
      //! 
      void CHECK_MONITORS_cmdHandler(
          const FwOpcodeType opCode, /*!< The opcode*/
          const U32 cmdSeq /*!< The command sequence number*/
      );

    };

} // end namespace Ref

#endif
