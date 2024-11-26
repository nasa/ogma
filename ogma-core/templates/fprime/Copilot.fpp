module Ref {

{{{ifaceTypePorts}}}

  @ Monitoring component
  queued component Copilot {

    # ----------------------------------------------------------------------
    # General ports
    # ----------------------------------------------------------------------

{{{ifaceInputPorts}}}

    # ----------------------------------------------------------------------
    # Special ports
    # ----------------------------------------------------------------------

    @ Command receive
    command recv port cmdIn

    @ Command registration
    command reg port cmdRegOut

    @ Command response
    command resp port cmdResponseOut

    @ Event
    event port eventOut

    @ Parameter get
    param get port prmGetOut

    @ Parameter set
    param set port prmSetOut

    @ Telemetry
    telemetry port tlmOut

    @ Text event
    text event port textEventOut

    @ Time get
    time get port timeGetOut

    # ----------------------------------------------------------------------
    # Parameters
    # ----------------------------------------------------------------------

    # This section intentionally left blank

    # ----------------------------------------------------------------------
    # Events
    # ----------------------------------------------------------------------

{{{ifaceViolationEvents}}}

    # ----------------------------------------------------------------------
    # Commands
    # ----------------------------------------------------------------------

    sync command CHECK_MONITORS()

    # ----------------------------------------------------------------------
    # Telemetry
    # ----------------------------------------------------------------------

    # This section intentionally left blank

  }

}
