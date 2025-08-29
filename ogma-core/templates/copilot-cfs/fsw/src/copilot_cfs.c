/*******************************************************************************
** File: copilot_cfs.c
**
** Purpose:
**   This file contains the source code for the Copilot App.
**
*******************************************************************************/

/*
**   Include Files:
*/

#include "copilot_cfs.h"
#include "copilot_cfs_perfids.h"
#include "copilot_cfs_msgids.h"
#include "copilot_cfs_msg.h"
#include "copilot_cfs_events.h"
#include "copilot_cfs_version.h"
{{#impl_extra_header}}
{{{.}}}
{{/impl_extra_header}}
{{#copilot}}
#include "{{{copilot.specName}}}_types.h"
#include "{{{copilot.specName}}}.h"
#include "{{{copilot.specName}}}.c"
{{/copilot}}

{{#variables}}
{{varDeclType}} {{varDeclName}};
{{/variables}}

/*
** global data
*/

copilot_hk_tlm_t    COPILOT_HkTelemetryPkt;
CFE_SB_PipeId_t    COPILOT_CommandPipe;
CFE_SB_MsgPtr_t    COPILOTMsgPtr;

static CFE_EVS_BinFilter_t  COPILOT_EventFilters[] =
       {  /* Event ID    mask */
          {COPILOT_STARTUP_INF_EID,       0x0000},
          {COPILOT_COMMAND_ERR_EID,       0x0000},
          {COPILOT_COMMANDCPVIOL_INF_EID,    0x0000},
       };

/** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* COPILOT_AppMain() -- Application entry point and main process loop          */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * *  * * * * **/
void COPILOT_AppMain( void )
{
    int32  status;
    uint32 RunStatus = CFE_ES_APP_RUN;

    CFE_ES_PerfLogEntry(COPILOT_CFS_PERF_ID);

    COPILOT_AppInit();

    /*
    ** COPILOT Runloop
    */
    while (CFE_ES_RunLoop(&RunStatus) == TRUE)
    {
        CFE_ES_PerfLogExit(COPILOT_CFS_PERF_ID);

        /* Pend on receipt of command packet -- timeout set to 500 millisecs */
        status = CFE_SB_RcvMsg(&COPILOTMsgPtr, COPILOT_CommandPipe, 500);

        CFE_ES_PerfLogEntry(COPILOT_CFS_PERF_ID);

        if (status == CFE_SUCCESS)
        {
            COPILOT_ProcessCommandPacket();
        }

    }

    CFE_ES_ExitApp(RunStatus);

} /* End of COPILOT_AppMain() */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  */
/*                                                                            */
/* COPILOT_AppInit() --  initialization                                       */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
void COPILOT_AppInit(void)
{
    /*
    ** Register the app with Executive services
    */
    CFE_ES_RegisterApp() ;

    /*
    ** Register the events
    */
    CFE_EVS_Register(COPILOT_EventFilters,
                     sizeof(COPILOT_EventFilters)/sizeof(CFE_EVS_BinFilter_t),
                     CFE_EVS_BINARY_FILTER);

    /*
    ** Create the Software Bus command pipe and subscribe to housekeeping
    **  messages
    */
    CFE_SB_CreatePipe(&COPILOT_CommandPipe, COPILOT_PIPE_DEPTH,"COPILOT_CMD_PIPE");
    {{#msgIds}}
    CFE_SB_Subscribe({{.}}, COPILOT_CommandPipe);
    {{/msgIds}}


    CFE_EVS_SendEvent (COPILOT_STARTUP_INF_EID, CFE_EVS_INFORMATION,
               "COPILOT App Initialized. Version %d.%d.%d.%d",
                COPILOT_CFS_MAJOR_VERSION,
                COPILOT_CFS_MINOR_VERSION,
                COPILOT_CFS_REVISION,
                COPILOT_CFS_MISSION_REV);

} /* End of COPILOT_AppInit() */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
/*  Name:  COPILOT_ProcessCommandPacket                                        */
/*                                                                            */
/*  Purpose:                                                                  */
/*     This routine will process any packet that is received on the COPILOT    */
/*     command pipe.                                                          */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * *  * *  * * * * */
void COPILOT_ProcessCommandPacket(void)
{
    CFE_SB_MsgId_t  MsgId;

    MsgId = CFE_SB_GetMsgId(COPILOTMsgPtr);

    switch (MsgId)
    {
        {{#msgCases}}
        case {{msgInfoId}}:
            COPILOT_Process{{msgInfoDesc}}();
            break;

        {{/msgCases}}

        default:
            COPILOT_HkTelemetryPkt.copilot_command_error_count++;
            CFE_EVS_SendEvent(COPILOT_COMMAND_ERR_EID,CFE_EVS_ERROR,
              "COPILOT: invalid command packet,MID = 0x%x", MsgId);
            break;
    }

    return;

} /* End COPILOT_ProcessCommandPacket */

{{#msgHandlers}}
/**
* Make received data available to Copilot and run monitors.
*/
void COPILOT_Process{{msgDataDesc}}(void)
{
    {{#msgDataFromType}}
    {{msgDataFromType}}* msg;
    msg = ({{.}}*) COPILOTMsgPtr;
    {{/msgDataFromType}}
    {{^msgDataFromType}}
    {{msgDataVarType}}* msg;
    msg = ({{msgDataVarType}}*) COPILOTMsgPtr;
    {{/msgDataFromType}}
    {{#msgDataFromField}}
    {{msgDataVarName}} = msg->{{.}};
    {{/msgDataFromField}}
    {{^msgDataFromField}}
    {{msgDataVarName}} = *msg;
    {{/msgDataFromField}}

    // Run all copilot monitors.
    step();
}

{{/msgHandlers}}


{{#triggers}}
/**
 * Report copilot property violations.
 */
{{#triggerType}}
void {{triggerName}}({{.}} arg) {
{{/triggerType}}
{{^triggerType}}
void {{triggerName}}(void) {
{{/triggerType}}
    CFE_EVS_SendEvent(COPILOT_COMMANDCPVIOL_INF_EID, CFE_EVS_ERROR,
        "COPILOT: violation: {{triggerName}}");
}
{{/triggers}}
