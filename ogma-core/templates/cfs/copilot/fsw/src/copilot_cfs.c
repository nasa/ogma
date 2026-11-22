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
{{/copilot}}

{{#variables}}
{{varDeclType}} {{varDeclName}};
{{/variables}}

/*
** global data
*/

copilot_hk_tlm_t    COPILOT_HkTelemetryPkt;
CFE_SB_PipeId_t    COPILOT_CommandPipe;
CFE_MSG_Message_t *COPILOTMsgPtr;
CFE_SB_Buffer_t   *SBBufPtr;

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
    CFE_Status_t status;
    uint32 RunStatus = CFE_ES_RunStatus_APP_RUN;

    CFE_ES_PerfLogEntry(COPILOT_CFS_PERF_ID);

    status = COPILOT_AppInit();

    if (status != CFE_SUCCESS)
    {
        RunStatus = CFE_ES_RunStatus_APP_ERROR;
    }

    /*
    ** COPILOT Runloop
    */
    while (CFE_ES_RunLoop(&RunStatus) == true)
    {
        CFE_ES_PerfLogExit(COPILOT_CFS_PERF_ID);

        /* Pend on receipt of command packet -- timeout set to 500 millisecs */
        status = CFE_SB_ReceiveBuffer(&SBBufPtr, COPILOT_CommandPipe, 500);

        CFE_ES_PerfLogEntry(COPILOT_CFS_PERF_ID);

        if (status == CFE_SUCCESS)
        {
            COPILOT_ProcessCommandPacket();
        }

    }

    CFE_ES_PerfLogExit(COPILOT_CFS_PERF_ID);

    CFE_ES_ExitApp(RunStatus);

} /* End of COPILOT_AppMain() */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  */
/*                                                                            */
/* COPILOT_AppInit() --  initialization                                       */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
CFE_Status_t COPILOT_AppInit(void)
{
    CFE_Status_t status;

    /*
    ** Register the events
    */
    status = CFE_EVS_Register(COPILOT_EventFilters,
                     sizeof(COPILOT_EventFilters)/sizeof(CFE_EVS_BinFilter_t),
                     CFE_EVS_EventFilter_BINARY);

    if (status != CFE_SUCCESS)
    {
        CFE_ES_WriteToSysLog("Copilot App: Error Registering Events, RC = 0x%08lX\n", (unsigned long)status);
    }
    else
    {
        /*
        ** Create the Software Bus command pipe and subscribe to housekeeping
        **  messages
        */
        status = CFE_SB_CreatePipe(&COPILOT_CommandPipe, COPILOT_PIPE_DEPTH,"COPILOT_CMD_PIPE");

        if (status != CFE_SUCCESS)
        {
            CFE_EVS_SendEvent(COPILOT_CR_PIPE_ERR_EID,
                              CFE_EVS_EventType_ERROR,
                              "Copilot App: Error creating SB Command Pipe, RC = 0x%08lX",
                              (unsigned long)status);
        }
    }

    {{#msgIds}}
    if (status == CFE_SUCCESS)
    {
        status = CFE_SB_Subscribe(CFE_SB_ValueToMsgId({{.}}), COPILOT_CommandPipe);

        if (status != CFE_SUCCESS)
        {
            CFE_EVS_SendEvent(COPILOT_CR_SUB_ERR_EID,
                              CFE_EVS_EventType_ERROR,
                              "Copilot App: Error subscribing to necessary messages, RC = 0x%08lX",
                              (unsigned long)status);
        }
    }
    {{/msgIds}}

    if (status == CFE_SUCCESS)
    {
        status = CFE_SB_Subscribe(CFE_SB_ValueToMsgId(COPILOT_CFS_REEVAL_CMD_MID), COPILOT_CommandPipe);

        if (status != CFE_SUCCESS)
        {
            CFE_EVS_SendEvent(COPILOT_CR_SUB_ERR_EID,
                              CFE_EVS_EventType_ERROR,
                              "Copilot App: Error subscribing to necessary messages, RC = 0x%08lX",
                              (unsigned long)status);
        }
    }

    CFE_EVS_SendEvent (COPILOT_STARTUP_INF_EID, CFE_EVS_EventType_INFORMATION,
               "COPILOT App Initialized. Version %d.%d.%d.%d",
                COPILOT_CFS_MAJOR_VERSION,
                COPILOT_CFS_MINOR_VERSION,
                COPILOT_CFS_REVISION,
                COPILOT_CFS_MISSION_REV);

    return status;

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
    {{#msgCases}}
    static CFE_SB_MsgId_t {{msgInfoId}}_s = CFE_SB_MSGID_RESERVED;
    {{/msgCases}}

    static CFE_SB_MsgId_t COPILOT_CFS_REEVAL_CMD_MID_s = CFE_SB_MSGID_RESERVED;

    CFE_SB_MsgId_t MsgId = CFE_SB_INVALID_MSG_ID;

    if (!CFE_SB_IsValidMsgId(COPILOT_CFS_REEVAL_CMD_MID_s))
    {
        COPILOT_CFS_REEVAL_CMD_MID_s = CFE_SB_ValueToMsgId(COPILOT_CFS_REEVAL_CMD_MID);
        {{#msgCases}}
        {{msgInfoId}}_s = CFE_SB_ValueToMsgId({{msgInfoId}});
        {{/msgCases}}
    }

    CFE_MSG_GetMsgId(&SBBufPtr->Msg, &MsgId);

    if (CFE_SB_MsgId_Equal(MsgId, COPILOT_CFS_REEVAL_CMD_MID_s))
    {
        copilot_step();
    }
    {{#msgCases}}
    else if (CFE_SB_MsgId_Equal(MsgId, {{msgInfoId}}_s))
    {
        COPILOTMsgPtr = &SBBufPtr->Msg;
        COPILOT_Process{{msgInfoDesc}}();
    }
    {{/msgCases}}
    else
    {
        COPILOT_HkTelemetryPkt.copilot_command_error_count++;
        CFE_EVS_SendEvent(COPILOT_COMMAND_ERR_EID,CFE_EVS_EventType_ERROR,
          "COPILOT: invalid command packet,MID = 0x%x", CFE_SB_MsgIdToValue(MsgId));
    }

} /* End COPILOT_ProcessCommandPacket */

{{#msgHandlers}}
/**
* Make received data available to Copilot and run monitors.
*/
void COPILOT_Process{{msgDataDesc}}(void)
{
    {{msgDataType}}* msg;
    msg = ({{msgDataType}}*) COPILOTMsgPtr;
    {{#msgDataContents}}
    {{#msgDataFromField}}
    {{msgDataVarName}} = msg->{{.}};
    {{/msgDataFromField}}
    {{^msgDataFromField}}
    {{msgDataVarName}} = *msg;
    {{/msgDataFromField}}
    {{/msgDataContents}}

    {{#msgDataActive}}
    // Run all copilot monitors.
    copilot_step();
    {{/msgDataActive}}
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
    CFE_EVS_SendEvent(COPILOT_COMMANDCPVIOL_INF_EID, CFE_EVS_EventType_ERROR,
        "COPILOT: violation: {{triggerName}}");
}
{{/triggers}}
