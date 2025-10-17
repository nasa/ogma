/************************************************************************
 * NASA Docket No. GSC-18,719-1, and identified as “core Flight System: Bootes”
 *
 * Copyright (c) 2020 United States Government as represented by the
 * Administrator of the National Aeronautics and Space Administration.
 * All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ************************************************************************/

/**
 * \file
 *   This file contains the source code for the Sample App.
 */

/*
** Include Files:
*/
#include "{{app_name_lc}}.h"
#include "{{app_name_lc}}_dispatch.h"
#include "{{app_name_lc}}_cmds.h"
#include "{{app_name_lc}}_eventids.h"
#include "{{app_name_lc}}_msgids.h"
#include "{{app_name_lc}}_msg.h"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
/*                                                                            */
/* Verify command packet length                                               */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
bool {{app_name_uc}}_VerifyCmdLength(const CFE_MSG_Message_t *MsgPtr, size_t ExpectedLength)
{
    bool              result       = true;
    size_t            ActualLength = 0;
    CFE_SB_MsgId_t    MsgId        = CFE_SB_INVALID_MSG_ID;
    CFE_MSG_FcnCode_t FcnCode      = 0;

    CFE_MSG_GetSize(MsgPtr, &ActualLength);

    /*
    ** Verify the command packet length.
    */
    if (ExpectedLength != ActualLength)
    {
        CFE_MSG_GetMsgId(MsgPtr, &MsgId);
        CFE_MSG_GetFcnCode(MsgPtr, &FcnCode);

        CFE_EVS_SendEvent({{app_name_uc}}_CMD_LEN_ERR_EID, CFE_EVS_EventType_ERROR,
                          "Invalid Msg length: ID = 0x%X,  CC = %u, Len = %u, Expected = %u",
                          (unsigned int)CFE_SB_MsgIdToValue(MsgId), (unsigned int)FcnCode, (unsigned int)ActualLength,
                          (unsigned int)ExpectedLength);

        result = false;

        {{app_name_uc}}_Data.ErrCounter++;
    }

    return result;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
/*                                                                            */
/* SAMPLE ground commands                                                     */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
void {{app_name_uc}}_ProcessGroundCommand(const CFE_SB_Buffer_t *SBBufPtr)
{
    CFE_MSG_FcnCode_t CommandCode = 0;

    CFE_MSG_GetFcnCode(&SBBufPtr->Msg, &CommandCode);

    /*
    ** Process SAMPLE app ground commands
    */
    switch (CommandCode)
    {
        case {{app_name_uc}}_NOOP_CC:
            if ({{app_name_uc}}_VerifyCmdLength(&SBBufPtr->Msg, sizeof({{app_name_uc}}_NoopCmd_t)))
            {
                {{app_name_uc}}_NoopCmd((const {{app_name_uc}}_NoopCmd_t *)SBBufPtr);
            }
            break;

        case {{app_name_uc}}_RESET_COUNTERS_CC:
            if ({{app_name_uc}}_VerifyCmdLength(&SBBufPtr->Msg, sizeof({{app_name_uc}}_ResetCountersCmd_t)))
            {
                {{app_name_uc}}_ResetCountersCmd((const {{app_name_uc}}_ResetCountersCmd_t *)SBBufPtr);
            }
            break;

        case {{app_name_uc}}_PROCESS_CC:
            if ({{app_name_uc}}_VerifyCmdLength(&SBBufPtr->Msg, sizeof({{app_name_uc}}_ProcessCmd_t)))
            {
                {{app_name_uc}}_ProcessCmd((const {{app_name_uc}}_ProcessCmd_t *)SBBufPtr);
            }
            break;

        case {{app_name_uc}}_DISPLAY_PARAM_CC:
            if ({{app_name_uc}}_VerifyCmdLength(&SBBufPtr->Msg, sizeof({{app_name_uc}}_DisplayParamCmd_t)))
            {
                {{app_name_uc}}_DisplayParamCmd((const {{app_name_uc}}_DisplayParamCmd_t *)SBBufPtr);
            }
            break;

        /* default case already found during FC vs length test */
        default:
            CFE_EVS_SendEvent({{app_name_uc}}_CC_ERR_EID, CFE_EVS_EventType_ERROR, "Invalid ground command code: CC = %d",
                              CommandCode);
            break;
    }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
/*                                                                            */
/*  Purpose:                                                                  */
/*     This routine will process any packet that is received on the SAMPLE    */
/*     command pipe.                                                          */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * *  * *  * * * * */
void {{app_name_uc}}_TaskPipe(const CFE_SB_Buffer_t *SBBufPtr)
{
    CFE_SB_MsgId_t MsgId = CFE_SB_INVALID_MSG_ID;

    CFE_MSG_GetMsgId(&SBBufPtr->Msg, &MsgId);

    switch (CFE_SB_MsgIdToValue(MsgId))
    {
        case {{app_name_uc}}_CMD_MID:
            {{app_name_uc}}_ProcessGroundCommand(SBBufPtr);
            break;

        case {{app_name_uc}}_SEND_HK_MID:
            {{app_name_uc}}_SendHkCmd((const {{app_name_uc}}_SendHkCmd_t *)SBBufPtr);
            break;

        {{#msgCases}}
        case {{msgInfoId}}:
            COPILOT_Process{{msgInfoDesc}}();
            break;

        {{/msgCases}}
        default:
            CFE_EVS_SendEvent({{app_name_uc}}_MID_ERR_EID, CFE_EVS_EventType_ERROR,
                              "SAMPLE: invalid command packet,MID = 0x%x", (unsigned int)CFE_SB_MsgIdToValue(MsgId));
            break;
    }
}

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
    copilot_step();
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
