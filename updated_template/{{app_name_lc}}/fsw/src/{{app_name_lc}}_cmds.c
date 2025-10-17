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
 *   This file contains the source code for the Sample App Ground Command-handling functions
 */

/*
** Include Files:
*/
#include "{{app_name_lc}}.h"
#include "{{app_name_lc}}_cmds.h"
#include "{{app_name_lc}}_msgids.h"
#include "{{app_name_lc}}_eventids.h"
#include "{{app_name_lc}}_version.h"
#include "{{app_name_lc}}_tbl.h"
#include "{{app_name_lc}}_utils.h"
#include "{{app_name_lc}}_msg.h"

/* The sample_lib module provides the SAMPLE_Function() prototype */
#include "sample_lib.h"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
/*                                                                            */
/*  Purpose:                                                                  */
/*         This function is triggered in response to a task telemetry request */
/*         from the housekeeping task. This function will gather the Apps     */
/*         telemetry, packetize it and send it to the housekeeping task via   */
/*         the software bus                                                   */
/* * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * *  * *  * * * * */
CFE_Status_t {{app_name_uc}}_SendHkCmd(const {{app_name_uc}}_SendHkCmd_t *Msg)
{
    int i;

    /*
    ** Get command execution counters...
    */
    {{app_name_uc}}_Data.HkTlm.Payload.CommandErrorCounter = {{app_name_uc}}_Data.ErrCounter;
    {{app_name_uc}}_Data.HkTlm.Payload.CommandCounter      = {{app_name_uc}}_Data.CmdCounter;

    /*
    ** Send housekeeping telemetry packet...
    */
    CFE_SB_TimeStampMsg(CFE_MSG_PTR({{app_name_uc}}_Data.HkTlm.TelemetryHeader));
    CFE_SB_TransmitMsg(CFE_MSG_PTR({{app_name_uc}}_Data.HkTlm.TelemetryHeader), true);

    /*
    ** Manage any pending table loads, validations, etc.
    */
    for (i = 0; i < {{app_name_uc}}_NUMBER_OF_TABLES; i++)
    {
        CFE_TBL_Manage({{app_name_uc}}_Data.TblHandles[i]);
    }

    return CFE_SUCCESS;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
/*                                                                            */
/* SAMPLE NOOP commands                                                       */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
CFE_Status_t {{app_name_uc}}_NoopCmd(const {{app_name_uc}}_NoopCmd_t *Msg)
{
    {{app_name_uc}}_Data.CmdCounter++;

    CFE_EVS_SendEvent({{app_name_uc}}_NOOP_INF_EID, CFE_EVS_EventType_INFORMATION, "SAMPLE: NOOP command %s",
                      {{app_name_uc}}_VERSION);

    return CFE_SUCCESS;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
/*                                                                            */
/*  Purpose:                                                                  */
/*         This function resets all the global counter variables that are     */
/*         part of the task telemetry.                                        */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * *  * *  * * * * */
CFE_Status_t {{app_name_uc}}_ResetCountersCmd(const {{app_name_uc}}_ResetCountersCmd_t *Msg)
{
    {{app_name_uc}}_Data.CmdCounter = 0;
    {{app_name_uc}}_Data.ErrCounter = 0;

    CFE_EVS_SendEvent({{app_name_uc}}_RESET_INF_EID, CFE_EVS_EventType_INFORMATION, "SAMPLE: RESET command");

    return CFE_SUCCESS;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
/*                                                                            */
/*  Purpose:                                                                  */
/*         This function Process Ground Station Command                       */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * *  * * * * * * *  * *  * * * * */
CFE_Status_t {{app_name_uc}}_ProcessCmd(const {{app_name_uc}}_ProcessCmd_t *Msg)
{
    CFE_Status_t               Status;
    void *                     TblAddr;
    {{app_name_uc}}_ExampleTable_t *TblPtr;
    const char *               TableName = "{{app_name_uc}}.ExampleTable";

    /* Sample Use of Example Table */
    {{app_name_uc}}_Data.CmdCounter++;
    Status = CFE_TBL_GetAddress(&TblAddr, {{app_name_uc}}_Data.TblHandles[0]);
    if (Status < CFE_SUCCESS)
    {
        CFE_ES_WriteToSysLog("Sample App: Fail to get table address: 0x%08lx", (unsigned long)Status);
    }
    else
    {
        TblPtr = TblAddr;
        CFE_ES_WriteToSysLog("Sample App: Example Table Value 1: %d  Value 2: %d", TblPtr->Int1, TblPtr->Int2);

        {{app_name_uc}}_GetCrc(TableName);

        Status = CFE_TBL_ReleaseAddress({{app_name_uc}}_Data.TblHandles[0]);
        if (Status != CFE_SUCCESS)
        {
            CFE_ES_WriteToSysLog("Sample App: Fail to release table address: 0x%08lx", (unsigned long)Status);
        }
        else
        {
            /* Invoke a function provided by {{app_name_uc}}_LIB */
            SAMPLE_LIB_Function();
        }
    }

    return Status;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
/*                                                                            */
/* A simple example command that displays a passed-in value                   */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
CFE_Status_t {{app_name_uc}}_DisplayParamCmd(const {{app_name_uc}}_DisplayParamCmd_t *Msg)
{
    {{app_name_uc}}_Data.CmdCounter++;
    CFE_EVS_SendEvent({{app_name_uc}}_VALUE_INF_EID, CFE_EVS_EventType_INFORMATION,
                      "{{app_name_uc}}: ValU32=%lu, ValI16=%d, ValStr=%s", (unsigned long)Msg->Payload.ValU32,
                      (int)Msg->Payload.ValI16, Msg->Payload.ValStr);

    return CFE_SUCCESS;
}
