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
#include "{{app_name_lc}}_cmds.h"
#include "{{app_name_lc}}_utils.h"
#include "{{app_name_lc}}_eventids.h"
#include "{{app_name_lc}}_dispatch.h"
#include "{{app_name_lc}}_tbl.h"
#include "{{app_name_lc}}_version.h"

/*
** global data
*/
{{app_name_uc}}_Data_t {{app_name_uc}}_Data;

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * *  * * * * **/
/*                                                                            */
/* Application entry point and main process loop                              */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * *  * * * * **/
void {{app_name_uc}}_Main(void)
{
    CFE_Status_t     status;
    CFE_SB_Buffer_t *SBBufPtr;

    /*
    ** Create the first Performance Log entry
    */
    CFE_ES_PerfLogEntry({{app_name_uc}}_PERF_ID);

    /*
    ** Perform application-specific initialization
    ** If the Initialization fails, set the RunStatus to
    ** CFE_ES_RunStatus_APP_ERROR and the App will not enter the RunLoop
    */
    status = {{app_name_uc}}_Init();
    if (status != CFE_SUCCESS)
    {
        {{app_name_uc}}_Data.RunStatus = CFE_ES_RunStatus_APP_ERROR;
    }

    /*
    ** Sample App Runloop
    */
    while (CFE_ES_RunLoop(&{{app_name_uc}}_Data.RunStatus) == true)
    {
        /*
        ** Performance Log Exit Stamp
        */
        CFE_ES_PerfLogExit({{app_name_uc}}_PERF_ID);

        /* Pend on receipt of command packet */
        status = CFE_SB_ReceiveBuffer(&SBBufPtr, {{app_name_uc}}_Data.CommandPipe, CFE_SB_PEND_FOREVER);

        /*
        ** Performance Log Entry Stamp
        */
        CFE_ES_PerfLogEntry({{app_name_uc}}_PERF_ID);

        if (status == CFE_SUCCESS)
        {
            {{app_name_uc}}_TaskPipe(SBBufPtr);
        }
        else
        {
            CFE_EVS_SendEvent({{app_name_uc}}_PIPE_ERR_EID, CFE_EVS_EventType_ERROR,
                              "SAMPLE APP: SB Pipe Read Error, App Will Exit");

            {{app_name_uc}}_Data.RunStatus = CFE_ES_RunStatus_APP_ERROR;
        }
    }

    /*
    ** Performance Log Exit Stamp
    */
    CFE_ES_PerfLogExit({{app_name_uc}}_PERF_ID);

    CFE_ES_ExitApp({{app_name_uc}}_Data.RunStatus);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  */
/*                                                                            */
/* Initialization                                                             */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
CFE_Status_t {{app_name_uc}}_Init(void)
{
    CFE_Status_t status;
    char         VersionString[{{app_name_uc}}_CFG_MAX_VERSION_STR_LEN];

    /* Zero out the global data structure */
    memset(&{{app_name_uc}}_Data, 0, sizeof({{app_name_uc}}_Data));

    {{app_name_uc}}_Data.RunStatus = CFE_ES_RunStatus_APP_RUN;

    /*
    ** Register the events
    */
    status = CFE_EVS_Register(NULL, 0, CFE_EVS_EventFilter_BINARY);
    if (status != CFE_SUCCESS)
    {
        CFE_ES_WriteToSysLog("Sample App: Error Registering Events, RC = 0x%08lX\n", (unsigned long)status);
    }
    else
    {
        /*
         ** Initialize housekeeping packet (clear user data area).
         */
        CFE_MSG_Init(CFE_MSG_PTR({{app_name_uc}}_Data.HkTlm.TelemetryHeader), CFE_SB_ValueToMsgId({{app_name_uc}}_HK_TLM_MID),
                     sizeof({{app_name_uc}}_Data.HkTlm));

        /*
         ** Create Software Bus message pipe.
         */
        status = CFE_SB_CreatePipe(&{{app_name_uc}}_Data.CommandPipe, {{app_name_uc}}_PIPE_DEPTH, {{app_name_uc}}_PIPE_NAME);
        if (status != CFE_SUCCESS)
        {
            CFE_EVS_SendEvent({{app_name_uc}}_CR_PIPE_ERR_EID, CFE_EVS_EventType_ERROR,
                              "Sample App: Error creating SB Command Pipe, RC = 0x%08lX", (unsigned long)status);
        }
    }

    if (status == CFE_SUCCESS)
    {
        /*
        ** Subscribe to Housekeeping request commands
        */
        status = CFE_SB_Subscribe(CFE_SB_ValueToMsgId({{app_name_uc}}_SEND_HK_MID), {{app_name_uc}}_Data.CommandPipe);
        if (status != CFE_SUCCESS)
        {
            CFE_EVS_SendEvent({{app_name_uc}}_SUB_HK_ERR_EID, CFE_EVS_EventType_ERROR,
                              "Sample App: Error Subscribing to HK request, RC = 0x%08lX", (unsigned long)status);
        }
    }

    if (status == CFE_SUCCESS)
    {
        /*
        ** Subscribe to ground command packets
        */
        status = CFE_SB_Subscribe(CFE_SB_ValueToMsgId({{app_name_uc}}_CMD_MID), {{app_name_uc}}_Data.CommandPipe);
        if (status != CFE_SUCCESS)
        {
            CFE_EVS_SendEvent({{app_name_uc}}_SUB_CMD_ERR_EID, CFE_EVS_EventType_ERROR,
                              "Sample App: Error Subscribing to Commands, RC = 0x%08lX", (unsigned long)status);
        }
    }

    if (status == CFE_SUCCESS)
    {
        /*
        ** Register Example Table(s)
        */
        status = CFE_TBL_Register(&{{app_name_uc}}_Data.TblHandles[0], "ExampleTable", sizeof({{app_name_uc}}_ExampleTable_t),
                                  CFE_TBL_OPT_DEFAULT, {{app_name_uc}}_TblValidationFunc);
        if (status != CFE_SUCCESS)
        {
            CFE_EVS_SendEvent({{app_name_uc}}_TABLE_REG_ERR_EID, CFE_EVS_EventType_ERROR,
                              "Sample App: Error Registering Example Table, RC = 0x%08lX", (unsigned long)status);
        }
        else
        {
            status = CFE_TBL_Load({{app_name_uc}}_Data.TblHandles[0], CFE_TBL_SRC_FILE, {{app_name_uc}}_TABLE_FILE);
        }

        CFE_Config_GetVersionString(VersionString, {{app_name_uc}}_CFG_MAX_VERSION_STR_LEN, "Sample App", {{app_name_uc}}_VERSION,
                                    {{app_name_uc}}_BUILD_CODENAME, {{app_name_uc}}_LAST_OFFICIAL);

        CFE_EVS_SendEvent({{app_name_uc}}_INIT_INF_EID, CFE_EVS_EventType_INFORMATION, "Sample App Initialized.%s",
                          VersionString);
    }

    return status;
}
