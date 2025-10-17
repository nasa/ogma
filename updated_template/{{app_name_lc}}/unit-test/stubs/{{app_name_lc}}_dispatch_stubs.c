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
 * @file
 *
 * Auto-Generated stub implementations for functions defined in {{app_name_lc}}_dispatch header
 */

#include "{{app_name_lc}}_dispatch.h"
#include "utgenstub.h"

/*
 * ----------------------------------------------------
 * Generated stub function for {{app_name_uc}}_ProcessGroundCommand()
 * ----------------------------------------------------
 */
void {{app_name_uc}}_ProcessGroundCommand(const CFE_SB_Buffer_t *SBBufPtr)
{
    UT_GenStub_AddParam({{app_name_uc}}_ProcessGroundCommand, const CFE_SB_Buffer_t *, SBBufPtr);

    UT_GenStub_Execute({{app_name_uc}}_ProcessGroundCommand, Basic, NULL);
}

/*
 * ----------------------------------------------------
 * Generated stub function for {{app_name_uc}}_TaskPipe()
 * ----------------------------------------------------
 */
void {{app_name_uc}}_TaskPipe(const CFE_SB_Buffer_t *SBBufPtr)
{
    UT_GenStub_AddParam({{app_name_uc}}_TaskPipe, const CFE_SB_Buffer_t *, SBBufPtr);

    UT_GenStub_Execute({{app_name_uc}}_TaskPipe, Basic, NULL);
}

/*
 * ----------------------------------------------------
 * Generated stub function for {{app_name_uc}}_VerifyCmdLength()
 * ----------------------------------------------------
 */
bool {{app_name_uc}}_VerifyCmdLength(const CFE_MSG_Message_t *MsgPtr, size_t ExpectedLength)
{
    UT_GenStub_SetupReturnBuffer({{app_name_uc}}_VerifyCmdLength, bool);

    UT_GenStub_AddParam({{app_name_uc}}_VerifyCmdLength, const CFE_MSG_Message_t *, MsgPtr);
    UT_GenStub_AddParam({{app_name_uc}}_VerifyCmdLength, size_t, ExpectedLength);

    UT_GenStub_Execute({{app_name_uc}}_VerifyCmdLength, Basic, NULL);

    return UT_GenStub_GetReturnValue({{app_name_uc}}_VerifyCmdLength, bool);
}
