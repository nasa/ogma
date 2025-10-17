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

/*
** File: coveragetest_{{app_name_lc}}.c
**
** Purpose:
** Coverage Unit Test cases for the Sample Application
**
** Notes:
** This implements various test cases to exercise all code
** paths through all functions defined in the Sample application.
**
** It is primarily focused at providing examples of the various
** stub configurations, hook functions, and wrapper calls that
** are often needed when coercing certain code paths through
** complex functions.
*/

/*
 * Includes
 */
#include "sample_lib.h" /* For SAMPLE_LIB_Function */
#include "{{app_name_lc}}_coveragetest_common.h"

/*
**********************************************************************************
**          TEST CASE FUNCTIONS
**********************************************************************************
*/

void Test_{{app_name_uc}}_Main(void)
{
    CFE_SB_MsgId_t MsgId = CFE_SB_INVALID_MSG_ID;

    /*
     * Test Case For:
     * void {{app_name_uc}}_Main( void )
     */

    UT_CheckEvent_t EventTest;

    /*
     * {{app_name_uc}}_Main does not return a value,
     * but it has several internal decision points
     * that need to be exercised here.
     *
     * First call it in "nominal" mode where all
     * dependent calls should be successful by default.
     */
    {{app_name_uc}}_Main();

    /*
     * Confirm that CFE_ES_ExitApp() was called at the end of execution
     */
    UtAssert_STUB_COUNT(CFE_ES_ExitApp, 1);

    /*
     * Now set up individual cases for each of the error paths.
     * The first is for {{app_name_uc}}_Init().  As this is in the same
     * code unit, it is not a stub where the return code can be
     * easily set.  In order to get this to fail, an underlying
     * call needs to fail, and the error gets propagated through.
     * The call to CFE_EVS_Register is the first opportunity.
     * Any identifiable (non-success) return code should work.
     */
    UT_SetDeferredRetcode(UT_KEY(CFE_EVS_Register), 1, CFE_EVS_INVALID_PARAMETER);

    /*
     * Just call the function again.  It does not return
     * the value, so there is nothing to test for here directly.
     * However, it should show up in the coverage report that
     * the {{app_name_uc}}_Init() failure path was taken.
     */
    {{app_name_uc}}_Main();

    /*
     * This can validate that the internal "RunStatus" was
     * set to CFE_ES_RunStatus_APP_ERROR, by querying the struct directly.
     */
    UtAssert_UINT32_EQ({{app_name_uc}}_Data.RunStatus, CFE_ES_RunStatus_APP_ERROR);

    /*
     * Note that CFE_ES_RunLoop returns a boolean value,
     * so in order to exercise the internal "while" loop,
     * it needs to return TRUE.  But this also needs to return
     * FALSE in order to get out of the loop, otherwise
     * it will stay there infinitely.
     *
     * The deferred retcode will accomplish this.
     */
    UT_SetDeferredRetcode(UT_KEY(CFE_ES_RunLoop), 1, true);

    /* Set up buffer for command processing */
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetMsgId), &MsgId, sizeof(MsgId), false);

    /*
     * Invoke again
     */
    {{app_name_uc}}_Main();

    /*
     * Confirm that CFE_SB_ReceiveBuffer() (inside the loop) was called
     */
    UtAssert_STUB_COUNT(CFE_SB_ReceiveBuffer, 1);

    /*
     * Now also make the CFE_SB_ReceiveBuffer call fail,
     * to exercise that error path.  This sends an
     * event which can be checked with a hook function.
     */
    UT_SetDeferredRetcode(UT_KEY(CFE_ES_RunLoop), 1, true);
    UT_SetDeferredRetcode(UT_KEY(CFE_SB_ReceiveBuffer), 1, CFE_SB_PIPE_RD_ERR);
    UT_CHECKEVENT_SETUP(&EventTest, {{app_name_uc}}_PIPE_ERR_EID, "SAMPLE APP: SB Pipe Read Error, App Will Exit");

    /*
     * Invoke again
     */
    {{app_name_uc}}_Main();

    /*
     * Confirm that the event was generated
     */
    UtAssert_UINT32_EQ(EventTest.MatchCount, 1);
}

void Test_{{app_name_uc}}_Init(void)
{
    /*
     * Test Case For:
     * CFE_Status_t {{app_name_uc}}_Init( void )
     */

    /* nominal case should return CFE_SUCCESS */
    UtAssert_INT32_EQ({{app_name_uc}}_Init(), CFE_SUCCESS);

    /*
     * Trigger a failure for each of the sub-calls, and confirm a write to syslog for
     * failure to register with EVS, and that an event is generated for subsequent error paths.
     * Note that the stub counts accumulate, because the status is _not_ reset between test cases.
     */
    UT_SetDeferredRetcode(UT_KEY(CFE_EVS_Register), 1, CFE_EVS_INVALID_PARAMETER);
    UtAssert_INT32_EQ({{app_name_uc}}_Init(), CFE_EVS_INVALID_PARAMETER);
    UtAssert_STUB_COUNT(CFE_ES_WriteToSysLog, 1);

    UT_SetDeferredRetcode(UT_KEY(CFE_SB_CreatePipe), 1, CFE_SB_BAD_ARGUMENT);
    UtAssert_INT32_EQ({{app_name_uc}}_Init(), CFE_SB_BAD_ARGUMENT);
    UtAssert_STUB_COUNT(CFE_EVS_SendEvent, 2); /* 1 from previous nominal case, 1 from this error path */

    UT_SetDeferredRetcode(UT_KEY(CFE_SB_Subscribe), 1, CFE_SB_BAD_ARGUMENT);
    UtAssert_INT32_EQ({{app_name_uc}}_Init(), CFE_SB_BAD_ARGUMENT);
    UtAssert_STUB_COUNT(CFE_EVS_SendEvent, 3); /* 1 additional event sent from this error path */

    UT_SetDeferredRetcode(UT_KEY(CFE_SB_Subscribe), 2, CFE_SB_BAD_ARGUMENT);
    UtAssert_INT32_EQ({{app_name_uc}}_Init(), CFE_SB_BAD_ARGUMENT);
    UtAssert_STUB_COUNT(CFE_EVS_SendEvent, 4); /* 1 additional event sent from this error path */

    UT_SetDeferredRetcode(UT_KEY(CFE_TBL_Register), 1, CFE_TBL_ERR_INVALID_OPTIONS);
    UtAssert_INT32_EQ({{app_name_uc}}_Init(), CFE_TBL_ERR_INVALID_OPTIONS);
    UtAssert_STUB_COUNT(CFE_EVS_SendEvent, 6); /* 1 from table registration error, 1 from successful init event */
}

/*
 * Register the test cases to execute with the unit test tool
 */
void UtTest_Setup(void)
{
    ADD_TEST({{app_name_uc}}_Main);
    ADD_TEST({{app_name_uc}}_Init);
}
