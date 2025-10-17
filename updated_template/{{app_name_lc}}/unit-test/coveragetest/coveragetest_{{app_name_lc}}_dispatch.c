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
** Purpose:
** Coverage Unit Test cases for the SAMPLE Application
**
** Notes:
** This implements various test cases to exercise all code
** paths through all functions defined in the SAMPLE application.
**
** It is primarily focused at providing examples of the various
** stub configurations, hook functions, and wrapper calls that
** are often needed when coercing certain code paths through
** complex functions.
*/

/*
 * Includes
 */

#include "{{app_name_lc}}_coveragetest_common.h"
#include "{{app_name_lc}}.h"
#include "{{app_name_lc}}_dispatch.h"
#include "{{app_name_lc}}_cmds.h"

/*
**********************************************************************************
**          TEST CASE FUNCTIONS
**********************************************************************************
*/

void Test_{{app_name_uc}}_TaskPipe(void)
{
    /*
     * Test Case For:
     * void {{app_name_uc}}_TaskPipe
     */
    /* a buffer large enough for any command message */
    union
    {
        CFE_SB_Buffer_t      SBBuf;
        {{app_name_uc}}_NoopCmd_t Noop;
    } TestMsg;
    CFE_SB_MsgId_t    TestMsgId;
    CFE_MSG_FcnCode_t FcnCode;
    size_t            MsgSize;
    UT_CheckEvent_t   EventTest;

    memset(&TestMsg, 0, sizeof(TestMsg));
    UT_CHECKEVENT_SETUP(&EventTest, {{app_name_uc}}_MID_ERR_EID, "SAMPLE: invalid command packet,MID = 0x%x");

    /*
     * The CFE_MSG_GetMsgId() stub uses a data buffer to hold the
     * message ID values to return.
     */
    TestMsgId = CFE_SB_ValueToMsgId({{app_name_uc}}_CMD_MID);
    FcnCode   = {{app_name_uc}}_NOOP_CC;
    MsgSize   = sizeof(TestMsg.Noop);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetMsgId), &TestMsgId, sizeof(TestMsgId), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetSize), &MsgSize, sizeof(MsgSize), false);
    {{app_name_uc}}_TaskPipe(&TestMsg.SBBuf);

    TestMsgId = CFE_SB_ValueToMsgId({{app_name_uc}}_SEND_HK_MID);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetMsgId), &TestMsgId, sizeof(TestMsgId), false);
    {{app_name_uc}}_TaskPipe(&TestMsg.SBBuf);

    /* invalid message id */
    TestMsgId = CFE_SB_INVALID_MSG_ID;
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetMsgId), &TestMsgId, sizeof(TestMsgId), false);
    {{app_name_uc}}_TaskPipe(&TestMsg.SBBuf);

    /*
     * Confirm that the event was generated only _once_
     */
    UtAssert_UINT32_EQ(EventTest.MatchCount, 1);
}

void Test_{{app_name_uc}}_ProcessGroundCommand(void)
{
    /*
     * Test Case For:
     * void {{app_name_uc}}_ProcessGroundCommand
     */
    CFE_MSG_FcnCode_t FcnCode;
    size_t            Size;
    CFE_SB_MsgId_t    MsgId = CFE_SB_ValueToMsgId(1);

    /* a buffer large enough for any command message */
    union
    {
        CFE_SB_Buffer_t               SBBuf;
        {{app_name_uc}}_NoopCmd_t          Noop;
        {{app_name_uc}}_ResetCountersCmd_t Reset;
        {{app_name_uc}}_ProcessCmd_t       Process;
        {{app_name_uc}}_DisplayParamCmd_t  DisplayParam;
    } TestMsg;
    UT_CheckEvent_t EventTest;

    memset(&TestMsg, 0, sizeof(TestMsg));

    UT_CHECKEVENT_SETUP(&EventTest, {{app_name_uc}}_CMD_LEN_ERR_EID,
                        "Invalid Msg length: ID = 0x%X,  CC = %u, Len = %u, Expected = %u");

    /*
     * call with each of the supported command codes
     * The CFE_MSG_GetFcnCode stub allows the code to be
     * set to whatever is needed.  There is no return
     * value here and the actual implementation of these
     * commands have separate test cases, so this just
     * needs to exercise the "switch" statement.
     */

    /* test dispatch of NOOP */
    FcnCode = {{app_name_uc}}_NOOP_CC;
    Size    = sizeof(TestMsg.Noop);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetSize), &Size, sizeof(Size), false);

    {{app_name_uc}}_ProcessGroundCommand(&TestMsg.SBBuf);

    UtAssert_STUB_COUNT({{app_name_uc}}_NoopCmd, 1);

    FcnCode = {{app_name_uc}}_NOOP_CC;
    Size    = sizeof(TestMsg.Noop) - 1;
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetSize), &Size, sizeof(Size), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetMsgId), &MsgId, sizeof(MsgId), false);

    {{app_name_uc}}_ProcessGroundCommand(&TestMsg.SBBuf);

    UtAssert_STUB_COUNT({{app_name_uc}}_NoopCmd, 1);
    UtAssert_UINT32_EQ(EventTest.MatchCount, 1);

    /* test dispatch of RESET */
    FcnCode = {{app_name_uc}}_RESET_COUNTERS_CC;
    Size    = sizeof(TestMsg.Reset);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetSize), &Size, sizeof(Size), false);

    {{app_name_uc}}_ProcessGroundCommand(&TestMsg.SBBuf);

    UtAssert_STUB_COUNT({{app_name_uc}}_ResetCountersCmd, 1);

    FcnCode = {{app_name_uc}}_RESET_COUNTERS_CC;
    Size    = sizeof(TestMsg.Reset) - 1;
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetSize), &Size, sizeof(Size), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetMsgId), &MsgId, sizeof(MsgId), false);

    {{app_name_uc}}_ProcessGroundCommand(&TestMsg.SBBuf);

    UtAssert_STUB_COUNT({{app_name_uc}}_ResetCountersCmd, 1);
    UtAssert_UINT32_EQ(EventTest.MatchCount, 2);

    /* test dispatch of PROCESS */
    /* note this will end up calling {{app_name_uc}}_Process(), and as such it needs to
     * avoid dereferencing a table which does not exist. */
    FcnCode = {{app_name_uc}}_PROCESS_CC;
    Size    = sizeof(TestMsg.Process);
    UT_SetDefaultReturnValue(UT_KEY(CFE_TBL_GetAddress), CFE_TBL_ERR_UNREGISTERED);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetSize), &Size, sizeof(Size), false);

    {{app_name_uc}}_ProcessGroundCommand(&TestMsg.SBBuf);

    UtAssert_STUB_COUNT({{app_name_uc}}_ProcessCmd, 1);

    FcnCode = {{app_name_uc}}_PROCESS_CC;
    Size    = sizeof(TestMsg.Process) - 1;
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetSize), &Size, sizeof(Size), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetMsgId), &MsgId, sizeof(MsgId), false);

    {{app_name_uc}}_ProcessGroundCommand(&TestMsg.SBBuf);

    UtAssert_STUB_COUNT({{app_name_uc}}_ProcessCmd, 1);
    UtAssert_UINT32_EQ(EventTest.MatchCount, 3);

    /* test dispatch of DISPLAY_PARAM */
    FcnCode = {{app_name_uc}}_DISPLAY_PARAM_CC;
    Size    = sizeof(TestMsg.DisplayParam);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetSize), &Size, sizeof(Size), false);

    {{app_name_uc}}_ProcessGroundCommand(&TestMsg.SBBuf);

    UtAssert_STUB_COUNT({{app_name_uc}}_DisplayParamCmd, 1);

    FcnCode = {{app_name_uc}}_DISPLAY_PARAM_CC;
    Size    = sizeof(TestMsg.DisplayParam) - 1;
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetSize), &Size, sizeof(Size), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetMsgId), &MsgId, sizeof(MsgId), false);

    {{app_name_uc}}_ProcessGroundCommand(&TestMsg.SBBuf);

    UtAssert_STUB_COUNT({{app_name_uc}}_DisplayParamCmd, 1);
    UtAssert_UINT32_EQ(EventTest.MatchCount, 4);

    /* test an invalid CC */
    FcnCode = 1000;
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &FcnCode, sizeof(FcnCode), false);
    UT_CHECKEVENT_SETUP(&EventTest, {{app_name_uc}}_CC_ERR_EID, "Invalid ground command code: CC = %d");
    {{app_name_uc}}_ProcessGroundCommand(&TestMsg.SBBuf);

    /*
     * Confirm that the event was generated only _once_
     */
    UtAssert_UINT32_EQ(EventTest.MatchCount, 1);
}

void Test_{{app_name_uc}}_VerifyCmdLength(void)
{
    /*
     * Test Case For:
     * bool {{app_name_uc}}_VerifyCmdLength
     */
    UT_CheckEvent_t   EventTest;
    size_t            size    = 1;
    CFE_MSG_FcnCode_t fcncode = 2;
    CFE_SB_MsgId_t    msgid   = CFE_SB_ValueToMsgId(3);

    /*
     * test a match case
     */
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetSize), &size, sizeof(size), false);
    UT_CHECKEVENT_SETUP(&EventTest, {{app_name_uc}}_CMD_LEN_ERR_EID,
                        "Invalid Msg length: ID = 0x%X,  CC = %u, Len = %u, Expected = %u");

    {{app_name_uc}}_VerifyCmdLength(NULL, size);

    /*
     * Confirm that the event was NOT generated
     */
    UtAssert_UINT32_EQ(EventTest.MatchCount, 0);

    /*
     * test a mismatch case
     */
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetSize), &size, sizeof(size), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetMsgId), &msgid, sizeof(msgid), false);
    UT_SetDataBuffer(UT_KEY(CFE_MSG_GetFcnCode), &fcncode, sizeof(fcncode), false);
    {{app_name_uc}}_VerifyCmdLength(NULL, size + 1);

    /*
     * Confirm that the event WAS generated
     */
    UtAssert_UINT32_EQ(EventTest.MatchCount, 1);
}

/*
 * Register the test cases to execute with the unit test tool
 */
void UtTest_Setup(void)
{
    ADD_TEST({{app_name_uc}}_TaskPipe);
    ADD_TEST({{app_name_uc}}_ProcessGroundCommand);
    ADD_TEST({{app_name_uc}}_VerifyCmdLength);
}
