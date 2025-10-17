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

#ifndef EVENTCHECK_H
#define EVENTCHECK_H

#include "common_types.h"
#include "cfe_evs.h"

#include "utassert.h"
#include "uttest.h"
#include "utstubs.h"

/*
 * Unit test check event hook information
 */
typedef struct
{
    uint16      ExpectedEvent;
    uint32      MatchCount;
    const char *ExpectedFormat;
} UT_CheckEvent_t;

/* Macro to get expected event name */
#define UT_CHECKEVENT_SETUP(Evt, ExpectedEvent, ExpectedFormat) \
    UT_CheckEvent_Setup_Impl(Evt, ExpectedEvent, #ExpectedEvent, ExpectedFormat)

/*
 * Helper function to set up for event checking
 * This attaches the hook function to CFE_EVS_SendEvent
 */
void UT_CheckEvent_Setup_Impl(UT_CheckEvent_t *Evt, uint16 ExpectedEvent, const char *EventName,
                              const char *ExpectedFormat);

#endif
