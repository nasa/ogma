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
 *   Specification for the {{app_name_uc}} command function codes
 *
 * @note
 *   This file should be strictly limited to the command/function code (CC)
 *   macro definitions.  Other definitions such as enums, typedefs, or other
 *   macros should be placed in the msgdefs.h or msg.h files.
 */
#ifndef {{app_name_uc}}_FCNCODES_H
#define {{app_name_uc}}_FCNCODES_H

/************************************************************************
 * Macro Definitions
 ************************************************************************/

/*
** Sample App command codes
*/
#define {{app_name_uc}}_NOOP_CC           0
#define {{app_name_uc}}_RESET_COUNTERS_CC 1
#define {{app_name_uc}}_PROCESS_CC        2
#define {{app_name_uc}}_DISPLAY_PARAM_CC  3

#endif
