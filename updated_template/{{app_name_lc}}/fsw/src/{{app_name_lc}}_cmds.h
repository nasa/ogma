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
 *   This file contains the prototypes for the Sample App Ground Command-handling functions
 */

#ifndef {{app_name_uc}}_CMDS_H
#define {{app_name_uc}}_CMDS_H

/*
** Required header files.
*/
#include "cfe_error.h"
#include "{{app_name_lc}}_msg.h"

CFE_Status_t {{app_name_uc}}_SendHkCmd(const {{app_name_uc}}_SendHkCmd_t *Msg);
CFE_Status_t {{app_name_uc}}_ResetCountersCmd(const {{app_name_uc}}_ResetCountersCmd_t *Msg);
CFE_Status_t {{app_name_uc}}_ProcessCmd(const {{app_name_uc}}_ProcessCmd_t *Msg);
CFE_Status_t {{app_name_uc}}_NoopCmd(const {{app_name_uc}}_NoopCmd_t *Msg);
CFE_Status_t {{app_name_uc}}_DisplayParamCmd(const {{app_name_uc}}_DisplayParamCmd_t *Msg);

#endif /* {{app_name_uc}}_CMDS_H */
