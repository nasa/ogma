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
 *   Specification for the {{app_name_uc}} command and telemetry
 *   message data types.
 *
 * @note
 *   Constants and enumerated types related to these message structures
 *   are defined in {{app_name_lc}}_msgdefs.h.
 */
#ifndef {{app_name_uc}}_MSGSTRUCT_H
#define {{app_name_uc}}_MSGSTRUCT_H

/************************************************************************
 * Includes
 ************************************************************************/

#include "{{app_name_lc}}_mission_cfg.h"
#include "{{app_name_lc}}_msgdefs.h"
#include "cfe_msg_hdr.h"

/*************************************************************************/

/*
** The following commands all share the "NoArgs" format
**
** They are each given their own type name matching the command name, which
** allows them to change independently in the future without changing the prototype
** of the handler function
*/
typedef struct
{
    CFE_MSG_CommandHeader_t CommandHeader; /**< \brief Command header */
} {{app_name_uc}}_NoopCmd_t;

typedef struct
{
    CFE_MSG_CommandHeader_t CommandHeader; /**< \brief Command header */
} {{app_name_uc}}_ResetCountersCmd_t;

typedef struct
{
    CFE_MSG_CommandHeader_t CommandHeader; /**< \brief Command header */
} {{app_name_uc}}_ProcessCmd_t;

typedef struct
{
    CFE_MSG_CommandHeader_t           CommandHeader; /**< \brief Command header */
    {{app_name_uc}}_DisplayParam_Payload_t Payload;
} {{app_name_uc}}_DisplayParamCmd_t;

/*************************************************************************/
/*
** Type definition (Sample App housekeeping)
*/

typedef struct
{
    CFE_MSG_CommandHeader_t CommandHeader; /**< \brief Command header */
} {{app_name_uc}}_SendHkCmd_t;

typedef struct
{
    CFE_MSG_TelemetryHeader_t  TelemetryHeader; /**< \brief Telemetry header */
    {{app_name_uc}}_HkTlm_Payload_t Payload;         /**< \brief Telemetry payload */
} {{app_name_uc}}_HkTlm_t;

#endif /* {{app_name_uc}}_MSGSTRUCT_H */
