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
 * Main header file for the Sample application
 */

#ifndef {{app_name_uc}}_H
#define {{app_name_uc}}_H

/*
** Required header files.
*/
#include "cfe.h"
#include "cfe_config.h"

#include "{{app_name_lc}}_mission_cfg.h"
#include "{{app_name_lc}}_platform_cfg.h"

#include "{{app_name_lc}}_perfids.h"
#include "{{app_name_lc}}_msgids.h"
#include "{{app_name_lc}}_msg.h"

/************************************************************************
** Type Definitions
*************************************************************************/

/*
** Global Data
*/
typedef struct
{
    /*
    ** Command interface counters...
    */
    uint8 CmdCounter;
    uint8 ErrCounter;

    /*
    ** Housekeeping telemetry packet...
    */
    {{app_name_uc}}_HkTlm_t HkTlm;

    /*
    ** Run Status variable used in the main processing loop
    */
    uint32 RunStatus;

    /*
    ** Operational data (not reported in housekeeping)...
    */
    CFE_SB_PipeId_t CommandPipe;

    CFE_TBL_Handle_t TblHandles[{{app_name_uc}}_NUMBER_OF_TABLES];
} {{app_name_uc}}_Data_t;

/*
** Global data structure
*/
extern {{app_name_uc}}_Data_t {{app_name_uc}}_Data;

/****************************************************************************/
/*
** Local function prototypes.
**
** Note: Except for the entry point ({{app_name_uc}}_Main), these
**       functions are not called from any other source module.
*/
void         {{app_name_uc}}_Main(void);
CFE_Status_t {{app_name_uc}}_Init(void);

#endif /* {{app_name_uc}}_H */
