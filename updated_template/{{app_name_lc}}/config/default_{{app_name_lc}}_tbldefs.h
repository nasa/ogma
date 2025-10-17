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
 *   Specification for the {{app_name_uc}} table related
 *   constant definitions.
 *
 * @note
 *   These Macro definitions have been put in this file (instead of
 *   {{app_name_lc}}_tbl.h). DO NOT PUT ANY TYPEDEFS OR
 *   STRUCTURE DEFINITIONS IN THIS FILE!
 *   ADD THEM TO {{app_name_lc}}_tbl.h IF NEEDED!
 */
#ifndef {{app_name_uc}}_TBLDEFS_H
#define {{app_name_uc}}_TBLDEFS_H

#include "common_types.h"
#include "{{app_name_lc}}_mission_cfg.h"

/*
** Example Table structure
*/
typedef struct
{
    uint16 Int1;
    uint16 Int2;
} {{app_name_uc}}_ExampleTable_t;

#endif
