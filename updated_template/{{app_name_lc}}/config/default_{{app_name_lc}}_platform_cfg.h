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
 * {{app_name_uc}} Application Platform Configuration Header File
 *
 * This is a compatibility header for the "platform_cfg.h" file that has
 * traditionally provided both public and private config definitions
 * for each CFS app.
 *
 * These definitions are now provided in two separate files, one for
 * the public/mission scope and one for internal scope.
 *
 * @note This file may be overridden/superceded by mission-provided defintions
 * either by overriding this header or by generating definitions from a command/data
 * dictionary tool.
 */
#ifndef {{app_name_uc}}_PLATFORM_CFG_H
#define {{app_name_uc}}_PLATFORM_CFG_H

#include "{{app_name_lc}}_mission_cfg.h"
#include "{{app_name_lc}}_internal_cfg.h"

#endif
