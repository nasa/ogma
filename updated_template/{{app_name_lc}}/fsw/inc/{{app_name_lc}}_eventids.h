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
 * Define Sample App Events IDs
 */

#ifndef {{app_name_uc}}_EVENTS_H
#define {{app_name_uc}}_EVENTS_H

#define {{app_name_uc}}_RESERVED_EID      0
#define {{app_name_uc}}_INIT_INF_EID      1
#define {{app_name_uc}}_CC_ERR_EID        2
#define {{app_name_uc}}_NOOP_INF_EID      3
#define {{app_name_uc}}_RESET_INF_EID     4
#define {{app_name_uc}}_MID_ERR_EID       5
#define {{app_name_uc}}_CMD_LEN_ERR_EID   6
#define {{app_name_uc}}_PIPE_ERR_EID      7
#define {{app_name_uc}}_VALUE_INF_EID     8
#define {{app_name_uc}}_CR_PIPE_ERR_EID   9
#define {{app_name_uc}}_SUB_HK_ERR_EID    10
#define {{app_name_uc}}_SUB_CMD_ERR_EID   11
#define {{app_name_uc}}_TABLE_REG_ERR_EID 12

#endif /* {{app_name_uc}}_EVENTS_H */
