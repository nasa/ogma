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
 *  The Sample App header file containing version information
 */

#ifndef {{app_name_uc}}_VERSION_H
#define {{app_name_uc}}_VERSION_H

/* Development Build Macro Definitions */

#define {{app_name_uc}}_BUILD_NUMBER    54
#define {{app_name_uc}}_BUILD_BASELINE  "equuleus-rc1" /*!< Development Build: git tag that is the base for the current development */
#define {{app_name_uc}}_BUILD_DEV_CYCLE "equuleus-rc2" /**< @brief Development: Release name for current development cycle */
#define {{app_name_uc}}_BUILD_CODENAME  "Equuleus" /**< @brief: Development: Code name for the current build */

/*
 * Version Macros, see \ref cfsversions for definitions.
 */
#define {{app_name_uc}}_MAJOR_VERSION 1  /*!< @brief Major version number. */
#define {{app_name_uc}}_MINOR_VERSION 1  /*!< @brief Minor version number. */
#define {{app_name_uc}}_REVISION      0  /*!< @brief Revision version number. Value of 0 indicates a development version.*/

/**
 * @brief Last official release.
 */
#define {{app_name_uc}}_LAST_OFFICIAL "v1.1.0"

/*!
 * @brief Mission revision.
 *
 * Reserved for mission use to denote patches/customizations as needed.
 * Values 1-254 are reserved for mission use to denote patches/customizations as needed. NOTE: Reserving 0 and 0xFF for
 * cFS open-source development use (pending resolution of nasa/cFS#440)
 */
#define {{app_name_uc}}_MISSION_REV 0xFF

#define {{app_name_uc}}_STR_HELPER(x) #x /*!< @brief Helper function to concatenate strings from integer macros */
#define {{app_name_uc}}_STR(x) \
    {{app_name_uc}}_STR_HELPER(x) /*!< @brief Helper function to concatenate strings from integer macros */

/*! @brief Development Build Version Number.
 * @details Baseline git tag + Number of commits since baseline. @n
 * See @ref cfsversions for format differences between development and release versions.
 */
#define {{app_name_uc}}_VERSION {{app_name_uc}}_BUILD_BASELINE "+dev" {{app_name_uc}}_STR({{app_name_uc}}_BUILD_NUMBER)

/**
 * @brief Max Version String length.
 * 
 * Maximum length that an OSAL version string can be.
 * 
 */
#define {{app_name_uc}}_CFG_MAX_VERSION_STR_LEN 256

#endif /* {{app_name_uc}}_VERSION_H */
