/*******************************************************************************
** File: copilot_app.h
**
** Purpose:
**   This file is main hdr file for the COPILOT application.
**
**
*******************************************************************************/

#ifndef _copilot_app_h_
#define _copilot_app_h_

/*
** Required header files.
*/
#include "cfe.h"
#include "cfe_error.h"
#include "cfe_evs.h"
#include "cfe_sb.h"
#include "cfe_es.h"

#include <string.h>
#include <errno.h>
#include <unistd.h>

/***********************************************************************/

#define COPILOT_PIPE_DEPTH                     32

/************************************************************************
** Type Definitions
*************************************************************************/

/****************************************************************************/
/*
** Local function prototypes.
**
** Note: Except for the entry point (COPILOT_AppMain), these
**       functions are not called from any other source module.
*/
void COPILOT_AppMain(void);
void COPILOT_AppInit(void);
void COPILOT_ProcessCommandPacket(void);
void COPILOT_ProcessIcarousPosition(void);
void COPILOT_ResetCounters(void);

boolean COPILOT_VerifyCmdLength(CFE_SB_MsgPtr_t msg, uint16 ExpectedLength);

#endif /* _copilot_app_h_ */
