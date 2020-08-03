/*******************************************************************************
** File:
**   copilot_cfs_msg.h 
**
** Purpose: 
**  Define COPILOT App  Messages and info
**
** Notes:
**
**
*******************************************************************************/
#ifndef _copilot_cfs_msg_h_
#define _copilot_cfs_msg_h_

/*************************************************************************/
/*
** Type definition (generic "no arguments" command)
*/
typedef struct
{
   uint8    CmdHeader[CFE_SB_CMD_HDR_SIZE];

} COPILOT_NoArgsCmd_t;

/*************************************************************************/
/*
** Type definition (COPILOT App housekeeping)
*/
typedef struct 
{
    uint8              TlmHeader[CFE_SB_TLM_HDR_SIZE];
    uint8              copilot_command_error_count;
    uint8              copilot_command_count;
    uint8              spare[2];

}   OS_PACK copilot_hk_tlm_t  ;

#endif /* _copilot_cfs_msg_h_ */

/************************/
/*  End of File Comment */
/************************/
