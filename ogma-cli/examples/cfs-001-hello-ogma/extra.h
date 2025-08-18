#define SAMPLE_MID 0x1878

typedef struct sample_msg {
   uint8_t CmdHeader[CFE_SB_CMD_HDR_SIZE];
   int32_t payload;
} sample_msg_t;
