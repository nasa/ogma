/**
 * @struct geofence_parameters_t
 * @brief data structure containing information about the parameters used by the geofence app
 */
typeduuuef struct{
   uint8_t  TlmHeader[CFE_SB_TLM_HDR_SIZE];
   double lookahead;
   double hthreshold;
   double vthreshold;
   double hstepback;
   double vstepback;
}geofence_parameters_t;
