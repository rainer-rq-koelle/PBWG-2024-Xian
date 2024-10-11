make_nice_names_apdf <- function(.df){
    rn_df <- .df
    oldnames <- names(rn_df)
    newnames <- gsub(pattern = "(AP_C_)|(AC_)|(_ICAO)|(_UTC)|(_CROSS)|(EA|IN)|(SRC_)", replacement = "", x = oldnames)
    names(rn_df) <- newnames
    rn_df
}

trim_apdf_for_pbwg <- function(.df){
   tmp <- .df |> dplyr::select(
        AP_C_FLTID, AP_C_REG, ADEP_ICAO, ADES_ICAO
        , SRC_PHASE
        , SCHED_TIME_UTC, BLOCK_TIME_UTC, MVT_TIME_UTC
        , ARCTYP, AC_CLASS, AP_C_FLTRUL, AP_C_RWY, AP_C_STND
        , C40_CROSS_TIME, C40_CROSS_LAT, C40_CROSS_LON, C40_CROSS_FL
        , C100_CROSS_TIME, C100_CROSS_LAT, C100_CROSS_LON, C100_CROSS_FL
        )
   return(tmp)
}
