append_dof <- function(.df){
    tmp <- .df |>
        dplyr::mutate(
            DOF = dplyr::case_when(
                 !is.na(BLOCK_TIME) ~ lubridate::date(BLOCK_TIME)
                , is.na(BLOCK_TIME) & !is.na(MVT_TIME) ~ lubridate::date(MVT_TIME)
                , is.na(BLOCK_TIME) &  is.na(MVT_TIME) & !is.na(SCHED_TIME) ~ lubridate::date(SCHED_TIME)
                , .default = NA
            ))
    return(tmp)
}
