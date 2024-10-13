#' Several cleansing activities to shape nice tibbles
#'
#' Movement dataset
clean_mov_xlsx <- function(mov_tibble){
    colnames(mov_tibble) <- mov_tibble[2,]
    mov_tibble <- mov_tibble |>
        # trim to payload, remove empty rows
        dplyr::slice(c(-1,-2)) |>
        # standardise names
        dplyr::rename(dplyr::any_of(nice_names))
    return(mov_tibble)
}

#' coerce excel to proper datetime
#' note really sure why we cannot read in excel files and force character
coerce_xlx_numeric_to_datetime <- function(.num_vecs, .date_only = FALSE){
    num_shit <- as.numeric(as.character(.num_vecs))
    dttm <- janitor::excel_numeric_to_date(
        num_shit
        , date_system = "modern"
        , include_time = !.date_only
        , tz = "UTC")
    return(dttm)
}

coerce_column_to_character <- function(.tibble, .cols){
    .tibble <- .tibble |>
        dplyr::mutate(dplyr::across(dplyr::any_of(.cols), .fns = ~ as.character(.x)))
}

coerce_commachar_to_numeric <- function(.tibble, .cols){
    .tibble <- .tibble |>
        dplyr::mutate(dplyr::across(dplyr::any_of(.cols), .fns = ~ stringr::str_replace(.x, pattern = ",", replacement = "."))) |>
        dplyr::mutate(dplyr::across(dplyr::any_of(.cols), .fns = ~ as.numeric(.x)))
}

#' Define a function to check and convert column type
#'
check_column_and_convert_type <- function(.df, .column_name, .desired_type = "character"){

    column_type <- .df |> dplyr::pull( {{.column_name}}) |> typeof()

    if(column_type != .desired_type) {
        #------------------- default to character for any other conversion
        .df <- .df |>
            dplyr::mutate(dplyr::across({{.column_name}}, as.character))

        if(.desired_type != "character"){#----------------- coerce to other types
            # to-do = expand to other desired types
            if(.desired_type == "numeric"){

                # .....
            }
        } # end ------------------------------------------- coerce to other types
    }
    return(.df)
}


#' Clean TXXT
#'
#' files OK - mostly renaming

#' Clean Punctuality
clean_punc <- function(.bra_punc){
    my_punc <- .bra_punc |>
        # standardise names
        dplyr::rename(dplyr::any_of(nice_names)) |>
        # coerce excel numberdate to dttm
        dplyr::mutate(dplyr::across( .cols = c("SCHED_TIME","BLOCK_TIME")
                                     , .fns = ~ as.numeric(.x) |>
                                         coerce_xlx_numeric_to_datetime()
        )
        )
    return(my_punc)
}


#' Clean ASMA table



#' make nice names
nice_names_df <- tibble::tribble(
    ~ old           , ~ new
    ,"Indicativo"   , "FLTID"
    ,"Locality"     , "ICAO"
    ,"Tipo Operacao", "PHASE"
    ,"Tipo Voo"     , "FLTTYP"
    ,"Equipamento"  , "TYPE"
    ,"Autorizado Push Back", "AOBT"    # we assume this to be AOBT
    ,"Decolagem"    , "ATOT"
    ,"Pouso Real"   , "ALDT"
    ,"Aeronave Estacionada", "AIBT"

    # not needed - tbd
    ,"Eobt Previsto", "PREV_EOBT"
    ,"Eta Previsto" , "PREV_EIBT"

    # additional taxi-in-out renaming
    ,"Aerop."       , "ICAO"
    ,"Box"          , "STND"
    ,"Pista"        , "RYW"
    ,"Aeronave"     , "TYPE"
    ,"Taxi (min)"   , "TXXT"
    ,"Desimp."      , "REF_BRA"
    ,"Adicional"    , "ADD_TXXT"

    # additional names from punctuality files
    ,"Callsign"    , "FLTID"
    ,"Sigla ADEP"  , "ADEP"
    ,"Sigla ADES"  , "ADES"
    ,"Sigla LOCALITY", "ICAO"
    ,"Evento"      , "PHASE"
    ,"DH Prev Calco Strat", "SCHED_TIME"
    ,"DH Real Calco Tat", "BLOCK_TIME"

    # additional ASMA times
    ,"adep"        , "ADEP"
    ,"ades"        , "ADES"
    ,"fltid"       , "FLTID"
    ,"reg"         , "REG"
    ,"type"        , "TYPE"
    ,"drwy"        , "RWY"
    ,"c40_bear"    , "C40_BRG"
    ,"c40_time"    , "C40_TIME"
    ,"c40time"     , "C40_TIME"
    ,"c100_bear"   , "C100_BRG"
    ,"c100time"    , "C100_TIME"
    ,"aibt"        , "AIBT"
    ,"sibt"        , "SIBT"
    ,"aldt"        , "ALDT"
    ,"fltrul"      , "FLTRUL"
    ,"stnd"        , "STND"

    #VRA data set
    ,"sg_empresa"   , "OPR"
    ,"nm_empresa"   , "OPR_NAME"
    ,"nr_voo"       , "TRIP_NBR"
    ,"cd_di"        , "CD_DI"
    ,"cd_tipo_linha", "OPR_TYPE"
    ,"sg_equip"     , "TYPE"
    ,"nr_assentos"  , "NR_ASSENTOS-Idk"
    ,"sg_origem"    , "ADEP"
    ,"nm_aerodromo_origem" ,"ADEP_NAME"
    ,"dt_partida_prevista" ,"STOT"
    ,"dt_partida_real" , "ATOT"
    ,"sg_destino"   , "ADES"
    ,"nm_aerodromo_destino" , "ADES_NAME"
    ,"dt_chegada_prevista"  , "SLDT"
    ,"dt_chegada_real" , "ALDT"
    #,"tp_situacao"
)
nice_names <- setNames(nice_names_df$old, nice_names_df$new)
