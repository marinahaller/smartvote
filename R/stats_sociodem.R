#' stats_sociodem
#'
#' Function calculates overall and grouped descriptive statistics on your variable of interest and puts it in a cross table.
#'
#' @name stats_sociodem
#' @param data A dataframe containing individual-level candidate data.
#' @param var A variable containing sociodemographic data of the candidates.
#' @param parties A variable containing the political parties of the candidates.
#'
#' @return A cross table with descriptive statistics on your variable of interest overall and for each party.
#'
#' @importFrom dplyr group_by summarise n mutate select left_join %>%
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
#'
#' @examples
#' # Import or Create a data frame
#' df <- data.frame(candidate = c("Candidate A", "Candidate B", "Candidate C", "Candidate D"),
#'                  parties = c("Party A", "Party B", "Party A", "Party C"),
#'                  gender = c("Male", "Female", "Female", "Female"))
#' df
#'
#' # Example for gender statistics
#' stats_sociodem(df, gender, parties)
#'
#' @export

stats_sociodem <- function(data, var, parties){

  # Create tables for statistics overall
  sv_stats_all <- data %>%
    dplyr::group_by({{var}}) %>%
    dplyr::summarise(all_n = dplyr::n()) %>%
    dplyr::mutate(all_per = prop.table(all_n) * 100)

  # Create table for statistics grouped by parties
  parties_name <- deparse(substitute(parties))
  sv_stats <- data %>%
    dplyr::group_by({{var}}, {{parties}}) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::group_by({{parties}}) %>%
    dplyr::mutate(per = prop.table(n) * 100) %>%
    tidyr::pivot_wider(names_from = {{parties}},
                       values_from = c("n", "per"),
                       names_glue = paste0("{", parties_name, "}_{.value}")
    ) %>%
    dplyr::select({{var}}, sort(names(.)))

  # Join
  var_name <- deparse(substitute(var))
  sv_stats_final <- dplyr::left_join(sv_stats, sv_stats_all, by=var_name)
  sv_stats_final
}

