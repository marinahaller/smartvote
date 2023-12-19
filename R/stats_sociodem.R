#' stats_soziodem
#'
#' @param data A dataframe containing individual-level candidate data.
#' @param var A variable containing sociodemographic data of the candidates.
#' @param parties A variable containing the political parties of the candidates.
#'
#' @return A cross table with descriptive statistics on your variable of interest overall and for each party.
#' @export
#'
#' @examples
#' # Import or Create a data frame
#' df <- data.frame(candidate = c("Candidate A", "Candidate B", "Candidate C", "Candidate D"),
#'                  parties = c("Party A", "Party B", "Party A", "Party C"),
#'                  gender = c("Male", "Female", "Female", "Female"))
#' df
#'
#' # Example for gender statistics
#' stats_soziodem(df, gender, parties)

stats_sociodem <- function(data, var, parties){

  # Create tables for statistics overall
  sv_stats_all <- data %>%
    group_by({{var}}) %>%
    summarise(all_n = n()) %>%
    mutate(all_per = prop.table(all_n) * 100)

  # Create table for statistics grouped by parties
  parties_name <- deparse(substitute(parties))
  sv_stats <- data %>%
    group_by({{var}}, {{parties}}) %>%
    summarise(n = n()) %>%
    group_by({{parties}}) %>%
    mutate(per = prop.table(n) * 100) %>%
    pivot_wider(names_from = {{parties}},
                values_from = c("n", "per"),
                names_glue = paste0("{", parties_name, "}_{.value}")
    ) %>%
    select({{var}}, sort(names(.)))

  # Join
  var_name <- deparse(substitute(var))
  sv_stats_final <- left_join(sv_stats, sv_stats_all, by=var_name)
  sv_stats_final
}
