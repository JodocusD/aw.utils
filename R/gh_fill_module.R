#gh_fillrate_module


#' df_ghfill_mock_creation
#'
#' creates up a dummy tibble with 0 values
#' @keywords none
#' @export
#' @examples
#' df_ghfill_mock_creation()
df_ghfill_mock_creation <- function() {
  df <- tibble::tibble(
    date = as.Date("2019-01-01"),
    adserver = 0,
    adx = 0,
    advertiser = rep(c("Preferred_deal", "Direct_sales", "Freebee_fillers", "Freebee_fillers", "AdX", "EBDA"), 9),
    unit = rep(c("LB", "SS", "SS", "mobMR", "mobMR", "mobMR", "mobMR", "mobMR", "MR"), each = 6),
    impr = 0,
    weekbegin = date  # only for empty vector construction
  ) %>%
    dplyr::mutate(unit = as.factor(unit) %>% forcats::fct_relevel("mobMR", "LB", "SS", "MR"),
           advertiser = as.factor(advertiser) %>%
             forcats::fct_relevel("Direct_sales", "AdX", "Freebee_fillers") %>%
             forcats::fct_explicit_na() )

  return(df)
}

#' dateconverterRdfpJst
#'
#' converts dates to a list that the DFP API understnads
#' @param this_date the date to be converted
#' @return returns a list representing the date
#' @examples
#' dateconverterRdfpJst(as.Date("2019-01-01"))
#' @export

dateconverterRdfpJst <- function(this_date)
{
  x <- list(year = lubridate::year(this_date), month = lubridate::month(this_date),
            day = lubridate::day(this_date))
  return(x)
}



# Module UI --------
#' gh_fillUI
#'
#' UI part of ghanafill shinyModule
#' @param id use a unique id for the module
#' @return returns a taglist to be used in shiny UI
#' @export
gh_fillUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

tagList(
  fluidRow(
  column(
    3,
    actionButton(ns("goButton"), "Download Data!",icon("download")),
#style="color: #fff; background-color: #00a65a; border-color: #2e6da4"),
    h5("be patient, this can take a minute (literally)")
  ),

  column(
    3,
    radioButtons(ns("dorw"), label = "Group Impressions per:", choiceNames = c("Week", "Day"), choiceValues = c("weekbegin", "date"), selected = "weekbegin")
  ),
  column(
    3,
    radioButtons(ns("facet"),
                 label = "Show banner numbers:",
                 choiceNames = c("All-in-one", "Per size"),
                 choiceValues = c("null", "unit"),
                 selected = "null"
    )
  ),
  column(
    2,
    radioButtons(ns("stack"),
                 label = "Display totals:",
                 choiceNames = c("on_top", "side-by-side"),
                 choiceValues = c("on_top", "side-by-side"),
                 selected = "on_top"
    )
  )
),
# Show a plot of the generated distribution
fluidRow(
  shinydashboard::box(title = "Percent of Ghanaian Impressions", width = 12,
      plotOutput(ns("pct_plot"), height = "200px")
  )
),
fluidRow(
  shinydashboard::box(title = "Absolute Numbers", width = 12,
      plotOutput(ns("abs_plot"), height = "200px")
  )
))
}


# server module --------
#' gh_fill
#'
#' Server part of ghanafill shinyModule
#' @param input will be filled automatically by shiny
#' @param output will be filled automatically by shiny
#' @param session will be filled automatically by shiny
#' @return returns code to be used in shiny server file
#' @example gh_fill()
#' @export
gh_fill <- function(input, output, session) {

  rv <- reactiveValues(df = df_ghfill_mock_creation())

  df_grouped <- reactive({
    var_dorw <- dplyr::sym(input$dorw)
    var_facet <- dplyr::sym(input$facet)

    x <- rv$df %>%
      {if(input$facet == "null") {
        dplyr::group_by(., !!var_dorw, advertiser)
      } else {dplyr::group_by(., !!var_dorw, advertiser, !!var_facet) }
      } %>%
      dplyr::summarise(impressions = sum(impr)) %>%
      dplyr::ungroup()

    return(x)
  })


  output$pct_plot <- renderPlot({
    var_dorw <- dplyr::sym(input$dorw)
    var_facet <- dplyr::sym(input$facet)

    df_grouped() %>%
      ggplot2::ggplot(aes(!!var_dorw, impressions)) +
      ggplot2::geom_col(aes(fill = forcats::fct_rev(advertiser), colour = forcats::fct_rev(advertiser)),position = ggplot2::position_fill()) +
      #scale_x_date(date_breaks = "1 days", date_labels = "%b %d") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      viridis::scale_fill_viridis(discrete = TRUE, begin = 0.1, end = 0.9)+
      viridis::scale_colour_viridis(discrete = TRUE, begin = 0.1, end = 0.9, guide = "none")+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(title = "", y = "% of Impressions", fill = "Advertiser", x = NULL) +
      {if(var_facet == "unit")ggplot2::facet_wrap(~unit, scales = 'free_y', ncol = 4)} +
      NULL
  })

  output$abs_plot <- renderPlot({
    var_dorw <- dplyr::sym(input$dorw)
    var_facet <- dplyr::sym(input$facet)
    var_stack <- dplyr::sym(input$stack)

    df_grouped() %>%
      ggplot2::ggplot(aes(!!var_dorw, impressions, fill = forcats::fct_rev(advertiser), colour = forcats::fct_rev(advertiser))) +
      {if(var_stack == "side-by-side"){ggplot2::geom_col(position = ggplot2::position_dodge())}else{ggplot2::geom_col(position = ggplot2::position_stack())}} +
      ggplot2::labs(title = "", y = "Impressions x 1000", fill = "Advertiser", x = NULL) +
      ggplot2::scale_y_continuous(labels = scales::comma_format()) +
      #scale_x_date(date_breaks = "1 days", date_labels = "%b %d") +
      viridis::scale_fill_viridis(discrete = TRUE, begin = 0.1, end = 0.9)+
      viridis::scale_colour_viridis(discrete = TRUE, begin = 0.1, end = 0.9, guide = "none")+
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      {if(var_facet == "unit")ggplot2::facet_wrap(~unit, scales = 'free_y', ncol = 4)} +
      NULL
  })



  # observer -----
  observeEvent(input$goButton, {
    withProgress(message = "Plucking fresh data: Patience Please!",{

      filter_GH_inGhana <- "WHERE (COUNTRY_CRITERIA_ID = 2288 AND PARENT_AD_UNIT_ID = 21732917487)"

      request_data_gh_fill <- list(reportJob =
                                     list(reportQuery =
                                            list(dimensions = 'DATE',
                                                 dimensions = 'AD_UNIT_NAME',
                                                 dimensions = 'LINE_ITEM_TYPE',
                                                 adUnitView = 'FLAT',
                                                 columns = 'AD_SERVER_IMPRESSIONS',
                                                 columns = 'AD_EXCHANGE_IMPRESSIONS',
                                                 startDate = dateconverterRdfpJst(Sys.Date() - 45),
                                                 endDate =  dateconverterRdfpJst(Sys.Date() - 1),
                                                 dateRangeType = 'CUSTOM_DATE',
                                                 # statement = list(query = filter1),
                                                 statement = list(query = filter_GH_inGhana)
                                            )
                                     )
      )

      report_data_raw <- rdfp::dfp_full_report_wrapper(request_data_gh_fill, max_tries = 40, check_interval = 5)

      report_data <-
        report_data_raw %>%
        janitor::clean_names() %>%
        dplyr::select(
          date = dimension_date,
          adunitraw = dimension_ad_unit_name,
          adserver = column_ad_server_impressions,
          adx = column_ad_exchange_impressions,
          type = dimension_line_item_type
        ) %>%
        dplyr::mutate(adunit = stringr::str_extract(adunitraw, "(?<=487\\) . ).*(?= \\()") ) %>%
        dplyr::filter(
          # !is.na(adunit),
          adunit != "LBmid"
        ) %>%
        dplyr::mutate(unit = dplyr::case_when(
          grepl("^MR", adunit, ignore.case = TRUE) ~ "MR",
          grepl("mobMR", adunit, ignore.case = TRUE) ~ "mobMR",  # make mob.MR
          grepl("WLB", adunit, ignore.case = TRUE) ~ "LB",
          grepl("WSS", adunit, ignore.case = TRUE) ~ "SS",
          TRUE ~ "other"
        )) %>%
        dplyr::mutate(unit = as.factor(unit) %>% forcats::fct_relevel("mobMR", "LB", "SS", "MR")) %>%
        dplyr::mutate(advertiser = dplyr::case_when(
          type == "STANDARD" ~ "Direct_sales",
          type == "PRICE_PRIORITY" ~ "Freebee_fillers",
          type == "NETWORK" ~ "Freebee_fillers",
          type == "AD_EXCHANGE" ~ "AdX",
          type == "PREFERRED_DEAL" ~ "Preferred_deal",
          type == "HOUSE" ~ "House_banner",
          type == "-" ~ "EBDA",
          TRUE ~ "unknown"
        ) %>%
          as.factor() %>%
          forcats::fct_relevel("Direct_sales", "AdX", "Freebee_fillers") #%>%
        # fct_explicit_na()
        ) %>%
        #dplyr::filter(unit != "other")  %>%
        dplyr::mutate(weekbegin = lubridate::floor_date(date, unit = "weeks"),
               impr = adserver + adx)

      rv$df <- report_data
    })
  })

}
