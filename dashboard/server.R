library(shiny)
library(shinyURL)
library(shinyjs)

#options(shiny.trace = TRUE)

shinyServer(function(input, output) {
  shinyURL.server()
  useShinyjs()  # Include shinyjs

   observe({
     if (input$visitor.success.type == "service.type") {
       shinyjs::hide(id = "advanced")
     }else if(input$visitor.success.type == "all"){
       shinyjs::hide(id = "advanced")
     }else(
       shinyjs::show(id = "advanced")
       )
   })

  #### VISITOR SUCCESS ####
  # DATA MUNGING
  # subset by timeseries start and end
  visitor.success.subset.timeseries <- reactive({  # creates data frame subset by time selection
    ga.conversions %>%
      dplyr::filter(lubridate::date(hit_timestamp_eastern) >= input$visitor.success.daterange[1],
                    lubridate::date(hit_timestamp_eastern) <= input$visitor.success.daterange[2])
  })

  # subset by radio button selector
  visitor.success.subset.data <- reactive({
    visitor.success.subset.timeseries() %>%
    {
      # anon function to filter by service_type
      if ("all" %in% c(input$visitor.success.type, input$visitor.success.type.selector)) {
        return(.)
      } else if (input$visitor.success.type == "page.type") {
        dplyr::filter(., content_type == input$visitor.success.type.selector) %>%
          return(.)
      } else if (input$visitor.success.type == "event.type") {
        dplyr::filter(., event_action == input$visitor.success.type.selector) %>%
          return(.)
      } else {
        return(.)
      }
    }
  })

  # get ids for current group by selections
  type.group.by <- reactive({
    if ("site_section" %in% input$visitor.success.group.by) {
      return(section.landing.ids)
    } else if ("topic" %in% input$visitor.success.group.by) {
      return(topic.ids)
    } else if ("subtopic" %in% input$visitor.success.group.by) {
      return(subtopic.ids)
    } else {
      return(NULL)
    }
  })

  # creates data frame of conversions (grouped or not) by date
  visitor.success.timeseries.data <- reactive({
    # add additional group_by hit_timestamp
    if (is.null(input$visitor.success.group.by)) {
      dat = visitor.success.subset.data() %>%
        dplyr::mutate(hit_timestamp = lubridate::date(hit_timestamp_eastern)) %>%
        dplyr::group_by(hit_timestamp) %>%
        dplyr::count() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percent_success = round(n / sum(n), 3) * 100)
      return(dat)
    } else {
      dat = visitor.success.subset.data() %>%
        dplyr::mutate(hit_timestamp = lubridate::date(hit_timestamp_eastern)) %>%
        {
          if (is.null(type.group.by())) {  # if we have no types to join on don't join
            return(.)
          } else {
            dplyr::inner_join(., type.group.by(), by = "node_id")
          }
        } %>%
        dplyr::group_by_(.dots = c("hit_timestamp", input$visitor.success.group.by)) %>%
        dplyr::count() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percent_success = round(n / sum(n), 3) * 100)  # round to 3rd decimal and multiply
      dat$group_factor = apply(dat[, c(input$visitor.success.group.by)],
                               MARGIN = 1, FUN = paste, collapse = " - ")
      return(dat)
    }
  })

  # reactively create aggregate data frame sans timeseries
  visitor.success.aggregate.data <- reactive({
    if (is.null(input$visitor.success.group.by)) {  # if nothing selected pass an empty df
      data.frame()
    } else {
      dat = visitor.success.subset.data() %>%
      {
        if (is.null(type.group.by())) {  # if we have no types to join on don't join
          return(.)
        } else {
          dplyr::inner_join(., type.group.by(), by = "node_id")
        }
      } %>%
        dplyr::group_by_(.dots = input$visitor.success.group.by) %>%
        dplyr::count() %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percent_success = round(n / sum(n), 3) * 100,
                      cum_percent = cumsum(percent_success))

      dat$group_factor = apply(dat[, c(input$visitor.success.group.by)],
                               MARGIN = 1, FUN = paste, collapse = " - ")
      return(dat)
    }

  })


  # REACTIVE UI
  #   filter by type
  output$type.selection.options <- renderUI({
    unique.subtypes <- visitor.success.subset.timeseries() %>%  # data which is 2 vector data frame 1) every type and 2) corresponding subtypes
    {
      if (input$visitor.success.type == "page.type") {
        unique(.$content_type) %>%
          return(.)
      } else if (input$visitor.success.type == "event.type") {
        unique(.$event_action) %>%
          return(.)
      } else {
        return(NULL)
      }
    }

    selectInput(inputId = "visitor.success.type.selector",
                label = "Filter By Type",
                choices = c("all", unique.subtypes))
  })

  # FILE DOWNLOADS
  output$visitor.success.download.timeseries <- downloadHandler(
    filename = function() {  # for some reason shiny expects it this way
      paste("vs-timeseries-", lubridate::today(tzone = "America/New_York"), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(visitor.success.timeseries.data(), file)
    }
  )

  output$visitor.success.download.aggregate <- downloadHandler(
    filename = function() {
      paste("vs-aggregate-", lubridate::today(tzone = "America/New_York"), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(visitor.success.aggregate.data(), file)
    }
  )

  # PLOTS
  #   Pareto
  output$visitor.success.grouped.pareto <- renderPlotly({
    if (is.null(input$visitor.success.group.by)) {  # if we have nothing to group on return a blank plot
      makeBlankPlot() %>%
        printGGplotly(.)
    } else if (input$visitor.success.top.bottom == "top") {
      slice.to = as.numeric(input$visitor.success.select.k)

      top.groups = visitor.success.aggregate.data() %>%
        dplyr::arrange(dplyr::desc(n)) %>%  # arrange high to low
        dplyr::slice(1:slice.to)

      visitor.success.aggregate.data() %>%
        dplyr::filter(group_factor %in% top.groups$group_factor) %>%
        {
          if (input$visitor.success.units == "percent") {
            makeGroupedPareto(df = .,
                              x = "group_factor",
                              y = "percent_success",
                              ylab = "% of Total",
                              cumul.line = "cum_percent",
                              plot.title = "Success by Group")
          } else {
            makeGroupedPareto(df = .,
                              x = "group_factor",
                              y = "n",
                              ylab = "Count",
                              plot.title = "Success by Group")
          }
        } %>%
        printGGplotly(.)
    } else {
      slice.to = as.numeric(input$visitor.success.select.k)

      top.groups = visitor.success.aggregate.data() %>%
        dplyr::arrange(n) %>%  # arrange low to high
        dplyr::slice(1:slice.to)

      visitor.success.aggregate.data() %>%
        dplyr::filter(group_factor %in% top.groups$group_factor) %>%
        {
          if (input$visitor.success.units == "percent") {
            makeGroupedPareto(df = .,
                              x = "group_factor",
                              y = "percent_success",
                              ylab = "% of Total",
                              cumul.line = "cum_percent",
                              plot.title = "Success by Group")
          } else {
            makeGroupedPareto(df = .,
                              x = "group_factor",
                              y = "n",
                              ylab = "Count",
                              plot.title = "Success by Group")
          }
        } %>%
        printGGplotly(.)
    }
  })

  # grouped timeseries
  output$visitor.success.grouped.timeseries <- renderPlotly({
    if (is.null(input$visitor.success.group.by)) {  # if we are grouping on nothing show the raw trend
      visitor.success.timeseries.data() %>%
      {
        if (input$visitor.success.units == "percent") {
          makeGroupedTimeseries(df = .,
                                x = "hit_timestamp",
                                y = "percent_success",
                                ylab = "% of Total",
                                plot.title = "Success Over Time",
                                fill = NULL)
        } else {
          makeGroupedTimeseries(df = .,
                                x = "hit_timestamp",
                                y = "n",
                                ylab = "Count",
                                plot.title = "Success Over Time",
                                fill = NULL)
        }
      } %>%
        printGGplotly(.)
    } else if (input$visitor.success.top.bottom == "top") {
      slice.to = as.numeric(input$visitor.success.select.k)

      top.groups = visitor.success.timeseries.data() %>%
        dplyr::group_by(group_factor) %>%
        dplyr::summarise(n = sum(n)) %>%
        dplyr::arrange(dplyr::desc(n)) %>%  # arrange high to low
        dplyr::slice(1:slice.to)

      visitor.success.timeseries.data() %>%
        dplyr::filter(group_factor %in% top.groups$group_factor) %>%
        {
          if (input$visitor.success.units == "percent") {
            makeGroupedTimeseries(df = .,
                                  x = "hit_timestamp",
                                  y = "percent_success",
                                  ylab = "% of Total",
                                  plot.title = "Success Over Time",
                                  fill = "group_factor")
          } else {
            makeGroupedTimeseries(df = .,
                                  x = "hit_timestamp",
                                  y = "n",
                                  ylab = "Count",
                                  plot.title = "Success Over Time",
                                  fill = "group_factor")
          }
        } %>%
        printGGplotly(.)
    } else {
      slice.to = as.numeric(input$visitor.success.select.k)

      top.groups = visitor.success.timeseries.data() %>%
        dplyr::group_by(group_factor) %>%
        dplyr::summarise(n = sum(n)) %>%
        dplyr::arrange(n) %>%  # arrange low to high
        dplyr::ungroup() %>%
        dplyr::slice(1:slice.to)

      visitor.success.timeseries.data() %>%
        dplyr::filter(group_factor %in% top.groups$group_factor) %>%
        {
          if (input$visitor.success.units == "percent") {
            makeGroupedTimeseries(df = .,
                                  x = "hit_timestamp",
                                  y = "percent_success",
                                  ylab = "% of Total",
                                  plot.title = "Success Over Time",
                                  fill = "group_factor")
          } else {
            makeGroupedTimeseries(df = .,
                                  x = "hit_timestamp",
                                  y = "n",
                                  ylab = "Count",
                                  plot.title = "Success Over Time",
                                  fill = "group_factor")
          }
        } %>%
        printGGplotly(.)

    }
  })

  #### SUCCESS RATE ####
  output$topic.conversions <- renderPlotly({
    grouped.sessions.conversions %>%
      dplyr::filter(., parent_type == 'Topic') %>%
      groupAndOrder(.,
                    group.col = 'parent_title',
                    data.col = 'conversions',
                    percent = input$success.rate.percent,
                    top.pct = .8) %>%
      buildParetoChart(grouped.df = .,
                       x.lab = 'Topics',
                       y.lab = 'Conversions',
                       title = "Conversions on Top Topics",
                       cumul.line = TRUE) %>%
      printGGplotly()
  })

  output$topic.conversion.rate <- renderPlotly({
    plt = grouped.sessions.conversions %>%
      dplyr::filter(., parent_type == 'Topic') %>%
      dplyr::group_by(parent_title) %>%
      dplyr::summarise(.,
                       conversion_rate = round(sum(conversions) / sum(sessions) * 100, 1),
                       c = sum(conversions),
                       s = sum(sessions)) %>%
      dplyr::arrange(., desc(s)) %>%
      dplyr::filter(s > quantile(s, .2)) %>% # Top 80% based on sessions
      ggplot(., aes(x = parent_title, y = conversion_rate)) +
      geom_bar(stat = 'identity') + theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = 'Topics', y = 'Conversion Rate (%)', title = 'Conversion Rate for Top Topics')

    printGGplotly(plt)
  })

  output$topic.sessions <- renderPlotly({
    grouped.sessions.conversions %>%
      dplyr::filter(., parent_type == 'Topic') %>%
      groupAndOrder(.,
                    group.col = 'parent_title',
                    data.col = 'sessions',
                    percent = input$success.rate.percent,
                    top.pct = .8) %>%
      buildParetoChart(grouped.df = .,
                       x.lab = 'Topics',
                       y.lab = 'Sessions',
                       title = "Sessions on Top Topics",
                       cumul.line = TRUE) %>%
      printGGplotly()
  })
})
