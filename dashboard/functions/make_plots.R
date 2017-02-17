#### PLOTS ####
makeBlankPlot <- function() {
  # makes a blank plot
  # Args:
  #   none
  # Returns:
  #   a ggplot object
  ggplot(data.frame()) +  # pass an empty data frame
    geom_blank() +
    theme_bw() +
    geom_label() +
    geom_text() +
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    annotate("text", label = "No Data, yet!",x = 50, y = 50, size = 8, colour = "black")
}


makeBreakoutPlot <- function(df, breakouts, x, y, plot.title = "") {
  # makes a timeseries line plot with vertical red bars indicating the date a breakout is detetected
  # Args:
  #   dat = data frame which is the output of detect breakouts in bradford. This df contains point
  #         estimates for user satisfaction as well as a vector indicating standard error for the point
  #         estimate and dates for each
  #   breakouts = a vector of dates for which a breakout was detected
  #   x = the vector to plot along the x axis
  #   y = the vector to plot along the y axis
  #   plot.title = atomic character or factor vector which will be the plot title
  # Returns:
  #   a ggplot object
  if (nrow(df) == 0) {
    makeBlankPlot()
  } else {
    limits = aes(ymax = prop_affirmative + prop_affirmative_se,
                 ymin = prop_affirmative - prop_affirmative_se)
    if (length(breakouts) > 0) {
      plt = ggplot(df, aes_string(x = x, y = y)) +
        geom_vline(xintercept = as.numeric(breakouts), color = "red", linetype = "dashed") +
        geom_line(group = 1) +
        geom_errorbar(limits) +
        theme_bw() +
        xlab("") +
        ggtitle(plot.title) +
        ylab("Proportion Finding Desired Content") +
        scale_x_date()
      return(plt)
    } else {
      plt = ggplot(df, aes_string(x = x, y = y)) +
        geom_line(group = 1) +
        geom_errorbar(limits) +
        theme_bw() +
        ggtitle(plot.title) +
        xlab("") +
        ylab("Proportion Finding Desired Content")
      return(plt)
    }
  }
}

makeVolumeAreaPlot <- function(df, x, y, fill, plot.title = "", xlab = "", ylab = "") {
  # makes a stacked area plot for traffic/response volumes according to a given categorical
  # Args:
  #   df = a data frame containing dates and volumes to plot along with a categorial to fill by
  #   x = the vector to plot along the x axis
  #   y = the vector to plot along the y axis
  #   fill = the categorical which will constitute the "stacks" in the area chart
  #   plot.title = atomic character or factor vector which will be the plot title
  #   xlab = atomic character or factor vector which will be the x axis label
  #   ylab = atomic character or factor vector which will be the y axis label
  # Returns:
  #   a ggplot object
  if (nrow(df) == 0) {
    makeBlankPlot()
  } else {
    df %>%
      ggplot(aes_string(x = x, y = y, fill = fill)) +
      geom_area() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(plot.title) +
      xlab(xlab) +
      ylab(ylab)
  }
}

makeVolumeBarPlot <- function(df, x, y, plot.title = "", xlab = "", ylab = "") {
  # makes a bar plot which shows the volume of a supplied categorial over time
  # Args:
  #   df = a data frame of counts and categorical values
  #   x = the vector to plot along the x axis (should be a vector of dates)
  #   y = the vector to plot along the y axis
  #   plot.title = atomic character or factor vector which will be the plot title
  #   xlab = atomic character or factor vector which will be the x axis label
  #   ylab = atomic character or factor vector which will be the y axis label
  # Returns:
  #   a ggplot object
  if (nrow(df) == 0) {
    makeBlankPlot()
  } else {
    df %>%
      ggplot(aes_string(x = x, y = y)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_bw() +
      ggtitle(plot.title) +
      xlab(xlab) +
      ylab(ylab)
  }
}

makeAffirmativeBarPlot <- function(df, x, y, plot.title = "", xlab = "", ylab = "") {
  # makes a bar plot which shows the volume of a supplied categorial
  # Args:
  #   df = a data frame of counts and categorical values
  #   x = the vector to plot along the x axis (should be a categorical)
  #   y = the vector to plot along the y axis
  #   plot.title = atomic character or factor vector which will be the plot title
  #   xlab = atomic character or factor vector which will be the x axis label
  #   ylab = atomic character or factor vector which will be the y axis label
  # Returns:
  #   a ggplot object
  if (nrow(df) == 0) {
    makeBlankPlot()
  } else {
    df %>%
      ggplot(aes_string(x = x, y = y)) +
      geom_bar(stat = "identity") +
      xlab(xlab) +
      ggtitle(plot.title) +
      ylab(ylab) +
      theme_bw()
  }
}

makeGroupedPareto <- function(df, x, y, cumul.line = NULL, plot.title = "", xlab = "", ylab = "") {
  # makes a bar chart and optionally adds a pareto line
  # Args:
  #   df = a data frame of counts and categorical values
  #   x = the vector of categoricals to plot along the x axis
  #   y = the vector of values to plot along the y axis
  #   cumul.line = the vector of values which are a cumumlative sum of percentages to plot, NULL returns no line
  #   plot.title = the title of the plot to be applied
  #   xlab = the label for the x axis
  #   ylab = the label for the y axis
  # Returns:
  #   a ggplot object
  if (nrow(df) == 0) {
    makeBlankPlot()
  } else if (is.null(cumul.line)) {
    df %>%
      ggplot(aes_string(x = paste0("reorder(", x, ", -", y, ")"), y = y)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      xlab(xlab) +
      ylab(ylab) +
      labs(fill = "",
           color = "") +
      ggtitle(plot.title)
  } else {
    df %>%
      ggplot(aes_string(x = paste0("reorder(", x, ", -", y, ")"), y = y)) +
      geom_line(aes_string(x = paste0("reorder(", x, ", -", y, ")"), y = cumul.line, group = 1)) +
      geom_point(aes_string(x = paste0("reorder(", x, ", -", y, ")"), y = cumul.line)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      xlab(xlab) +
      ylab(ylab) +
      labs(fill = "",
           color = "") +
      ggtitle(plot.title)
  }
}

makeGroupedTimeseries <- function(df, x, y, fill, plot.title = "", xlab = "", ylab = "") {
  # makes a grouped time series chart
  # Args:
  #   df = a data frame of counts, categorical values, and dates
  #   x = the vector of dates to plot along the x axis
  #   y = the vector of values to plot along the y axis
  #   fill = the vector of categorical values which color the lines
  #   plot.title = the title of the plot to be applied
  #   xlab = the label for the x axis
  #   ylab = the label for the y axis
  # Returns:
  #   a ggplot object
  if (nrow(df) == 0) {
    makeBlankPlot()
  } else {
    df %>%
      ggplot(aes_string(x = x, y = y, color = fill, fill = fill)) +
      geom_line(group = 1) +
      geom_point() +
      theme_bw() +
      scale_fill_manual(values = c("#14558f", "#43956f", "#f6c51b", "#535353", "#9C27B0")) +
      scale_color_manual(values = c("#14558f", "#43956f", "#f6c51b", "#535353", "#9C27B0")) +
      xlab(xlab) +
      ylab(ylab) +
      labs(fill = "",
           color = "") +
      ggtitle(plot.title)
  }
}

buildParetoChart <- function(grouped.df, group.col = 'group', data.col = 'total', cumul.col = 'cumul',
                             x.lab = "Groups", y.lab = "Total", title = "TITLE", cumul.line = TRUE) {

  # Draws a bar chart based off grouped data in a specific column displaying the highest value categories descending, includes
  # an option to draw a cumulative traffic line
  #
  # df: dataframe
  # group.col: column that will be grouped along x-axis
  # data.col: numeric column
  # cumul.col: cumulative totals
  # x and y lab: labels for x and y axes
  # percent: Show metrics as % of total
  # cumul.line: Show cumulative line


  # Rename columns
  grouped.df$group = grouped.df[[group.col]]
  grouped.df$total = grouped.df[[data.col]]
  grouped.df$cumul = grouped.df[[cumul.col]]

  # Reorder factors largest to smallest
  grouped.df = transform(grouped.df, group = reorder(group, order(total, decreasing = TRUE)))

  plt = ggplot(grouped.df, aes(x = group, y = total)) +
    geom_bar(stat = "identity", colour = "black") +
    labs(x = paste0(x.lab), title = title, y = y.lab) +
    expand_limits(y = 0) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  if (cumul.line) {
    plt = plt +
      geom_line(aes(x = group, y = cumul, group = 1), colour = "black") +
      scale_colour_manual(values = c("Cumulative Graph"))
  }
  return(plt)
}


#### PLOT HELPERS ####
padXlim <- function(plot.item.count, item.limit = 4,  offset = .5) {
  # conditional logic function to pad xlim values in ggplot plot object creation
  # Args:
  #   plot.item.count = the number of items to plot along a given axis. corresponds to the number of levels in a factor
  #   item.limit = the lower bound beyond which no padding will occur
  #   offset = the value with which to pad the xlim with
  # Returns:
  #   a scalar offset
  if (!any(purrr::map(c(plot.item.count, item.limit, offset), is.numeric))) {
    stop("all args to padXlim must be numeric!")
  }
  if (plot.item.count > item.limit) {
    return(plot.item.count + offset)
  } else {
    return(plot.item.count)
  }
}

printGGplotly <- function(plt) {
  # coerces a ggplot object to a plotly object and prints that objects contents for rendering
  # Args:
  #   plt = a ggplot object to coerce to a plotly object and print
  # Returns:
  #   printed contents of the plotly object
  print(plotly::ggplotly(plt))
}
