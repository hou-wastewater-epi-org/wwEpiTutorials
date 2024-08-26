## f is a data 
fplot <- function(f, title_char, points=F) {
  missing_dates <- unique(f$date[which(f$ts_missing)])
  
  # Check if there are any missing dates
  if (length(missing_dates) > 0) {
    rect_data <- data.frame(
      xmin = missing_dates,
      xmax = c(missing_dates[-1], NA) # Shift the xmin to get xmax
    )
    # Remove last row if xmax is NA
    rect_data <- rect_data[!is.na(rect_data$xmax),]
  } else {
    rect_data <- NULL
  }
  
  x <- ggplot(f, aes(x = date, y = est, color = name, fill = name)) +
    theme_minimal() +
    geom_line(linewidth=2) +
    geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=.2) +
    scale_color_manual(values = c("#D96D2C", "#17266D", "#00898C", "#5A319F", "#59580C")) +
    scale_fill_manual(values = c("#F2A879", "#7C8FF2", "#00FBDE", "#B794F2", "#D9D75F"), guide = "none") +
    labs(title = paste(title_char), x= "Date", y = "Log10 Copies/L-WW", color = "")
  
  if (!is.null(rect_data)) {
    x <- x + geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
                       inherit.aes = FALSE, fill = "grey", alpha = 0.1)
  }
  
  if (points) {
    x <- x + new_scale("shape") +
      new_scale_color() +
      geom_point(aes(x=date, y=obs, shape = ts_missing, color = ts_missing)) +
      scale_shape_manual(values = c(8,19)) +
      scale_color_manual(values = c("#69dd58", "#69dd58")) +
      labs(color = "", shape = "")
  }
  
  return(x)
}

