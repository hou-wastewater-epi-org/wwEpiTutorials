## f is a data 
fplot <- function(f, title_char, points=F){
  if(points){
  x <- {ggplot(f, aes(x = date, y = est, color = name, fill = name)) +
      theme_minimal()+
      
      geom_line(linewidth=2) +
      
      geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=.2) +
      
      scale_color_manual(values = c("#D96D2C", "#17266D", "#00898C", "#5A319F", "#59580C")) +
      
      scale_fill_manual(values = c("#F2A879", "#7C8FF2", "#00FBDE", "#B794F2", "#D9D75F"), guide = "none") +
      
      #ylim(2, 8) + 
      
      labs(title = paste(title_char), x= "Date", y = "Log10 Copies/L-WW", color = "") +
      new_scale("shape")+ 
      new_scale_color()+
      geom_point(aes(x=date, y=obs, shape = ts_missing, color = ts_missing)) +
      scale_shape_manual(values = c(8,19))+
      scale_color_manual(values = c("#69dd58", "#69dd58"))+
      labs(color = "", shape = "") +
      geom_rect(data = data.frame(xmin = unique(f$date[which(f$ts_missing)]), 
                                  xmax = unique(f$date[which(f$ts_missing) + 1])[1:length(unique(f$date[which(f$ts_missing)]))]), 
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "grey", alpha = 0.1)
    
    }
  }
  if(!points){
    x <- {ggplot(f, aes(x = date, y = est, color = name, fill = name)) +
        theme_minimal()+
        
        geom_line(linewidth=2) +
        
        geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=.2) +
        
        scale_color_manual(values = c("#D96D2C", "#17266D", "#00898C", "#5A319F", "#59580C")) +
        
        scale_fill_manual(values = c("#F2A879", "#7C8FF2", "#00FBDE", "#B794F2", "#D9D75F"), guide = "none") +
        
        #ylim(2, 8) + 
        
        labs(title = paste(title_char), x= "Date", y = "Log10 Copies/L-WW", color = "")  +
        geom_rect(data = data.frame(xmin = unique(f$date[which(f$ts_missing)]), 
                                    xmax = unique(f$date[which(f$ts_missing) + 1])[1:length(unique(f$date[which(f$ts_missing)]))]), 
                  aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "grey", alpha = 0.1)
      
        }
  }
  return(x)
  
}
