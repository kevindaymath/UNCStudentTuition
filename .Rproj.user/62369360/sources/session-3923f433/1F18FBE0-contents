library(tidyverse)
library(DT)
data <- read.csv("data.csv")
data$total <- data$tuition + data$fees

data %>%
  filter(year == "2022-23")


sketch <- htmltools::withTags(table(
  class = "display",
  thead(
    tr(
      th(colspan = 3, "In-State", style = "border-right: solid 1px; border-bottom: 0;"),
      th(colspan = 3, "Out-of-State", style = "border-bottom: 0;") 
    ),
    tr(
      mapply(th, 
             rep(c("Tuition", "Fees", "Total"), 2), 
             style = sprintf("border-right: solid %dpx; border-bottom: 0;", 
                             c(0,0,1,0,0,0)), 
             SIMPLIFY = FALSE)
    )
  )
))

table1 <- data %>%
  filter(year == "2022-23", education_level == "Graduate") %>%
  mutate(across(c("tuition", "fees", "total"), ~ paste0("$", formatC(as.numeric(.x), format="f", digits=0, big.mark=",")))) %>% 
  pivot_wider(names_from = "in_out_state", 
              values_from = c("tuition", "fees", "total")) %>%
  select(3, 5, 7, 4, 6, 8)

datatable(table1, container = sketch, rownames = FALSE, options = list(dom = 't', ordering=F, columnDefs = 
                                                                         list(list(className = 'dt-center', 
                                                                                   targets = "_all")))) %>% 
  formatStyle(columns = names(table1), backgroundColor = "#FFFFFF") %>% 
  formatStyle(c(3), `border-right` = "solid 1px")



  {data %>%
  filter(education_level == "Graduate", in_out_state == "In-State") %>% 
  mutate(label = paste0("In <b>", year, "</b>", "the charge type ")) %>% 
  ggplot(aes(x=year, y=total, group=1)) +
  
  geom_line()+
  geom_point(aes(text=label)) +
  
  theme_bw() +
    theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
      axis.title.y=element_blank()
    ) 
  } %>% 
  ggplotly(tooltip = "label")  %>%
  
  config(displayModeBar = F) %>% 
  layout(hoverlabel=list(bgcolor="white"))
 

ggplotly(p1, tooltip = "label") %>%
  
  config(displayModeBar = F) %>% layout(hoverlabel=list(bgcolor="white"))


breakout <- read.csv("breakout.csv")
breakout$category <- factor(breakout$category, levels = rev(unique(breakout$category)))
{breakout %>%
    filter(education_level == "Graduate", year == "2022-23") %>% 
    ggplot(aes(x=fees, y=category)) +
    geom_bar(stat = "identity", fill = "#5C86A7")+
    geom_text(aes(label = fees), hjust = -0.1, nudge_x = 0.5) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(expand = c(.01, .01)) +
    scale_fill_identity(guide = "none") +
    # scale_fill_identity(guide = "none") +
    # coord_flip(clip = "off")+
    theme_minimal() +
    theme(
      axis.title.x=element_blank(),
          # axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          plot.margin = margin(15, 30, 15, 15)
    ) 
  } 


