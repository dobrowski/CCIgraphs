

library(tidyverse)
library(janitor)
library(here)
library(scales)
library(ggthemes)
library(MCOE)
library(ggrepel)


con <- mcoe_sql_con()



cci <- tbl(con,"DASH_CCI") %>%
    filter( (cds == "27660680000000"),
            id == "ccidownload2021.txt") %>%
    #         ReportingCategory == "TA",
    #         CharterSchool =="All",
    #         Dass == "All") %>%
    collect() %>%
    select(studentgroup, ends_with("pct")) %>%
    select(studentgroup, ap_pct:two_college_courses_pct) %>%
    na.omit() %>%
    left_join_codebook("DASH_CCI","studentgroup")



cci_graph <- function(var, tit){
    
    cci %>%
        mutate(rate = {{var}}
        ) %>%
        
        ggplot( aes( y = rate, x =fct_reorder(definition, rate) ,  label = round2( rate, 2) )) +
        geom_segment( aes(x=fct_reorder(definition, rate), xend=fct_reorder(definition, rate), y=0, yend=rate),
                      color="orange",
                      size =2 ) +
        geom_point( color="orange", size=5, alpha=0.6) +
        expand_limits(y = c(0,5)) +
        coord_flip() +
        geom_text(size = 3, color = "black") +
        theme_hc() +
        mcoe_theme +
        labs(x = "",
             y = "",
             color ="",
             title = tit, 
             subtitle = "South Monterey County JUHSD - 2021",
             caption = "Source: https://www6.cde.ca.gov/californiamodel/ccireport2021?&year=2021&cdcode=2766068&scode=&reporttype=schools")
    
    ggsave(here("output",paste0(tit,".png")), width = 7, height = 5)    
    
}

cci_graph(ap_pct,"Percentage Passing Two AP Exams")

cci_graph(cte_pct,"Percentage Passing at Least One CTE Pathway")

cci_graph(ag_pct,"Percentage Meeting UC-CSU criteria (A-G)")

cci_graph(ag_cte_pct,"Percentage Passing at Least One CTE Pathway\n and Meeting UC-CSU criteria (A-G)")

cci_graph(ssb_pct,"Percentage Earning State Seal of Biliteracy")

cci_graph(one_college_course_pct,"Percentage Passing One College Course")

cci_graph(two_college_courses_pct,"Percentage Passing Two College Courses")
