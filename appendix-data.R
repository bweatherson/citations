the_cat <- topics_big_table$cat_num[jjj]
target_articles <- round_three %>% filter(cat_num == the_cat)
active_precursors <- precursors %>% filter(cat_num == the_cat)
category_properties <- category_years %>% filter(cat_num == the_cat)

cat(" \n\n")
cat("## ",category_properties$cat_real_name[1], "{-#t-",category_properties$cat_shortcode,"}",sep="")  

cat(" \n\n")
cat("**Field**: ",category_properties$cat_field, " \n <br/>", sep = "")
cat("**Articles**: ",round(category_properties$n[1], 1), sep = "")

temp_dt <- datatable(
  category_dt %>% 
    filter(Topic == category_properties$cat_real_name[1]) %>% 
    select(-Topic) %>% 
    rename(Cites = Citations, Details = Bib),
  rownames = FALSE,
  filter = 'top',
  options = list(
    columnDefs = list(list(
      className = 'dt-left', targets = 0:6
    )),
    pageLength = 10,
    search = list(regex = TRUE, caseInsensitive = FALSE)
  ),
  caption = htmltools::tags$caption("List of papers and categories", style = "font-weight: bold")
) %>%
  formatStyle(1:7, `text-align` = 'left')
