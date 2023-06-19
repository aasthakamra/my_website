# my_website

library(blogdown)
blogdown::install_hugo()
blogdown::hugo_version()

blogdown::new_site(theme = "MarcusVirg/forty", 
                   sample = TRUE, 
                   theme_example = TRUE,            
                   empty_dirs = TRUE,            
                   to_yaml = TRUE)

