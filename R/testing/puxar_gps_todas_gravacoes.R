library('tidyverse')
library('tidylog')

gps_files <- list.files(pattern = '^gps\\.csv$', recursive = TRUE, full.names = TRUE)
gps_files <- map_df(gps_files, read_delim, delim = ',', col_types = 'Tiddd')

write_delim(gps_files, 'gps_todos.csv', delim = ';')