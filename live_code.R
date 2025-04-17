# coffee and live(ish) code

library(tidyverse)

download.file("https://files.digital.nhs.uk/A0/A7EED3/Appointments_GP_Daily_CSV_Feb_25.zip",
          destfile = "data.zip")

zipped_csv_names <- grep('\\.csv$',
                         unzip('data.zip',
                               list = TRUE)$Name,
                         ignore.case = TRUE,
                         value=TRUE)

zipped_csv_names <- zipped_csv_names[startsWith(zipped_csv_names, 'SUB')]

unzip('data.zip', files = zipped_csv_names)

df <- zipped_csv_names |>
  lapply(read_csv) |>
          bind_rows()

view(head(df,100))

library(janitor)

df <- clean_names(df)

summary(df)

unique(df$sub_icb_location_name)

icbs <- c("NHS Nottingham and Nottinghamshire ICB - 52R",
          "NHS Devon ICB - 15N" )

df<- df |>
  filter (sub_icb_location_name %in% icbs) |>
  mutate(appointment_date = as.Date(appointment_date, '%d%b%Y'),
         icb = sub_icb_location_name) |>
  select(-starts_with('sub'),
         -contains('code'))

summary(df)

month_sum <- df |>
  filter(appt_status == 'Attended',
         hcp_type == 'Other Practice staff') |>
  mutate(month = floor_date(appointment_date, 'month')) |>
  summarise(total_appts_brk = sum(count_of_appointments),
             .by = c('time_between_book_and_appt',
                     'month',
                     'icb')) |>
  mutate(total_appts = sum(total_appts_brk),
         perc = total_appts_brk / total_appts * 100,
         .by = c('month',
                 'icb')) |>
  filter(time_between_book_and_appt == 'Same Day')

month_sum |>
  ggplot() +
  aes(x = month,
      y = total_appts,
      colour = icb) +
  geom_line() +
  theme_minimal()


















library(forecast)


seasonal_trend <- df |>
  filter(icb ==  "NHS Devon ICB - 15N" ,
         hcp_type == 'GP')  |>
  mutate(month = floor_date(appointment_date, 'month')) |>
  summarise(total_appts = sum(count_of_appointments),
            .by = c('month'))

seasonal_trend_ts <- ts(seasonal_trend[, -1],
                        frequency = 12,
                        start = c(2022, 9))

season_dec <- decompose(seasonal_trend_ts)  

autoplot(season_dec)

##############


month_sum <- df |>
  filter(appt_status == 'Attended',
         hcp_type == 'Other Practice staff') |>
  mutate(month = floor_date(appointment_date, 'month')) |>
  summarise(total_appts_brk = sum(count_of_appointments),
            .by = c('time_between_book_and_appt',
                    'month',
                    'icb')) |>
  mutate(total_appts = sum(total_appts_brk),
         perc = total_appts_brk / total_appts * 100,
         .by = c('month',
                 'icb')) 

month_sum_bar <- month_sum |>
  filter(month == max(month)) |>
  mutate(time_between_book_and_appt = factor(time_between_book_and_appt,
                                             levels = c( "Unknown / Data Quality",
                                                         "More than 28 Days"  ,
                                                         "22  to 28 Days"  ,
                                                         "15  to 21 Days"  ,
                                                         "8  to 14 Days"    ,
                                                         "2 to 7 Days"    ,
                                                         "1 Day" ,
                                                         "Same Day"  )))

month_sum_bar |>
  ggplot() +
  aes(x = perc,
      y = time_between_book_and_appt,
      fill = icb) + 
  geom_col(position = position_dodge())


month_sum_bar |> ggplot() +
  aes(x = perc,
      y = time_between_book_and_appt,
      fill = icb) +
  geom_col(position = position_dodge()) +
  geom_text(position = position_dodge(width= 1), 
            aes(x=perc+0.2,
                label=paste0(round(perc,1), '%'), 
                hjust=0)) +
  xlim(0, max(month_sum_bar$perc) + 5) +
  theme_void() +
  theme(axis.text.y= element_text(month_sum_bar$time_between_book_and_appt, 
                                  hjust = 1),
        legend.position="bottom") +
  labs(title = paste0('Breakdown of GP appointment timeliness: ', 
                      format(max(month_sum_bar$month), '%B %y')),
       subtitle = paste0(unique(month_sum_bar$icb), 
                         collapse = " "),
       caption = paste0('Data downloaded: ', 
                        format(Sys.Date(), 
                               '%Y/%m/%d'))) +
  coord_cartesian(expand = TRUE) 
  


