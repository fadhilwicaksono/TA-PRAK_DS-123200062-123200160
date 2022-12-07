#Menyetting Kebutuhan Library
install.packages("tidyverse")
install.packages("devtools")
install.packages("cli")
install.packages("e1071")
install.packages("caret")
install.packages("rlang")
install.packages("outliers")


library('janitor') #library bantuan untuk data exploration
library('lubridate') #library untuk parsing date and time
library('skimr') #summary dataframe
library('tidyverse')
library('ggplot2')
library('tidymodels')
library('rsample')
library('devtools')
library('dplyr')
library('recipes')
library('tidyr')
library('e1071')
library('caret')
library('workflows')
library('outliers')
library('shiny')


#Melakukan importing data csv
df_airbnb <- read.csv("D:\\Sem_5\\Praktikum_Data_Science\\Tugas_Akhir_Praktikum_Fixed\\NYC.csv.xls", header=TRUE, stringsAsFactors=FALSE)

head(df_airbnb)

#Data cleaning
#Cleaning missing value
df_airbnb <- df_airbnb %>% 
  distinct() %>%
  clean_names() %>% 
  drop_na() 

#Membuat subset yang lebih kecil sehingga lebih mudah untuk melihat data
airbnb_hood <- df_airbnb %>%
  select(id, name, host_id, neighbourhood_group, neighbourhood, latitude, longitude, room_type, price, number_of_reviews)

head(airbnb_hood)

airbnb_property <- df_airbnb %>%
  select(id, name, host_id, host_name, neighbourhood_group, neighbourhood, price, number_of_reviews, calculated_host_listings_count)

#summary dari dataset
airbnb_hood %>%
  group_by(neighbourhood_group) %>% 
  summarize(min_price = min(price), max_price = max(price), avg_price = mean(price))

#membersihkan harga rumah yang tidak masuk akal atau 0
airbnb_hood <- subset(airbnb_hood, price > 0)

airbnb_property <- subset(airbnb_property, price > 0)

df_airbnb_clean <- subset(df_airbnb, price > 0)

#membuat supervised learning
set.seed(123)
df_airbnb_split <- initial_split(df_airbnb_clean, prop = 0.8, strata = price)

df_airbnb_train <- training(df_airbnb_split)
df_airbnb_test <- testing(df_airbnb_split)

#membuat recipe
airbnb_recipe <- df_airbnb_train %>%
  recipe() %>%
  update_role(price, new_role = "outcome") %>%
  update_role(number_of_reviews, minimum_nights, new_role = "predictor") %>%
  update_role(host_id, new_role = "id") %>%
  step_corr(all_predictors(), threshold = 0.9)

#membuat model
airbnb_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

#melakukan training
airbnb_workflow <- workflow() %>%
  add_recipe(airbnb_recipe) %>%
  add_model(airbnb_model)

airbnb_fit <- airbnb_workflow %>%
  fit(data = df_airbnb_train)

summary(airbnb_fit)

#melakukan prediksi
airbnb_pred <- airbnb_fit %>%
  predict(df_airbnb_test)
bind_cols(df_airbnb_test, airbnb_pred)
airbnb_pred


#melakukan analising data kembali 
airbnb_hood %>%
  group_by(neighbourhood_group, room_type) %>%
  summarize(min_price = min(price), max_price = max(price), avg_price = mean(price))

#Mnngecek relasi dengan cara melakukan grouping neighborhood sehingga dapat melihat mengukur demands dari tiap neighborhood
np_airbnb_hood <- airbnb_hood %>%
  group_by(neighbourhood_group) %>%
  count(room_type)

head(np_airbnb_hood)

#Visualisasi data perbandingan properties dan daerah/hood
ggplot(np_airbnb_hood, aes(x = neighbourhood_group, y = n, fill = room_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  labs(title = "Persebaran Daerah/Hood vs Bentuk Properties in NYC", x = "Hood", y = "Jumlah Properties") +
  theme(plot.title = element_text(face = "bold"))

#Visualisasi data perbandingan daerah dengan harga di NYC
ggplot(airbnb_hood) +
  geom_point(aes(x = neighbourhood_group, y = price, color = room_type)) +
  theme_classic() +
  labs(title = "Persebaran Daerah/Hood vs Harga in NYC", x = "Hood", y = "Harga") +
  theme(plot.title = element_text(face = "bold"))

#Melakukan analisis lebih detail per daerah/hood

#Brooklyn
airbnb_Brooklyn <- airbnb_property %>%
  select(neighbourhood_group, neighbourhood, calculated_host_listings_count, host_id) %>%
  filter(neighbourhood_group == "Brooklyn")

ggplot(airbnb_Brooklyn, aes(x = calculated_host_listings_count, y = neighbourhood,fill = calculated_host_listings_count)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  labs(title = "Airbnb Daerah Brooklyn vs Banyak daftar Host", x = "Daftar Host", y = "Hood") +
  theme(plot.title = element_text(face = "bold"))

#Bronx
airbnb_Bronx <- airbnb_property %>%
  select(neighbourhood_group, neighbourhood, calculated_host_listings_count, host_id) %>%
  filter(neighbourhood_group == "Bronx")


ggplot(airbnb_Bronx, aes(x = calculated_host_listings_count, y = neighbourhood,fill = calculated_host_listings_count)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  labs(title = "Airbnb Daerah Bronx vs Banyak daftar Host", x = "Daftar Host", y = "Hood") +
  theme(plot.title = element_text(face = "bold"))

#Manhattan
airbnb_Manhattan <- airbnb_property %>%
  select(neighbourhood_group, neighbourhood, calculated_host_listings_count, host_id) %>%
  filter(neighbourhood_group == "Manhattan")

ggplot(airbnb_Manhattan, aes(x = calculated_host_listings_count, y = neighbourhood,fill = calculated_host_listings_count)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  labs(title = "Airbnb Daerah Manhattan vs Banyak daftar Host", x = "Daftar Host", y = "Hood") +
  theme(plot.title = element_text(face = "bold"))

#Queens
airbnb_Queens <- airbnb_property %>%
  select(neighbourhood_group, neighbourhood, calculated_host_listings_count, host_id) %>%
  filter(neighbourhood_group == "Queens")

ggplot(airbnb_Queens, aes(x = calculated_host_listings_count, y = neighbourhood,fill = calculated_host_listings_count)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  labs(title = "Airbnb Daerah Queen vs Banyak daftar Host", x = "Daftar Host", y = "Hood") +
  theme(plot.title = element_text(face = "bold"))

#Staten Island
airbnb_StatenIsland <- airbnb_property %>%
  select(neighbourhood_group, neighbourhood, calculated_host_listings_count, host_id) %>%
  filter(neighbourhood_group == "Staten Island")

ggplot(airbnb_StatenIsland, aes(x = calculated_host_listings_count, y = neighbourhood,fill = calculated_host_listings_count)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  labs(title = "Airbnb Daerah Staten Island vs Banyak daftar Host", x = "Daftar Host", y = "Hood") +
  theme(plot.title = element_text(face = "bold"))

#Melakukan analisis trend harga
airbnb_Activity <- df_airbnb %>%
  select(host_name, host_id, price, number_of_reviews, calculated_host_listings_count, minimum_nights, room_type)

ggplot(airbnb_Activity, aes(x = minimum_nights, y = calculated_host_listings_count, fill = price)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  labs(title = "Aktivitas visitor vs List Airbnb", x = "Penyewaan( 12 Bulan terakhir)", y = "listings") +
  theme(plot.title = element_text(face = "bold"))

#Mengidentifikasi persewaan dalam jangka waktu pendek
ggplot(airbnb_Activity, aes(x = minimum_nights, y = calculated_host_listings_count, fill = number_of_reviews)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  labs(title = "Rental Dengan Jangka Pendek", x = "Malam", y = "listings") +
  theme(plot.title = element_text(face = "bold"))

#membuat supervised learning
#membuat data training dan data testing
set.seed(123)
df_airbnb_split <- initial_split(df_airbnb_clean, prop = 0.8, strata = "neighbourhood_group")

df_airbnb_train <- training(df_airbnb_split)
df_airbnb_test <- testing(df_airbnb_split)

#membuat model
airbnb_nb <- naiveBayes(neighbourhood_group ~ ., data = df_airbnb_train)

#melihat hasil prediksi
airbnb_nb %>%
  predict(df_airbnb_test) %>%
  bind_cols(df_airbnb_test) %>%
  select(neighbourhood_group, .pred_class, everything()) %>%
  head()

#melihat akurasi model
airbnb_nb %>%
  predict(df_airbnb_test) %>%
  bind_cols(df_airbnb_test) %>%
  metrics(truth = neighbourhood_group, estimate = .pred_class)

#membuat tampilan shiny
ui <- fluidPage(
  titlePanel("Analisis Airbnb Saat Berwisata Ke New York"),
  sidebarLayout(
    sidebarPanel(
      selectInput("neighbourhood_group", "Daerah", choices = c("Brooklyn", "Bronx", "Manhattan", "Queens", "Staten Island")),
      selectInput("room_type", "Bentuk Properties", choices = c("Private room", "Entire home/apt", "Shared room")),
      sliderInput("price", "Harga", min = 10, max = 10000, value = c(10, 10000)),
      sliderInput("minimum_nights", "Jumlah Malam", min = 1, max = 1250, value = c(1, 1250)),
      sliderInput("number_of_reviews", "Jumlah Review", min = 0, max = 629, value = c(0, 629)),
      sliderInput("calculated_host_listings_count", "Jumlah Daftar Host", min = 1, max = 327, value = c(1, 327))
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)
server <- function(input, output) {
  output$plot1 <- renderPlot({
    airbnb_property <- df_airbnb %>%
      select(neighbourhood_group, neighbourhood, calculated_host_listings_count, host_id, price, number_of_reviews, minimum_nights, room_type) %>%
      filter(neighbourhood_group == input$neighbourhood_group, room_type == input$room_type, price >= input$price[1], price <= input$price[2], minimum_nights >= input$minimum_nights[1], minimum_nights <= input$minimum_nights[2], number_of_reviews >= input$number_of_reviews[1], number_of_reviews <= input$number_of_reviews[2], calculated_host_listings_count >= input$calculated_host_listings_count[1], calculated_host_listings_count <= input$calculated_host_listings_count[2])
    
    ggplot(airbnb_property, aes(x = calculated_host_listings_count, y = neighbourhood,fill = calculated_host_listings_count)) +
      geom_bar(position = "dodge", stat = "identity") +
      theme_classic() +
      labs(title = "Airbnb Daerah vs Banyak daftar Host", x = "Daftar Host", y = "Hood") +
      theme(plot.title = element_text(face = "bold"))
  })
}
output <- shinyApp(ui = ui, server = server)
output

