# Aufgabe 3.1 -----------------------------------

  siegrunden <- 0      # Zaehle die gewonnenen Runden             
  iterations <- 1000  # Wie oft das Spiel gespielt werden soll
  
  for (i in 1:iterations){
    offene_klappen <- c(1:12)  # Vektor mit den Klappen, die noch umgestossen werden müssen
    weitermachen <- TRUE       # Soll die eine Runde fortgesetzt oder abgebrochen werden?
  
   
  
    while (weitermachen){ # Solange die Runde fortgesetzt werden soll, ...
    
    augenzahl1<- sample(1:6, 1)
    augenzahl2<- sample(1:6, 1)
    wurf <- augenzahl1 + augenzahl2 # Zufaelliges Wurfergebnis
    wurf  
    # 1. Möglichkeit: Falls der Wurf im Vektor enthalten ist, nimm diese Zahl aus den offenen Klappen heraus
        if (wurf %in% offene_klappen){ 
         offene_klappen <- offene_klappen[offene_klappen != wurf]
         weitermachen <- TRUE # Setze die aktuelle Runde oben fort
   
  # 2. Möglichkeit: Schaue, ob man ein Paar aus zwei Zahlen findet, sodass deren Summe dem Wurf entspricht    
          }else {
           
             gefunden <- FALSE 
         
            for (a in offene_klappen){
              b <- wurf - a
            
               if(b %in% offene_klappen && b !=a){                             # Falls das funktioniert...
               offene_klappen <- offene_klappen[!offene_klappen %in% c(a,b)]   # ... entferne die beiden Zahlen
                gefunden <- TRUE                                               # setze Paar gefunden auf ja
                break
               }
             }
  # 3. Möglichkeit: Es wird kein Paar gefunden             
          if (!gefunden) { 
              weitermachen <- FALSE # Aktuelle Runde wird beendet und man bekommt ein Verloren zurück
             
              break
              
            }
           }
  
        if (length(offene_klappen) == 0) { # Falls der Vektor leer ist (keine Zahlen mehr zum umklappen) hat man gewonnen
          siegrunden <- siegrunden + 1     # erhoehe den Siegrundencounter um 1
          weitermachen <- FALSE
         
          break
        }
      }
    }
  

siegrunden / iterations # Wahrscheinlichkeit, das Spiel zu gewinnen


# Aufgabe 3.2 -----------------------------------

palindrom <- function(s) {
  Wort <- strsplit(s, "")[[1]]
  Wortrueckwaerts <- rev(Wort)
  s_Wortrueckwaerts <- paste(Wortrueckwaerts, collapse = "")
  return(s == s_Wortrueckwaerts)
}

palindrom("rentner")
palindrom("comet")


library(testthat)

test_that("palindrom funktioniert korrekt", {
  expect_true(palindrom("rentner"))
  expect_false(palindrom("comet"))
})


# Aufgabe 3.3 ----------------------------------
# Teil 1
data <- read.csv(
  file = "bike_sharing_data_(with_NAs)(6).csv",
  header = TRUE,         
  sep = ",",             
  dec = "."
)

class(data)

# Teil 2
data_60 <- subset(data, group == "60") 
data_60

# Teil 3
anyNA(data_60)
ids <- which(!complete.cases(data_60))
data_60[ids,]

ids <- which(is.na(data_60$precipitation))
mean_precipitation <- mean(data_60$precipitation, na.rm = TRUE)
data_60[ids, "precipitation"] <- mean_precipitation

ids <- which(is.na(data_60$windspeed))
mean_windspeed <- mean(data_60$windspeed, na.rm = TRUE)
data_60[ids, "windspeed"] <- mean_windspeed

ids <- which(is.na(data_60$min_temperature))
mean_min_temperature <- mean(data_60$min_temperature, na.rm = TRUE)
data_60[ids, "min_temperature"] <- mean_min_temperature

ids <- which(is.na(data_60$average_temperature))
mean_average_temperature <- mean(data_60$average_temperature, na.rm = TRUE)
data_60[ids, "average_temperature"] <- mean_average_temperature

ids <- which(is.na(data_60$max_temperature))
mean_max_temperature <- mean(data_60$max_temperature, na.rm = TRUE)
data_60[ids, "max_temperature"] <- mean_max_temperature

ids <- which(is.na(data_60$count))
median_count <- median(data_60$count, na.rm = TRUE)
data_60[ids, "count"] <- median_count

# Teil 4
summary(data_60)

         #nix unplausibles meiner meinung nach


#Teil 5
count_jan <- subset(data_60, month_of_year == "1")$count
count_feb <- subset(data_60, month_of_year == "2")$count
count_mar <- subset(data_60, month_of_year == "3")$count
count_apr <- subset(data_60, month_of_year == "4")$count
count_may <- subset(data_60, month_of_year == "5")$count
count_jun <- subset(data_60, month_of_year == "6")$count
count_jul <- subset(data_60, month_of_year == "7")$count
count_aug <- subset(data_60, month_of_year == "8")$count
count_sep <- subset(data_60, month_of_year == "9")$count
count_oct <- subset(data_60, month_of_year == "10")$count
count_nov <- subset(data_60, month_of_year == "11")$count
count_dec <- subset(data_60, month_of_year == "12")$count
counts <- c(sum(count_jan), sum(count_feb),sum(count_mar),sum(count_apr),sum(count_may),
            sum(count_jun), sum(count_jul), sum(count_aug), sum(count_sep),
            sum(count_oct), sum(count_nov), sum(count_dec))

which.max(counts)

# Aufgabe 4.1 ----------------------------
#siehe Latex

#Aufgabe 4.2 ------------------------
install.packages("ggplot2")
install.packages("dplyr")
install.packages("plotly")
library(ggplot2)
library(dplyr)
library(plotly)

ggplot(data = data_60) +
  geom_point(aes(x = (average_temperature - 32) * 5/9,
                 y = count)) +
  xlab("Mittlere Temperatur (in °C)") +
  ylab("Anzahl ausgeliehener Fahrräder") +
  ggtitle("Anzahl ausgeliehener Fahrräder im Zusammenhang mit der Temperatur") +
  theme_classic()

ggplot(data =  data_60) +
  geom_point(aes(x = precipitation, y = count)) +
  xlab("Niederschlagsmenge (in Zoll)") +
  ylab("Anzahl ausgeliehener Fahrräder") +
  ggtitle("Ausgeliehene Fahrräder im Zusammenhang mit dem Niederschlag") +
  theme_classic()

ggplot(data =  data_60) +
  geom_point(aes(x = windspeed *1.61, y = count)) +
  xlab("Windgeschwindigkeit (in Kilometer pro Stunde)") +
  ylab("Anzahl ausgeliehener Fahrräder") +
  ggtitle("Ausgeliehene Fahrräder im Zusammenhang mit der Windgeschwindigkeit") +
  theme_classic()

ggplot(data =  data_60) +
  geom_line(aes(x = day_of_year, y = count)) +
  xlab("Tag des Jahres") +
  ylab("Anzahl ausgeliehener Fahrräder") +
  ggtitle("Ausgeliehene Fahrräder im Zusammenhang mit dem Tag des Jahres (2023)") +
  theme_classic()

# Aufgabe 4.3 -------------------------------

data_60_rain <- data_60 %>%
  filter(precipitation > 0) %>%
  select(average_temperature, count)

data_60_no_rain <- data_60 %>%
  filter(precipitation == 0) %>%
  select(average_temperature, count)



ggplot(data = data_60_rain) +
  geom_point(aes(x = (average_temperature - 32) * 5/9,
                 y = count)) +
  xlab("Mittlere Temperatur (in °C)") +
  ylab("Anzahl ausgeliehener Fahrräder") +
  ggtitle("Anzahl ausgeliehene Fahrräder bei Regen")+
  theme_classic()

ggplot(data = data_60_no_rain) +
  geom_point(aes(x = (average_temperature - 32) * 5/9,
                 y = count)) +
  xlab("Mittlere Temperatur (in °C)") +
  ylab("Anzahl ausgeliehener Fahrräder") +
  ggtitle("Anzahl ausgeliehene Fahrräder ohne Regen")+
  theme_classic()

# Aufgabe 4.4 ------------

ggplot(data = data_60) +
  geom_histogram(aes(x = count),col="black") +
  xlab("Anzahl ausgeliehener Fahrräder") +
  ylab("Absolute Häufigkeit") +
  ggtitle("Verteilung der Anzahl ausgeliehener Fahrräder")

ggplot(data = data_60) +
  geom_histogram(aes(x = (average_temperature - 32) * 5/9),col="black") +
  xlab("Mittlere Temperatur (in °C)") +
  ylab("Absolute Häufigkeit") +
  ggtitle("Verteilung der mittleren Temperatur")

ggplot(data = data_60) +
  geom_histogram(aes(x = precipitation),col="black") +
  xlab("Niederschlagsmenge (in Zoll)") +
  ylab("Absolute Häufigkeit") +
  ggtitle("Verteilung der Niederschlagsmenge")

ggplot(data = data_60) +
  geom_histogram(aes(x = windspeed *1.61),col="black") +
  xlab("Windgeschwindigkeit (in km/h)") +
  ylab("Absolute Häufigkeit") +
  ggtitle("Verteilung der Windgeschwindigkeit")

# Aufgabe 4.5 -----------------
winter <- data_60 %>%
  filter( month_of_year %in% c(12, 1, 2)) %>%
  select(count)

spring <- data_60 %>%
  filter(month_of_year %in% c(3:5)) %>%
  select(count)

summer <- data_60 %>%
  filter(month_of_year %in% c(6:8)) %>%
  select(count)

autmn <- data_60 %>%
  filter(month_of_year %in% c(9:11)) %>%
  select(count)


fig <- plot_ly() %>% 
  add_histogram(data = winter, x = ~count, name = "Winter", opacity = 0.7, marker = list(color = "black"), xbins = list(size = 2)) %>%
  add_histogram(data = spring, x = ~count, name = "Frühling", opacity = 0.7, marker = list(color = "grey")) %>%
  add_histogram(data = summer, x = ~count, name = "Sommer", opacity = 0.7, marker = list(color = "blue")) %>%
  add_histogram(data = autmn, x = ~count, name = "Herbst", opacity = 0.5, marker = list(color = "orange")) %>%
  layout(barmode = "stack", xaxis = list(title = "Anzahl ausgeliehener Fahrräder"), yaxis = list(title= "ABsolute Häufigkeit"),
         title = "Histogramm der Anzahl ausgeliehener Farbe - sortiert nach Jahreszeit")

fig
# Aufgabe 4.6 ---------------------

plot_ly(
  data = data_60,
  x = ~(average_temperature - 32 )* 5/9,
  y = ~windspeed*1.61,
  z = ~count,
  type = "scatter3d",
  mode = "markers",
  color = ~count
) %>% 
  layout(
  scene = list(
      xaxis = list(title = "Mittlere Temperatur (in °C)"),
      yaxis = list(title = "Windgeschwindigkeit (in km/h)"),
      zaxis = list(title = "Anzahl ausgeliehener Fahrräder")
    )
  )

