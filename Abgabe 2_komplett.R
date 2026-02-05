# AUFGABE 3 -------------------------------------
# ------ Aufgabe 3.1 ------

  siegrunden <- 0     # Zaehle die gewonnenen Runden             
  iterations <- 1000  # Wie oft das Spiel gespielt werden soll
  
  for (i in 1:iterations){
    offene_klappen <- c(1:12)  # Vektor mit den Klappen, die noch umgeklappt werden müssen
    weitermachen <- TRUE       # Soll die eine Runde fortgesetzt oder abgebrochen werden?
  
   
  
    while (weitermachen){ # Solange die Runde fortgesetzt werden soll, ...
    
    augenzahl1<- sample(1:6, 1)
    augenzahl2<- sample(1:6, 1)
    wurf <- augenzahl1 + augenzahl2 # Zufaelliges Wurfergebnis
    wurf  
  # 1. Möglichkeit: Falls der Wurf direkt im Vektor enthalten ist, nimm diese Zahl aus den offenen Klappen heraus
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
              weitermachen <- FALSE # Aktuelle Runde wird abgebrochen und man hat die Runde verloren
             
              break
              
            }
           }
  # 4. Möglichkeit: Der Vektor der zu umstoßenden Klappen ist leer
        if (length(offene_klappen) == 0) { # Falls der Vektor leer ist (keine Zahlen mehr zum umklappen) hat man gewonnen
          siegrunden <- siegrunden + 1     # erhoehe den Siegrundencounter um 1
          weitermachen <- FALSE
         
          break
        }
      }
    }
  

siegrunden / iterations # Wahrscheinlichkeit, das Spiel zu gewinnen


# ------ Aufgabe 3.2 ------

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


#  ------Aufgabe 3.3 ------
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
data_60 # Wir geben die Daten zur Kontrolle aus, nur Gruppe 60

# Teil 3
anyNA(data_60) # Liegen NAs vor?
ids <- which(!complete.cases(data_60)) 
data_60[ids,] # Gib mir alle Zeilen, in denen NAs vorliegen

ids <- which(is.na(data_60$precipitation)) # Alle NAs, die in der Kategorie precipitation sind
mean_precipitation <- mean(data_60$precipitation, na.rm = TRUE) # Mittelwert berechnen
data_60[ids, "precipitation"] <- mean_precipitation # Datensatz überschreiben (NA durch Durchschnitt ersetzen)

# Das Vorgehen wird für alle Attribute, in denen NAs vorliegen, gemacht

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

anyNA(data_60) # Überprüfung: Liegen NAs vor? -> jetzt nicht mehr

# Teil 4
summary(data_60)

        
#Teil 5
count_jan <- subset(data_60, month_of_year == "1")$count #abspeichern eines Vektors mit den counts aus Januar
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
            sum(count_oct), sum(count_nov), sum(count_dec)) # Vektor, der die Summe aller ausgeliehenen Fahrrädern enthält.

which.max(counts) # Abfrage des Maximums
