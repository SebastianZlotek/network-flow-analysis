# Administracja systemów rozproszonych

#Instalacja bibliotek
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("dplyr")  
install.packages("tidyverse") 
install.packages("scales")
install.packages("installr")
install.packages("ggvis")
install.packages("ggpubr")
install.packages("corrplot")
install.packages("heatmaply")
install.packages("plotrix")

#Potrzebne biblioteki
library("plotrix")
library("heatmaply")
library("corrplot")
library("ggpubr")
library('ggvis')
library("dplyr") 
library("tidyverse") 
library("ggplot2")
library("RColorBrewer")

options(scipen=999) #usuwamy postać wykładniczą

setwd("C:/Users/Sebastian/Desktop/Studia/II rok 2 semestr/ASR_PROJEKT") #ustawiamy folder główny

netflowds <- read.csv("Train_Test_Network.csv", encoding = "UTF-8") #wczytujemy plik csv

netflowds

summary(netflowds) #podsumowanie całej ramki danych

(netflowds3 <-netflowds[netflowds$`dns_query`!="(empty)" & netflowds$`dns_query`!="-",]) #wykluczanie niepotrzebnych linii

sorttowanie <- (sort(table(netflowds3$'dns_query'),DECREASING=F)) #wybieramy najczęściej używane strony internetowe

paletabarw <- brewer.pal(6, "Set2") #kolory oraz opcje wykresu najczęściej używanych stron
(takiwykres <- pie(tail(sorttowanie),
                       border="white" ,
                       main = "Application awareness",
                       col = paletabarw))
dev.off() 

sorttowanie2 <- (sort(table(netflowds$'proto'),DECREASING=F)) #wybieramy najczęściej używane protokoły

paletabarw <- brewer.pal(3, "Set2") #kolory oraz opcje wykresu najczęściej używanych protokołów
(takiwykres <- pie(tail(sorttowanie2),
                   border="white" ,
                   main = "Najczęściej używane protokoły",
                   col = paletabarw))

dev.off() #zapisanie wykresu

mean(netflowds$'duration') #średnie, sumy oraz odchylenia standardowe utraconych bajtów i czasu trwania połączenia

mean(netflowds$'missed_bytes')

sd(netflowds$'duration')

sd(netflowds$'missed_bytes')

sum(netflowds$'duration')

sum(netflowds$'missed_bytes')

#średnia zużytych bajtów
bytesperipmean <- (aggregate(netflowds$'missed_bytes', by=list(netflowds$'dst_ip'), FUN = mean))

bytesperipmeansorted <- bytesperipmean[with(bytesperipmean, order(x, Group.1, decreasing = T)), ]

#odchylenie standardowe zużytych bajtów
bytesperipsd <- (aggregate(netflowds$'missed_bytes', by=list(netflowds$'dst_ip'), FUN = sd))

bytesperipsdsorted <- bytesperipsd[with(bytesperipsd, order(x, Group.1, decreasing = T)), ]

#suma zużytych bajtów
bytesperip <- (aggregate(netflowds$'missed_bytes', by=list(netflowds$'dst_ip'), FUN = sum))

bytesperipsorted <- bytesperip[with(bytesperip, order(x, Group.1, decreasing = T)), ]

bytesperipsorted <- bytesperipsorted %>% #zaokrąglenie i zamiana bajtów na megabajty
  mutate(inmb = round((bytesperipsorted$x / 1048576), 2))

bytesperipsorted$x <- NULL

TES1 <- head(bytesperipsorted, n = 10)

TES2 <- head(bytesperipsorted, n = 3)

TES3 <- tail(TES1, n = 7)

str(bytesperipsorted)

ggplot(TES1, aes(Group.1, inmb)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle=65, vjust=0.6)) #top 10 zużycia pasma po IP

dev.off()

ggplot(TES2, aes(Group.1, inmb)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle=65, vjust=0.6)) #top 3 zużycia pasma po IP

dev.off()

ggplot(TES3, aes(Group.1, inmb)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle=65, vjust=0.6)) #porównanie zużytych megabajtów dla ip poza top 3 

dev.off()

View(head(bytesperipsorted, n = 50))

src_pkts <- netflowds %>% #informacje o pakietach, bajtach i długości połączenia
  select(src_pkts, dst_pkts, src_bytes, dst_bytes, duration,  missed_bytes, src_ip_bytes, dst_ip_bytes ) %>% 
  filter(src_pkts >0 & dst_pkts >0 )

cor(src_pkts$src_pkts,src_pkts$dst_pkts, method="spearman") #korelacja pakietów

length(src_pkts$src_pkts)
length(src_pkts$dst_pkts)

#wykresy korelacji
corrplot(corr = cor(src_pkts), method='number') #heatmapa korelacji

str(netflowds)

r <- cor(src_pkts) #dokładniejszy opis wykresu i wartości korelacji
cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}
p <- cor.test.p(src_pkts)

heatmaply_cor(
  r,
  node_type = "scatter",
  point_size_mat = p,
  point_size_name = "p value",
  label_names = c("x", "y", "Correlation")
)

#----------------------------------------------

koripbytes <- cor.test(src_pkts$src_ip_bytes, src_pkts$dst_ip_bytes, method="pearson") #sprawdzanie czy wartości korelacji są poprawne
koripbytes

korpkts <- cor.test(src_pkts$src_pkts, src_pkts$dst_pkts, method="pearson")

korpkts

korbytes <- cor.test(src_pkts$src_bytes, src_pkts$dst_bytes, method="pearson")

korbytes

kordest <- cor.test(src_pkts$duration, src_pkts$dst_bytes, method="pearson")

kordest

#sprawdzenie najczęstszych typów ataków na dany adres IP
type1 <- netflowds[netflowds$`type`!="normal",]

typeanalysis <- (sort(table(type1$'type'),DECREASING=F))

ggplot(data.frame(typeanalysis), aes(Var1, Freq)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle=65, vjust=0.6)) + scale_y_continuous(breaks=seq(0,20000,by=1000))

dev.off()

typeanalysis2 <- (sort(table(netflowds$'type'),DECREASING=F))

ggplot(data.frame(typeanalysis2), aes(Var1, Freq)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle=65, vjust=0.6)) + scale_y_continuous(breaks=seq(0,300000,by=20000))

dev.off()

#sprawdzenie czy dane połączenie powiodło się, gdzie:
#S0 połączenie bez informacji zwrotnej
#S1 połączenie powiodło się
#REJ próba połączenia została odrzucona
status_polaczenia <- (sort(table(netflowds$'conn_state'),DECREASING=F))

view(status_polaczenia)

stat_data <- data.frame(status_polaczenia)

namedata <- stat_data %>%
  select(Var1) %>%
  filter((Var1 == "S0") | (Var1 == "S1") | Var1 == "REJ") #wybieranie danych kolumn i wierszy

freqdata <- stat_data %>%
  filter((Var1 == "S0") | (Var1 == "S1") | Var1 == "REJ") %>%
  select(Freq)

podsumowanie_polaczen <- stat_data %>% #spis polaczen
  select(Var1,Freq)%>% 
  filter(Var1 == "S0" | Var1=="S1" | Var1=="REJ")

pie3D(as.numeric(unlist(freqdata)),labels = as.character(unlist(namedata)),explode = 0.1, main = "Status połączeń ")

dev.off()






















































































