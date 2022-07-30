library(dplyr)
library(readxl)
library(epiDisplay)
library(lmtest)
library(sandwich)
library(mfx)
library(margins)
library(stargazer)
library(pscl)
library(car)
library(ROCR)
library(caTools)
library(AICcmodavg)
library(scorecard)

#wczytywanie danych
dane <- read_xlsx("lokalizacja pliku")


#rekodowanie zmiennych

dane$population_chart <- dane$population
dane$population_chart[dane$population_chart ==2]<-1
dane$population_chart[dane$population_chart ==3]<-1
dane$population_chart[dane$population_chart ==4]<-1

#usuwanie obserwacji odstaj¹cych dla zmiennej p³eæ
dane <- dane[!dane$age>30,]

#rekodowanie zmiennej status zatrudnienia
dane <- dane[!dane$employment==2,]
dane <- dane[!dane$employment==3,]
tab1(dane$employment)
#0-uczeñ/student
#1-pracuj¹cy

#rekodowanie zmiennej "wykszta³cenie"
dane$education_clean <- dane$education
dane$education_clean[dane$education_clean  == 0] <- 1
dane$education_clean[dane$education_clean  == 2] <- 1
dane$education_clean[dane$education_clean  == 1] <- 0
dane$education_clean[dane$education_clean  == 3] <- 1
tab1(dane$education_clean)
#0-podstawowe/zawodowe/œrednie
#1-wy¿sze

#rekodowanie zmiennej "choroby przewlek³e"
dane$diseases_clean <- dane$diseases
dane$diseases_clean[dane$diseases_clean == 0] <- 1
dane$diseases_clean[dane$diseases_clean == 5] <- 0
dane$diseases_clean[dane$diseases_clean == 4] <- 1
dane$diseases_clean[dane$diseases_clean == 3] <- 1
dane$diseases_clean[dane$diseases_clean == 2] <- 1
dane$diseases_clean[dane$diseases_clean == 1] <- 1
tab1(dane$diseases_clean)
#0-nie choruje przewlekle
#1-choruje przewlekle

#rekodowanie zmiennej "Ÿród³o czerpania informacji"
dane$information_clean <- dane$information
dane$information_clean[dane$information_clean == 0] <- 3
dane$information_clean[dane$information_clean == 1] <- 0
dane$information_clean[dane$information_clean == 2] <- 1
dane$information_clean[dane$information_clean == 3] <- 2
tab1(dane$information_clean)
#0-radio
#1-telewizja
#2-internet

#rozszerzenie zmiennej "Ÿród³o czerpania informacji" na zmienne binarne
#kategoria "radio"
dane$radio <- dane$information_clean
dane$radio[dane$radio == 0] <- "X"
dane$radio[dane$radio == 1] <- 0
dane$radio[dane$radio == 2] <- 0
dane$radio[dane$radio == "X"] <- 1
tab1(dane$radio)

#kategoria "telewizja"
dane$telewizja <- dane$information_clean
dane$telewizja[dane$telewizja == 1] <- "X"
dane$telewizja[dane$telewizja == 0] <- 0
dane$telewizja[dane$telewizja == 2] <- 0
dane$telewizja[dane$telewizja == "X"] <- 1
tab1(dane$telewizja)

#kategoria "internet"
dane$internet <- dane$information_clean
dane$internet [dane$internet  == 2] <- "X"
dane$internet [dane$internet  == 0] <- 0
dane$internet [dane$internet  == 1] <- 0
dane$internet [dane$internet  == "X"] <- 1
tab1(dane$internet )

#rekodowanie zmiennej "przemieszczanie siê"
dane$transport_clean <- dane$transport
dane$transport_clean[dane$transport_clean == 1] <- 0
dane$transport_clean[dane$transport_clean == 2] <- 1
dane$transport_clean[dane$transport_clean == 3] <- 2
tab1(dane$transport_clean)
#0-pieszo
#1-komunikacja miejska
#2-samochód

#rozszerzenie zmiennej "przemieszczanie siê" na zmienne binarne
#kategoria "pieszo"
dane$pieszo <- dane$transport_clean
dane$pieszo [dane$pieszo  == 0] <- "X"
dane$pieszo [dane$pieszo  == 1] <- 0
dane$pieszo [dane$pieszo  == 2] <- 0
dane$pieszo [dane$pieszo  == "X"] <- 1

#kategoria "transport miejski"
dane$transport_miejski <- dane$transport_clean
dane$transport_miejski [dane$transport_miejski  == 1] <- "X"
dane$transport_miejski [dane$transport_miejski  == 0] <- 0
dane$transport_miejski [dane$transport_miejski  == 2] <- 0
dane$transport_miejski [dane$transport_miejski  == "X"] <- 1

#kategoria "samochód"
dane$samochod <- dane$transport_clean
dane$samochod [dane$samochod  == 2] <- "X"
dane$samochod [dane$samochod  == 0] <- 0
dane$samochod [dane$samochod  == 1] <- 0
dane$samochod [dane$samochod  == "X"] <- 1


#rekodowanie zmiennej "u¿ywki"
dane$drugs_clean <- dane$drugs
dane$drugs_clean[dane$drugs_clean == 0] <- 1
dane$drugs_clean[dane$drugs_clean == 2] <- 1
dane$drugs_clean[dane$drugs_clean == 3] <- 1
dane$drugs_clean[dane$drugs_clean == 99] <- 0

#rozszerzenie zmiennej "miejsce zamieszkania" na zmienne binarne
#kategoria "wieœ"
dane$wies <- dane$population
dane$wies[dane$wies == 0] <- "TAK"
dane$wies[dane$wies == 1] <- 0
dane$wies[dane$wies == 2] <- 0
dane$wies[dane$wies == 3] <- 0
dane$wies[dane$wies == 4] <- 0
dane$wies[dane$wies == "TAK"] <- 1

#kategoria "miasto z liczb¹ ludnoœci do 20 tys. mieszkañców"
dane$miasto_do_20tys <- dane$population
dane$miasto_do_20tys[dane$miasto_do_20tys == 1] <- "TAK"
dane$miasto_do_20tys[dane$miasto_do_20tys == 4] <- 0
dane$miasto_do_20tys[dane$miasto_do_20tys == 2] <- 0
dane$miasto_do_20tys[dane$miasto_do_20tys == 3] <- 0
dane$miasto_do_20tys[dane$miasto_do_20tys == "TAK"] <- 1

#kategoria "miasto z liczb¹ ludnoœci od 20 do 50 tys. mieszkañców"
dane$miasto_20tys_50tys <- dane$population
dane$miasto_20tys_50tys[dane$miasto_20tys_50tys == 2] <- "TAK"
dane$miasto_20tys_50tys[dane$miasto_20tys_50tys == 1] <- 0
dane$miasto_20tys_50tys[dane$miasto_20tys_50tys == 4] <- 0
dane$miasto_20tys_50tys[dane$miasto_20tys_50tys == 3] <- 0
dane$miasto_20tys_50tys[dane$miasto_20tys_50tys == "TAK"] <- 1
tab1(dane$miasto_20tys_50tys)

#kategoria "miasto z liczb¹ ludnoœci od 50 do 100 tys. mieszkañców"
dane$miasto_50tys_100tys <- dane$population
dane$miasto_50tys_100tys[dane$miasto_50tys_100tys == 3] <- "TAK"
dane$miasto_50tys_100tys[dane$miasto_50tys_100tys == 1] <- 0
dane$miasto_50tys_100tys[dane$miasto_50tys_100tys == 2] <- 0
dane$miasto_50tys_100tys[dane$miasto_50tys_100tys == 4] <- 0
dane$miasto_50tys_100tys[dane$miasto_50tys_100tys == "TAK"] <- 1
tab1(dane$miasto_50tys_100tys)

#kategoria "miasto z liczb¹ ludnoœci powy¿ej 100 tys. mieszkañców"
dane$miasto_pow_100tys <- dane$population
dane$miasto_pow_100tys[dane$miasto_pow_100tys == 4] <- "TAK"
dane$miasto_pow_100tys[dane$miasto_pow_100tys == 1] <- 0
dane$miasto_pow_100tys[dane$miasto_pow_100tys == 2] <- 0
dane$miasto_pow_100tys[dane$miasto_pow_100tys == 3] <- 0
dane$miasto_pow_100tys[dane$miasto_pow_100tys == "TAK"] <- 1
tab1(dane$miasto_pow_100tys)






#estymacja modeli dwumianowych
options(scipen = 999) #wy³¹cza notacjê naukow¹

#przekszta³camy zmienn¹ objaœnian¹ z wielowariantowej na binarn¹. 
#Kategoriê 2- nie wiem czy chorowa³em/ chorowa³am do³¹czamy do kategorii 1- chorowa³em/ chorowa³am
dane$Y_1 <- dane$Y
dane$Y_1[dane$Y_1  == 2] <- 1

#specyfikacja pe³na
model_pe³ny <- glm( Y_1 ~ sex + age + education_clean + employment + miasto_do_20tys + 
miasto_20tys_50tys + miasto_50tys_100tys + miasto_pow_100tys + 
drugs_clean + telewizja + internet + samochod + transport_miejski + 
sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
family = "binomial", data = dane, maxit = 100)

#³¹czna istotnoœæ statystyczna (test LR)
#model z restrykcjami (bez zmiennej "miejsce zamieszkania")
model_pe³ny_restricted_miejsce_zamieszkania <- glm(Y_1 ~ sex + age + education_clean + employment + 
                                drugs_clean + telewizja + internet + samochod + transport_miejski + 
                                sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                                family = binomial(link = "logit"), data = dane)

#model z restrykcjami (bez zmiennej "Ÿród³o informacji")
model_pe³ny_restricted_Ÿród³o_informacji<- glm(Y_1 ~ sex + age + education_clean + employment + miasto_do_20tys + 
                                                 miasto_20tys_50tys + miasto_50tys_100tys + miasto_pow_100tys +
                                                 drugs_clean + samochod + transport_miejski + 
                                                 sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                                               family = binomial(link = "logit"), data = dane)

#model z restrykcjami (bez zmiennej "przemieszczanie siê")
model_pe³ny_restricted_przemieszczanie_sie <- glm(Y_1 ~ sex + age + education_clean + employment + miasto_do_20tys + 
                                                 miasto_20tys_50tys + miasto_50tys_100tys + miasto_pow_100tys +
                                                 drugs_clean + telewizja + internet + 
                                                 sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                                               family = binomial(link = "logit"), data = dane)

lrtest(model_pe³ny, model_pe³ny_restricted_miejsce_zamieszkania)
#brak podstaw do odrzucenia hipotezy zerowej. Brak istotnego wp³ywu zmiennej na Y
lrtest(model_pe³ny, model_pe³ny_restricted_Ÿród³o_informacji)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y
lrtest(model_pe³ny, model_pe³ny_restricted_przemieszczanie_sie)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y

#estymacja odpornych wartoœci statystyki Z
coeftest(model_1.1, vcov = vcovHC(model_1.1, type="HC"))


#miary dopasowania modelu g³ównego
pR2(model_pe³ny)
AIC(model_pe³ny)
BIC(model_pe³ny)

#Model z wy³¹cznie istotnymi statystycznie zmiennymi (proces konstrukcji)
#redukcja o zmienne dotycz¹ce miejsca zamieszkania (brak podstaw do odrzucenia hipotezy zerowej w teœcie ³¹cznej istotnoœci (LR test))
model_1.2 <- glm( Y_1 ~ sex + education_clean + employment + 
                        drugs_clean +telewizja + internet + samochod + transport_miejski + 
                        sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                        family = "binomial", data = dane, maxit = 100)

#³¹czna istotnoœæ statystyczna (test LR)
#model z restrykcjami (bez zmiennej "Ÿród³o informacji")
model_1.2_restricted_Ÿród³o_informacji<- glm(Y_1 ~ sex + age + education_clean + employment +
                                                 drugs_clean + samochod + transport_miejski + 
                                                 sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                                               family = binomial(link = "logit"), data = dane)

#model z restrykcjami (bez zmiennej "przemieszczanie siê")
model_1.2_restricted_przemieszczanie_sie <- glm(Y_1 ~ sex + age + education_clean + employment +
                                                    drugs_clean + telewizja + internet + 
                                                    sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                                                  family = binomial(link = "logit"), data = dane)

lrtest(model_1.2, model_1.2_restricted_Ÿród³o_informacji)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y
lrtest(model_1.2, model_1.2_restricted_przemieszczanie_sie)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y


#redukcja o p³eæ
model_1.3 <- glm( Y_1 ~ education_clean + employment + 
                        drugs_clean +telewizja + internet + samochod + transport_miejski + 
                        sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                        family = "binomial", data = dane, maxit = 100)

#³¹czna istotnoœæ statystyczna (test LR)
#model z restrykcjami (bez zmiennej "Ÿród³o informacji")
model_1.3_restricted_Ÿród³o_informacji<- glm(Y_1 ~ age + education_clean + employment +
                                               drugs_clean + samochod + transport_miejski + 
                                               sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                                             family = binomial(link = "logit"), data = dane)

#model z restrykcjami (bez zmiennej "przemieszczanie siê")
model_1.3_restricted_przemieszczanie_sie <- glm(Y_1 ~ age + education_clean + employment +
                                                  drugs_clean + telewizja + internet + 
                                                  sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                                                family = binomial(link = "logit"), data = dane)

lrtest(model_1.3, model_1.3_restricted_Ÿród³o_informacji)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y
lrtest(model_1.3, model_1.3_restricted_przemieszczanie_sie)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y


#redukcja o status zatrudnienia
model_1.4 <- glm( Y_1 ~ education_clean + 
                    drugs_clean +telewizja + internet + samochod + transport_miejski + 
                    sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                  family = "binomial", data = dane, maxit = 100)

#³¹czna istotnoœæ statystyczna (test LR)
#model z restrykcjami (bez zmiennej "Ÿród³o informacji")
model_1.4_restricted_Ÿród³o_informacji<- glm(Y_1 ~ education_clean +
                                               drugs_clean + samochod + transport_miejski + 
                                               sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                                             family = binomial(link = "logit"), data = dane)

#model z restrykcjami (bez zmiennej "przemieszczanie siê")
model_1.4_restricted_przemieszczanie_sie <- glm(Y_1 ~ education_clean +
                                                  drugs_clean + telewizja + internet + 
                                                  sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                                                family = binomial(link = "logit"), data = dane)

lrtest(model_1.4, model_1.4_restricted_Ÿród³o_informacji)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y
lrtest(model_1.4, model_1.4_restricted_przemieszczanie_sie)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y

#redukcja o wykszta³cenie
model_1.5 <- glm( Y_1 ~  drugs_clean +telewizja + internet + samochod + transport_miejski + 
                         sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                         family = "binomial", data = dane, maxit = 100)

#³¹czna istotnoœæ statystyczna (test LR)
#model z restrykcjami (bez zmiennej "Ÿród³o informacji")
model_1.5_restricted_Ÿród³o_informacji<- glm(Y_1 ~ drugs_clean + samochod + transport_miejski + 
                                                   sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                                                   family = binomial(link = "logit"), data = dane)

#model z restrykcjami (bez zmiennej "przemieszczanie siê")
model_1.5_restricted_przemieszczanie_sie <- glm(Y_1 ~ drugs_clean + telewizja + internet + 
                                                      sport + roommates + room_dis + prev_meas + diseases_clean + vaccine, 
                                                      family = binomial(link = "logit"), data = dane)

lrtest(model_1.5, model_1.5_restricted_Ÿród³o_informacji)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y
lrtest(model_1.5, model_1.5_restricted_przemieszczanie_sie)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y

#redukcja o liczbê wspó³lokatorów
model_1.6 <- glm( Y_1 ~  drugs_clean +telewizja + internet + samochod + transport_miejski + 
                         sport + room_dis + prev_meas + diseases_clean + vaccine, 
                         family = "binomial", data = dane, maxit = 100)

#³¹czna istotnoœæ statystyczna (test LR)
#model z restrykcjami (bez zmiennej "Ÿród³o informacji")
model_1.6_restricted_Ÿród³o_informacji<- glm(Y_1 ~ drugs_clean + samochod + transport_miejski + 
                                               sport + room_dis + prev_meas + diseases_clean + vaccine, 
                                             family = binomial(link = "logit"), data = dane)

#model z restrykcjami (bez zmiennej "przemieszczanie siê")
model_1.6_restricted_przemieszczanie_sie <- glm(Y_1 ~ drugs_clean + telewizja + internet + 
                                                  sport + room_dis + prev_meas + diseases_clean + vaccine, 
                                                family = binomial(link = "logit"), data = dane)

lrtest(model_1.6, model_1.6_restricted_Ÿród³o_informacji)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y
lrtest(model_1.6, model_1.6_restricted_przemieszczanie_sie)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y

#redukcja "szczepienie"
model_1.7 <- glm( Y_1 ~  drugs_clean +telewizja + internet + samochod + transport_miejski + 
                    sport + room_dis + prev_meas + diseases_clean, 
                  family = "binomial", data = dane, maxit = 100)

#³¹czna istotnoœæ statystyczna (test LR)
#model z restrykcjami (bez zmiennej "Ÿród³o informacji")
model_1.7_restricted_Ÿród³o_informacji<- glm(Y_1 ~ drugs_clean + samochod + transport_miejski + 
                                               sport + room_dis + prev_meas + diseases_clean, 
                                             family = binomial(link = "logit"), data = dane)

#model z restrykcjami (bez zmiennej "przemieszczanie siê")
model_1.7_restricted_przemieszczanie_sie <- glm(Y_1 ~ drugs_clean + telewizja + internet + 
                                                  sport + room_dis + prev_meas + diseases_clean, 
                                                family = binomial(link = "logit"), data = dane)

lrtest(model_1.7, model_1.7_restricted_Ÿród³o_informacji)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y
lrtest(model_1.7, model_1.7_restricted_przemieszczanie_sie)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y

#redukcja "u¿ywki"
model_1.8 <- glm( Y_1 ~ +telewizja + internet + samochod + transport_miejski + 
                    sport + room_dis + prev_meas + diseases_clean, 
                  family = "binomial", data = dane, maxit = 100)

#³¹czna istotnoœæ statystyczna (test LR)
#model z restrykcjami (bez zmiennej "Ÿród³o informacji")
model_1.8_restricted_Ÿród³o_informacji<- glm(Y_1 ~samochod + transport_miejski + 
                                               sport + room_dis + prev_meas + diseases_clean, 
                                             family = binomial(link = "logit"), data = dane)

#model z restrykcjami (bez zmiennej "przemieszczanie siê")
model_1.8_restricted_przemieszczanie_sie <- glm(Y_1 ~telewizja + internet + 
                                                  sport + room_dis + prev_meas + diseases_clean, 
                                                family = binomial(link = "logit"), data = dane)

lrtest(model_1.8, model_1.8_restricted_Ÿród³o_informacji)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y
lrtest(model_1.8, model_1.8_restricted_przemieszczanie_sie)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y

#redukcja "choroby przewlek³e"
model_1.9 <- glm( Y_1 ~ +telewizja + internet + samochod + transport_miejski + 
                    sport + room_dis + prev_meas, 
                  family = "binomial", data = dane, maxit = 100)

#³¹czna istotnoœæ statystyczna (test LR)
#model z restrykcjami (bez zmiennej "Ÿród³o informacji")
model_1.9_restricted_Ÿród³o_informacji<- glm(Y_1 ~samochod + transport_miejski + 
                                               sport + room_dis + prev_meas, 
                                             family = binomial(link = "logit"), data = dane)

#model z restrykcjami (bez zmiennej "przemieszczanie siê")
model_1.9_restricted_przemieszczanie_sie <- glm(Y_1 ~telewizja + internet + 
                                                  sport + room_dis + prev_meas, 
                                                family = binomial(link = "logit"), data = dane)

lrtest(model_1.9, model_1.9_restricted_Ÿród³o_informacji)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y
lrtest(model_1.9, model_1.9_restricted_przemieszczanie_sie)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y

#redukcja o stosowanie œrodków zapobiegawczych
model_zredukowany <- glm( Y_1 ~telewizja + internet + samochod + transport_miejski + 
                    sport + room_dis, 
                  family = "binomial", data = dane, maxit = 100)
summary(model_zredukowany)

#³¹czna istotnoœæ statystyczna (test LR)
#model z restrykcjami (bez zmiennej "Ÿród³o informacji")
model_zredukowany_restricted_Ÿród³o_informacji<- glm(Y_1 ~ samochod + transport_miejski + 
                                               sport + room_dis + diseases_clean, 
                                             family = binomial(link = "logit"), data = dane)

#model z restrykcjami (bez zmiennej "przemieszczanie siê")
model_zredukowany_restricted_przemieszczanie_sie <- glm(Y_1 ~telewizja + internet + 
                                                  sport + room_dis + diseases_clean, 
                                                family = binomial(link = "logit"), data = dane)

lrtest(model_zredukowany, model_zredukowany_restricted_Ÿród³o_informacji)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y
lrtest(model_zredukowany, model_zredukowany_restricted_przemieszczanie_sie)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y

coeftest(model_zredukowany, vcov = vcovHC(model_zredukowany, type="HC"))

#model zredukowany jest modelem zawieraj¹cym tylko zmienne istotne statystycznie
#miary dopasowania modelu zredukowanego
pR2(model_zredukowany)
AIC(model_zredukowany)
BIC(model_zredukowany)











#model_finalny jest modelem zawieraj¹cym najwa¿niejsze statystycznie zmienne z dodatkowymi zmiennymi istotnymi z punktu widzenia literatury

model_finalny <- glm( Y_1 ~  drugs_clean + telewizja + internet + samochod + transport_miejski + 
                     sport + room_dis + prev_meas + diseases_clean, 
                  family = "binomial", data = dane, maxit = 100)

#³¹czna istotnoœæ statystyczna (test LR)
#model z restrykcjami (bez zmiennej "Ÿród³o informacji")
model_finalny_restricted_Ÿród³o_informacji<- glm(Y_1 ~ drugs_clean +samochod + transport_miejski + 
                                                       sport + room_dis + diseases_clean + prev_meas + diseases_clean, 
                                                     family = binomial(link = "logit"), data = dane)

#model z restrykcjami (bez zmiennej "przemieszczanie siê")
model_finalny_restricted_przemieszczanie_sie <- glm(Y_1 ~ drugs_clean +telewizja + internet + 
                                                          sport + room_dis + diseases_clean + prev_meas + diseases_clean, 
                                                        family = binomial(link = "logit"), data = dane)

lrtest(model_finalny, model_finalny_restricted_Ÿród³o_informacji)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y
lrtest(model_finalny, model_finalny_restricted_przemieszczanie_sie)
#odrzucamy hipotezê zerow¹ na korzyœæ hipotezy alternatywnej. Isotny wp³yw zmiennych na Y


coeftest(model_finalny, vcov = vcovHC(model_finalny, type="HC"))

#miary dopasowania modelu finalnego
pR2(model_finalny)
AIC(model_finalny)
BIC(model_finalny)












###TABLICA KLASYFIKACJI, KRZYWA ROC i AUC###

#Model pe³ny
dane$used <- TRUE
dane$used[na.action(model_pe³ny)] <- FALSE;
tab_data <- dane[which(dane$used == TRUE),]
table(true = tab_data$Y_1, pred = round(fitted(model_pe³ny)))
pred_ROC <- prediction(fitted(model_pe³ny), tab_data$Y_1)
plot(performance(pred_ROC, "acc"))
plot(performance(pred_ROC, "tpr", "fpr"))
abline(0, 1, lty = 2)
auc_ROCR <- performance(pred_ROC, measure = "auc")
auc_ROCR_model_pe³ny <- auc_ROCR@y.values[[1]]
auc_ROCR_model_pe³ny

#Model zredukowany
dane$used <- TRUE
dane$used[na.action(model_zredukowany)] <- FALSE;
tab_data <- dane[which(dane$used == TRUE),]
table(true = tab_data$Y_1, pred = round(fitted(model_zredukowany)))
pred_ROC <- prediction(fitted(model_zredukowany), tab_data$Y_1)
plot(performance(pred_ROC, "acc"))
plot(performance(pred_ROC, "tpr", "fpr"))
abline(0, 1, lty = 2)
auc_ROCR <- performance(pred_ROC, measure = "auc")
auc_ROCR_model_zredukowany <- auc_ROCR@y.values[[1]]
auc_ROCR_model_zredukowany

#Model finalny
dane$used <- TRUE
dane$used[na.action(model_finalny)] <- FALSE;
tab_data <- dane[which(dane$used == TRUE),]
table(true = tab_data$Y_1, pred = round(fitted(model_finalny)))
pred_ROC <- prediction(fitted(model_finalny), tab_data$Y_1)
plot(performance(pred_ROC, "acc"))
plot(performance(pred_ROC, "tpr", "fpr"))
abline(0, 1, lty = 2)
auc_ROCR <- performance(pred_ROC, measure = "auc")
auc_ROCR_model_finalny <- auc_ROCR@y.values[[1]]
auc_ROCR_model_finalny


#wyniki estymacji wszystkich modeli w jednej tabeli
stargazer(model_pe³ny, model_zredukowany, model_finalny, type = "text")

#inny wygl¹d krzywej ROC dla modelu finalnego
model_finalny_pred = predict(model_finalny, dane, type="response")
perf_eva(model_finalny_pred,dane$Y_1, confusion_matrix = T, binomial_metric = c("ks", "auc", "gini"), show_plot = c("ks", "roc"))

#efekty krañcowe modelu finalnego
logitmfx(Y_1 ~  drugs_clean +telewizja + internet + samochod + transport_miejski + 
           sport + room_dis + prev_meas + diseases_clean, data = dane, atmean = FALSE)

#ilorazy szans modelu finalnego
exp(coefficients(model_finalny))
