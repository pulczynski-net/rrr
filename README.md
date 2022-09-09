# rrr
rrr

---
title: "HOTELE"
output: HOTELE
---

#1 Utwórz wektor i nazwij go zarobki. Przypisz do niego następujące wartości: 2300, 1800, 4500, 3700, 1300, 7200, 6100. Nazwij każdy element wektora dowolnym imieniem. Wyświetl wektor z nazwanymi elementami, najmniejszą wartość, jego długość i oblicz średnią wartość.

```{r}
zarobki <- c(jan=2300, tomek=1800, maciek=4500, grzegorz=3700, bartek=1300, kacper=7200, janusz=6100)
zarobki
min(zarobki)
length(zarobki)
mean(zarobki)
```
#2 Wczytaj dołączony plik Hotel_booking_satisfaction.csv do obiektu o nazwie hotele.  Wyświetl utworzony obiekt hotele.
```{r}
hotele <- read.table(file="C:\\Users\\barto\\Desktop\\europa_hotel.csv", sep=",", dec=".",header=TRUE)
hotele
```
#3 Z ramki danych hotele wyświetl: •3 pierwsze wiersze, •3 ostatnie wiersze, •nazwy wszystkich kolumn.
```{r}
hotele <- read.table(file="C:\\Users\\barto\\Desktop\\europa_hotel.csv", sep=",", dec=".",header=TRUE)
head(hotele,3)
tail(hotele,3)
names(hotele)
```
#4 Wyświetl dane dla kolumny „Gender” z obiektu hotele.
```{r}
hotele <- read.table(file="C:\\Users\\barto\\Desktop\\europa_hotel.csv", sep=",", dec=".",header=TRUE)
gener <- hotele %>%
  select(Gender)
gener
```
#5 Wyświetl informację, czy wartości z kolumny „Cleanliness” są równe bądź większe od 3.
```{r}
hotele <- read.table(file="C:\\Users\\barto\\Desktop\\europa_hotel.csv", sep=",", dec=".",header=TRUE)
clean <- hotele %>%
    mutate(isCleanliness=if_else(Cleanliness>=3,"tak","nie"))
clean
```
#6 Oblicz średni wynik dla kolumny „Food.and.drink” i zaokrąglij go do dwóch miejsc po przecinku.
```{r}
hotele <- read.table(file="C:\\Users\\barto\\Desktop\\europa_hotel.csv", sep=",", dec=".",header=TRUE)
sredni <- hotele %>%
  summarise(sredni_wynik=round(mean(Food.and.drink),2))
sredni
```
#7 Uporządkuj dane według kolumny „pupose_of_travel”.
```{r}
hotele <- read.table(file="C:\\Users\\barto\\Desktop\\europa_hotel.csv", sep=",", dec=".",header=TRUE)
uporz <- hotele %>%
  arrange(purpose_of_travel)
uporz
# lub jeśli chodzi o uporządkowanie inaczej grupowanie to:
uporz <- hotele %>%
  group_by(purpose_of_travel) %>%
  summarise(liczebnosc=n())
uporz
```
#8 Wyświetl liczebność według danych z kolumny „Type.Of.Booking” i zapisz to do wektora, który nazwiesz swoim nazwiskiem.
```{r}
hotele <- read.table(file="C:\\Users\\barto\\Desktop\\europa_hotel.csv", sep=",", dec=".",header=TRUE)
wektor<-hotele%>%
  group_by(Type.Of.Booking)%>%
 summarise(liczba=n())%>%
  select(liczba)
pulczynski=c(wektor)
pulczynski
```
#9 Przygotuj wykres kolumnowy przedstawiający liczebność danych z kolumny „Stay.comfort”. Wstaw tytuł wykresu, nazwij osie, ustaw różne kolory dla poszczególnych słupków. Wygeneruj plik graficzny z wykresem.
```{r}
hotele <- read.table(file="C:\\Users\\barto\\Desktop\\europa_hotel.csv", sep=",", dec=".",header=TRUE)
wykres<-hotele%>%
  group_by(Stay.comfort)%>%
 summarise(liczba=n())
wykres
barplot(wykres$liczba, main = "Stay.comfort", ylab="responses",xlab="liczebnosc", ylim = c(0,35000),col=c("red", "green", "blue","orange","pink", "yellow"), names.arg = c("0","1","2","3","4","5") )
```
#10 W nazwach kolumn w obiekcie hotele zamień „.” na „_”, wykorzystując funkcję gsub.
```{r}
hotele <- read.table(file="C:\\Users\\barto\\Desktop\\europa_hotel.csv", sep=",", dec=".",header=TRUE)
colnames(hotele) <- gsub("\\.+", "_", colnames(hotele))
hotele
```
#11 Utwórz funkcję o nazwie podatek, która obliczy, ile podatku należy zapłacić od podanego towaru i zwróci odpowiedź: „Od (podana wartość) złotych należy zapłacić (wyliczona wartość) złotych podatku”. Wyliczona wartość ma być zaokrąglona do 2 miejsc po przecinku. Przyjmij, że podatek wynosi 13%.
```{r}
podatek <- function(kwota)
{
  print(paste("Od ",kwota," złotych należy zapłacić ", round(kwota*0.13,2)," złotych podatku."))
}
podatek(100)
```
#12 Napisz program, który sprawdzi, czy liczba jest nieparzysta. Przy spełnieniu tego warunku od podanej wartości odejmie 3i poda odpowiedź, a w przeciwnym razie wyświetli komunikat: „Uwaga! Liczba jest parzysta”.
```{r}
czyNieparzysta <- function(liczba)
{
  if(liczba%%2==0)
  {
    print("Uwaga! Liczba jest parzysta!")
  } else {
    print(liczba-3)
  }
}
czyNieparzysta(10)
czyNieparzysta(11)
```


---
title: Wino
output: html_notebook
---
#1. Utwórz wektor i nazwij go swoim nazwiskiem. Przypisz do niego następujące wartości: 23,12, 49, 9, -4, 51, 83. Wyświetl zakres wektora, jego długość, a także najmniejszą i największą wartość.
```{r}


pulczynski=c(23,12, 49, 9, -4, 51, 83) #za mopomąca funkcji c() tworzymy wektor 
pulczynski                         # wypisujemy wektor
length(pulczynski)             # długość wektora
min(pulczynski)                # najmniejsza wartość.
max(pulczynski)               #  największa wartość
```

# 2. Utwórz listę i nazwij ją desery. Przypisz do niej nazwę ciasta (szarlotka), jego liczbę kalorii w 100 g (265 kcal) i informację o zawartości cynamonu (prawda). Wyświetl informację o liczbie kalorii. Usuń kategorię dotyczącą zawartości cynamonu. Dodaj kategorię dodatki i zapisz do niej lody waniliowe. 
```{r}


desery=list(ciasta="szarlotka", liczbe_kalorii="w 100g 265kcal", zawartosc_cynamonu=TRUE)
desery$liczbe_kalorii             # za pomocą $ odwoluje się do liczba_kalorij i wwyświetlam informacje 
desery$zawartosc_cynamonu=NULL    #usuwam kategorię dotyczącą zawartości cynamonu
desery$dodatki="lody waniliowe"   # dodaje nową kategorjie do listy 
desery                         # wypisuje liste dla sprawdzania 
```

#3. Wczytaj dołączony plik wine_red.csv do obiektu o nazwie wino.
#4 nazwy kolumn dla ramki danych wino , pierwsze wiersze ramki danych wino ,kolumnę pH z ramki danych wino
```{r}

#pakiety potrzebne do przetwarzania danych
#install.packages("dplyr")
#install.packages("tidry")

wino<-read.table(file="C:\\Users\\barto\\Desktop\\wine_red.csv", sep=";", dec=".",header=TRUE) #Wczytaj dołączony plik
wino

names(wino)     
head(wino,4)     
phh <- wino %>%
  select(pH)
phh

sulfur<-wino%>%     #informację, czy wartości z kolumny „total.sulfur.dioxide” są większe od 40.
  mutate(większa_od_40=if_else(total.sulfur.dioxide>40, "tak", "nie")) #dodaje nową kolumne gdzie zapiszę tak nie jeżeli zawartość total.sulfur.dioxide>40
sulfur

```

#5. Oblicz średni poziom alkoholu w winach z ramki danych wino
```{r}

wino<-read.table(file="C:\\Users\\barto\\Desktop\\wine_red.csv", sep=";", dec=".",header=TRUE)

srednia<-wino%>%            
summarise(srednia=round(mean(alcohol),2))
srednia

```
#6. Posortuj dane według podanych instrukcji, wykorzystują odpowiednie funkcje.
#Uporządkuj dane z kolumny „residual.sugar” (średni wynik) według zawartości wody (kolumna "citric.acid”)
#Uporządkuj dane z obiektu owoce według kolumny „chlorides”
```{r}

wino<-read.table(file="C:\\Users\\barto\\Desktop\\wine_red.csv", sep=";", dec=".",header=TRUE)
wino%>%
   group_by(citric.acid)%>%
  #group_by(citric.acid) %>%
  summarise(srednia=round(mean(residual.sugar),2))

wino%>%
 # group_by(chlorides)%>%
 # summarise(liczba=n())
  arrange(chlorides) #to jest sortowanie 
  
```

#7. Wyświetl liczebność win według danych z kolumny „quality”.
```{r}

wino<-read.table(file="C:\\Users\\barto\\Desktop\\wine_red.csv", sep=";", dec=".",header=TRUE)
wino%>%
  group_by(quality)%>%
  summarise(liczba=n()) 


```
#8. Przygotuj wykres kolumnowy przedstawiający dane z kolumny „quality”. Wstaw tytuł wykresu, nazwij osie,ustaw różne kolory dla poszczególnych słupków.
```{r}

wino<-read.table(file="C:\\Users\\barto\\Desktop\\wine_red.csv", sep=";", dec=".",header=TRUE)
wykres<-wino%>%
  group_by(quality)%>%
  summarise(liczba=n()) 

barplot(wykres$liczba, main = "Liczebność według quality", ylab= "Ilosć",xlab="quality", names.arg=c(wykres$quality), col=c("red", "green", "blue","orange","pink", "yellow") ) 

```

#9. Utwórz wykres kołowy dla danych w poprzedniego zadania.
```{r}

wino<-read.table(file="C:\\Users\\barto\\Desktop\\wine_red.csv", sep=";", dec=".",header=TRUE)
wykres<-wino%>%
  group_by(quality)%>%
  summarise(liczba=n())
pie(wykres$liczba, wykres$quality)
```
#10. Utwórz funkcję o nazwie zloty_euro, która obliczy, ile złotych można dostać przy wymianie 1346,89 EURO i zwróci odpowiedź: „Za (podana wartość) EURO otrzymasz (wyliczona wartość) złotych)”. Wyliczona wartość ma być zaokrąglona do 2 miejsc po przecinku. Zadanie wymaga sprawdzenia aktualnego kursu walut.
```{r}
zloty_eu=function(euro,zloty){
  cat(paste("Za", euro, "EURO otrzymasz", euro*zloty, "złotych"))
}
zloty_eu(1346.89, 4.7)
```

#11. Napisz program, który sprawdzi, czy liczba jest parzysta. Przy spełnieniu tego warunku podzieli tę liczbę przez 2 i poda odpowiedź, a w przeciwnym razie wyświetli komunikat: „Błąd! Liczba jest nieparzysta”
```{r}

czy_Parzysta=function(liczba){
  if(liczba%%2==0){
    
    cat(paste("jeat parzysta, i podzielona przez dwa:", liczba/2))
  }else{cat(paste("Błąd! Liczba jest nieparzysta"))
  }
}
czy_Parzysta(8989)
```




---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
plot(cars)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.


```{r}
# LAB 6

# Ćwiczenie 1
  # k1
  # k3
  install.packages("dplyr")
  install.packages("tidyr")
  
# Ćwiczenie 1
  # k1
  install.packages("PogromcyDanych")
  library(dplyr)
  library(PogromcyDanych)
  tmp <- filter(auta2012, Marka == "Porsche")
  head(tmp)
  # k2
  library(dplyr)
  library(PogromcyDanych)
  tmp <- filter(auta2012, Marka == "Porsche", KM > 300, Cena.w.PLN > 500)
  head(tmp)
  # k3
  library(dplyr)
  library(PogromcyDanych)
  tmp <- select(auta2012, Marka, Rok.produkcji)
  head(tmp)
  # k4
  tmp <- select(auta2012, starts_with("Cena"))
  head(tmp)
  # k5
  library(dplyr)
  library(PogromcyDanych)
  
  WiekAuta <- mutate(auta2012, Wiek.auta = 2010 - Rok.produkcji)
  head(WiekAuta[,c("Wiek.auta", "Rok.produkcji")])
  # k6
  library(dplyr)
  library(PogromcyDanych)
  
  sortowanieAut <- arrange(auta2012, Model, Cena.w.PLN)
  head(select(sortowanieAut, Model, Marka, Cena.w.PLN, Cena, Waluta))
  # k7
  auta2012 %>% 
    summarise(sredniaCena = mean(Cena.w.PLN), sdCena = sqrt(var(Cena.w.PLN)), medianaPrzebiegu = median(Przebieg.w.km, na.rm = TRUE))
  # k8
  auta2012 %>%
    filter(Marka == "Volkswagen", Rok.produkcji == 2007) %>%
      group_by(Rodzaj.paliwa) %>%
        summarise(medianaCeny = median(Cena.w.PLN, na.rm = TRUE), medianaPrzebieg = median(Przebieg.w.km, na.rm = TRUE), liczba = n())

# ZADANIA
  # z1
  auta2012
  # z2
  auta2012 %>%
    count(Marka, sort = TRUE) # Volkswagen
  # z3
  auta2012 %>%
    filter(Marka == "Toyota") %>%
      count(Model, sort = TRUE) # Yaris
  # z4
  autaMlode <- filter(auta2012, Rok.produkcji > 2010)
  # z5
  autaMlode %>%
    filter(Marka == "Fiat", Model == "500") %>%
      count(Model) # 48
  # z6
  auta2012 %>%
    filter(Marka == "Rolls-Royce") %>%
      arrange(Cena.w.PLN)
  # z7
  auta2012 %>%
    filter(Marka == "Rolls-Royce", Model == "Phantom") %>%
      arrange(Cena.w.PLN)
  # z8
  auta2012 %>%
    filter(Marka == "Volkswagen") %>%
      arrange(Cena.w.PLN) %>%
        head(5)
  # z9
  auta2012 %>%
    filter(Marka == "Volkswagen") %>%
      arrange(desc(Pojemnosc.skokowa)) %>%
        head(3)
  # z10
  auta2012 %>%
    filter(Marka == "Volvo", Kolor == "zielony") %>%
      arrange(desc(Pojemnosc.skokowa)) %>%
        select(Marka, Cena.w.PLN, Kolor)
  # z11
  auta2012 %>%
    select(ends_with("l"))
  # z12
  auta2012 %>%
    select(ends_with("a"))
  # z13
  auta2012 %>%
    transmute(sredni_przebieg = (Przebieg.w.km/(2012-Rok.produkcji)), Marka, Model) # przyjmując, że jest 2012 rok
  # średni przebieg per samochód
  # z14
  auta2012 %>%
    filter(Marka == "Volvo") %>%
      transmute(sredni_przebieg = (Przebieg.w.km/(2012-Rok.produkcji)), Marka, sredniaCena = mean(Cena.w.PLN))
  # z15
  auta2012 %>%
    filter(Marka == "Volkswagen", Model == "Golf") %>%
      group_by(Kraj.aktualnej.rejestracji, srednia = mean(Cena.w.PLN))
  # z16
  auta2012 %>%
    filter(Marka == "Peugeot", Model == "106") %>%
      group_by(Rok.produkcji) %>%
        transmute(srednia_cena = mean(Cena.w.PLN))
  # z17
  auta2012 %>%
    filter(Marka == "Seat", Model == "Leon", Kolor == "czarny", Cena.w.PLN < 10000) %>%
    arrange(Cena.w.PLN)
  
```


# LAB 5

```{r}
# Ćwiczenie 1
  # k1
  matrix(c(1, 3, 4, -1, 2, 4, 3, 2, 1), ncol = 3, nrow = 3)
  # k2
  matrix(c(2, 3, 1, -1, 2, 3, 3, -2, 2), ncol = 3, nrow = 3, byrow = TRUE)
  # k3
  macierz3 <- matrix(c(2, 3, 1, 3), ncol = 2, nrow = 2, byrow = TRUE)
  macierz4 <- matrix(c(-1, 2, -2, 4), ncol = 2, nrow = 2, byrow = TRUE)
  print(macierz3 %*% macierz4)
  # k4 zad1.
  outer(1:3, 1:5)
  outer(rep(7, 3), c(3, 1, 2))
  # k4 zad2.
  outer(1:3, 1:5, "+")
  outer(1:3, 1:5, function(x, y) log(x^2+y^2))
  # k5 zad1.
  ciag=1:10
  matrix(ciag, 2, 5, dimnames=list(c("rząd1", "rząd2"), c("kol1", "kol2", "kol3", "kol4", "kol5")))
  # k5 zad2.
  macierz = matrix(1:10, 2, 5, dimnames=list(c("rząd1", "rząd2"), c("kol1", "kol2", "kol3", "kol4", "kol5")))
  macierz["rząd2", "kol3"]
  # k6 zad1.
  macierz = matrix(1:9, 3, 3)
  macierz
  macierz[1,] = c (7, 6, 5)
  macierz
  macierz[,1] = c (7, 6, 5)
  macierz
  # k6 zad2.
  macierz = matrix(1:9, 3, 3)
  macierz
  macierz1 = macierz[c(1, 3),]
  macierz1
  macierz2 = macierz[-2,]
  macierz2
 # k6 zad3.
  macierz3 = macierz[,c(1, 3)]
  macierz3
  macierz4 = macierz[,-2]
  macierz4
# Zadania
  # zad1.1
  mac1 = matrix(15:23, 3, 3)
  mac2 = matrix(3:11, 3, 3)
  # a
  det(mac1)
  # b
  mac3 = mac1 %*% mac2
  mac3
  # c
  t(mac2)
  # d
  mac1[2,] = c(30, 31, 32)
  mac1
  # e
  mac2[,3]
# Ćwiczenie 2
  # k1
  imiona    <- c("Ania", "Kasia", "Janek", "Borys")
  wiek      <- c(8, 5, 3, 9)
  lubiaLody <- c(TRUE, TRUE, FALSE, TRUE)
  dzieci    <- data.frame(imiona, wiek, lubiaLody)
  dzieci
  # zad1.
  # a
  dzieci[1,1]
  # b
  dzieci[2:4, c(1, 3)]
  # c
  dzieci[1,]
  # d
  dzieci[1]

```




# LAB4

# Sprawdź działanie funkcji:
Sys.time()
month.name
#now()
#today()

# Utwórz funkcję o nazwie ab2 i b przypisz wartość 1
ab2 <- function() b = 1
ab2 <- function() {
    b=1
    a=2
}

# Napisz funkcję PodajDate(), która wyświetli tekst Dzisiaj jest z aktualną datą
PodajDate <- function() {
    print(paste("Dzisiaj jest ", Sys.time()))
}
PodajDate()

# Napisz program (zastosuj if … else), który wyliczy pierwiastki rzeczywiste równania kwadratowego
# ax2+bx+c=0. 
Pierwiastki <- function(a, b, c) {
    delta = b^2 - 4*a*c
    print(paste("delta ", delta))
    if(delta>0){
        x1 = (-b - sqrt(delta)) / (2*a)
        print(paste("x1 ", x1))
        x2 = ((-b)+sqrt(delta))/(2*a)
        print(paste("x2", x2))
    } else if (delta == 0) {
        x0 = (-b)/(2*a)
        print(paste("x0", x0))
    } else {
        print("brak pierwiastków")
    }
}
Pierwiastki(-3, 3, 4)

#  Napisz pętlę, która wyświetli na ekranie poniższe dane: krok numer: 1 itd. Wprowadź poniższe dane.
for(i in 1:5)
{
    cat(paste("krok numer: "), paste(i,"\n"))
}

# Korzystając z Iterator – obiekt pozwalający na sekwencyjny dostęp do wszystkich elementów lub części
# zawartych w innym obiekcie, zwykle kontenerze lub liście, napisz pętlę, która wyświetli jeden, dwa, trzy.
# Wprowadź dane.
iteratory <- c("jeden", "dwa", "trzy")
for(i in iteratory)
{
    cat(paste(i,"\n"))
}

# Napisz pętlę, która przeliczy wartości odległości z mil lądowych na kilometry. Nasze dane wejściowe to lista
# składająca się z trzech wartości:2, 10, oraz 100. Wiemy też, że jedna mila lądowa to 1,609 kilometra.
iletokm = c(2, 10, 100)
for(i in iletokm)
{
    print(i*1.609)
}

#
seq_along(iletokm)

# Aby skorzystać w przyszłości z wyników działania pętli (przelicznik kilometrów), należy od razu stworzyć
# listy, o długości zgodnej z naszym oczekiwaniem. Następnie kolejne elementy stworzonej listy są zamieniane na
# oczekiwane przez nas wartości.
iletokm = list(2,10,100)
ilekm= vector("list", length = length(iletokm))
for (i in seq_along(iletokm)){
  ilekm[[i]] = iletokm[[i]] * 1.609
}
ilekm

# Stwórz nową funkcję która przyjmuje listę z wartościami w milach lądowych jako obiekt wejściowy, a
# później zwraca listę z wartościami w kilometrach. Do poprzedniego przykładu wprowadź kod. 
mile_na_km <- function(iletokm){
    for(i in iletokm) {
        print(i*1.609)
    }  
}

odleglosci_mile = list(0, 1, 100, 170)
mile_na_km(odleglosci_mile)

# Funkcje zastępujące w R pętlę iteracyjną. Wyliczanie średniej wartości ciągu liczb można wykonać za
# pomocą funkcji mean() lub za pomocą pętli "for". Zapisz przykład. 
ciag = c(8, 1,3,1,4,1,4,2,1,4)
srednia = 0
for(i in 1:length(ciag)) srednia=srednia+ciag[i]
srednia=srednia/length(ciag)
srednia
mean(ciag)

# Wprowadź macierz
macierz = matrix(1:12, 4, 3)
macierz

# Zastosuj podwójną pętlę for obliczającą sumę pierwszego wiersza. Wynik poniżej. 
sumy=c(0,0,0,0)
for (i in 1:nrow(macierz)) 
  for (j in 1:ncol(macierz))  
    sumy[i]= sumy[i] + macierz[i,j]
sumy

# Wypisz nazwy miesięcy. Napisz pętlę for
for(i in month.name)
{
    print(i)
}

# Funkcja apply(macierz,1,sum) zastępuje podwójną pętlę. Za wyraz macierz podaje się nazwę analizowanej
# macierzy, liczba 1 na drugiej pozycji oznacza, że chodzi o zastosowanie funkcji sum dla wierszy.
sumy = apply(macierz,1,sum)
sumy

# Aby wyprowadzić kwadraty liczb od 1 do 10 należy skorzystać z pętli while. 
i <- 1
while(i<=10)
{
    print(i^2)
    i<-i+1
}

# Napisz pętlę (while), która wyświetli co drugą liczbę od 7 do 0
i <- 7
while(i>=0)
{
    print(i)
    i<-i-2
}

# Napisz program wyliczający największy wspólny mianownik dwóch liczb całkowitych metodą Euklidesa.
# Algorytm ten polega na dzieleniu liczby większej przez mniejszą, a następnie zastępowaniu liczby większej mniejszą,
# a mniejszej resztą z dzielenia. Proces ten jest powtarzany aż do uzyskania reszty zerowej.
a = 60
b = 5
c = 2
while (c != 0){
  c = b %% a
  b = a
  a = c
}
cat("NWD = ",b,"\n")


# Napisz funkcję o nazwie dolar, która przyjmuje argument liczbowy n, a następnie rysuje kwadrat
#(teoretycznie – w praktyce prostokąt) o boku n wypełniony znakami $
dolar <- function(n){
  
  for(i in 1:n){
    for(j in 1:n){
      cat(paste(n ="$"))
    } 
    cat(paste("\n"))
  }
}  
dolar(10)

print("")
# Napisz funkcję o nazwie pustedolary, która przyjmuje argument liczbowy n, a następnie rysuje kwadrat
# (teoretycznie – w praktyce prostokąt) o boku n ze znakami $ na brzegu i pusty w środku.
pustedolary <- function (n){
   for(i in 1:n){
    for(j in 1:n){
      if(i == 1 | i == n | j == 1 | j == n){
        cat(paste("$"))
      }else
      cat(paste(" "))
    } 
    cat(paste("\n"))
  }
   
}
pustedolary(10)



# LAB3

# Wprowadź dane.
wek1 = c("a", "a", "b", "b", "c", "d")
wek2 = c(1,2,3,3,2,1)
wek3 = c("duże", "duże", "małe", "duże", "małe", "małe")

# Wyprowadź pierwszy układ wektora.
table(wek1)

# Wyprowadź pierwszy i drugi układ wektora
table(wek1,wek2)

# Wyprowadź pierwszy, drugi i trzeci układ wektora. 
table(wek1,wek2,wek3)

# Wprowadź tbl=1:20
tbl=1:20
tbl

# Tablice można tworzyć z istniejących wektorów używając funkcji dim
dim(tbl) = c(4,5)
tbl

# . Argumentem outer może być tez nazwa funkcji opierająca się na dwóch zmiennych. Wprowadź dane
outer(1:10,1:10,"/")

# . Przegrupowanie tabeli – funkcja aperm. Transpozycje tabeli. c(2,1) – oznacza, że drugi staje się pierwszym
# a pierwszy drugim,
aperm(tbl, c(2,1))

#  Funkcje cbind i rbind formują tablice z podanych wektorów, poprzez umieszczenie ich rzędami lub
# kolumnami w nowo tworzonej tabeli. Wprowadź dane. 
cbind(1:5,6:10,11:15) # kolumnami
rbind(1:5,6:10,11:15) # wierszami

# Każdą tabelę można przekształcić na wektor funkcją as.vector.
as.vector(tbl)

# Gdy liczba elementów ciągu, z którego tworzy się tablicę o rozmiarach c(n1, n2, ...,nk), jest mniejsza od
# n1n2...nk ciąg ten jest kilkakrotnie powtarzany.
tab = array(1:7,dim=c(4,3,2))

# Wprowadź wektor 31:50 i zamień na tablicę o wymiarach 4*5
wektor = c(31:50)
wektor
dim(wektor) = c(4,5)
wektor

# a. Odwołaj się do pierwszego elementu tablicy.
wektor[1,1]
# b. Odwołaj się do pierwszego rzędu tablicy.
wektor[1,]
# c. Odwołaj się do pierwszej kolumny tablicy.
wektor[,1]
# d. Odwołaj się do drugiego rzędu i drugiej kolumny.
wektor[2,2]
# e. Wyświetla wymiary tablicy.
dim(wektor)
# f. Zmień wymiary tablicy (przykład poniżej)
dim(wektor) = c(2,10)
wektor
# g. Pokaż szóstą kolumnę.
wektor[,6]
################################################################################################################
# h. Utwórz tablicę współrzędnych o nazwie i.Skorzystaj z funkcji array. 
i = array(1:4, c(4,2))
i
################################################################################################################

# Utwórz tabliczkę mnożenia 10*10
outer(1:10,1:10,"*")

# Utwórz dwie tablice dwuwymiarowe z 4 kolumnami i trzema wierszami. Wypełnij ją danymi od 1 do 24
tab1<-array(1:24,c(3,4))
tab2<-array(1:24,c(3,4))
tab1
tab2

###########################################################################################################
# Wprowadź elementy ciągu do tablicy, aby w efekcie powstała poniższa tablica. 
print("wpr")
w = array(3:10,c(2,4))
w
###########################################################################################################

# Wprowadź dane:
w = c(3,"4")
w
w = list(3, "4")
w

# Wprowadź dane:
x <- c(2,3,2,5,4,3,6,7,8)
opisStruktury <- list(srednia=mean(x), minimum=min(x), maksimum=max(x))
print(opisStruktury)

# Elementy listy – nazwy
w = list(liczba = 2, znak = "3")
w

#  Gdy jako elementy listy wprowadza się nazwane wcześniej obiekty, ich nazwy nie stają się nazwami
# elementów listy. Trzeba je wprowadzić ponownie. Wprowadź dane. 
Liczby = 1:10
Znaki = letters[1:10]
w = list(Liczby, Znaki)
w
w = list(Liczby=Liczby, Znaki=Znaki)
w

# Wyprowadź dane używając polecenia print. 
print(opisStruktury$maksimum)

#  Wyprowadź dane używając polecenia selektora, [numer] oznaczającego wybór elementu o podanym
# numerze. 
opisStruktury[[3]]

# Jeśli obiekt złożony, został utworzony za pomocą pewnej funkcji i nie znamy jego struktury, to nazwy
# elementów składowych są zawarte w atrybucie names: attr(opisStruktury,"names"), names(opisStruktury)
attr(opisStruktury,"names")
names(opisStruktury)

# Wprowadź: L <-list(1, "napis", TRUE, 5:10). Wywołaj L i wyznacz poszczególne elementy listy
L <- list(1, "napis", TRUE, 5:10)
L



# LAB2

# Utworzyć wektor liczb od 1.1 do 1.10 i zapamiętać go pod nazwą wektor_liczb.
wektor_liczb = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 1.10)
wektor_liczb

# Wprowadź dwa wektory i dokonaj obliczeń: +;-,*
a = c(1, 2, 3)
b = c(3, 5, 10)
a+b
a-b
a*b

# Wprowadź dzisiejszą datę: dzis = Sys.Date()
Sys.Date()

# Wprowadź wektor: liczby=c(4,6,2,6,7,4,3).
liczby = c(4,6,2,6,7,4,3)
liczby

# Wprowadź jeszcze raz, ale zamiast 2 wprowadź 2.5
liczby = c(4,6,2.5,6,7,4,3)
liczby
min(liczby)
max(liczby)

# Wprowadź do wektora o nazwie kolory – cztery dowolne kolory.
kolory = c("czerwony", "zielony", "niebieski", "żółty")
# a. Z wektora pobierz drugą wartość wpisując jej pozycję w ciągu (licząc od 1).
kolory[2]
# b. Z wektora wybierz część sekwencji, na przykład elementy od 2 do 4.
kolory[2:4]
# c. Do wektora dodaj piąty kolor.
kolory[5] = "fioletowy"
# d. Pierwszy kolor zmień na kolor rudy.
kolory[1] = "rudy"
# e. Sprawdź długość elementów: length(nazwa wektora)
length(kolory)
# f. Posortuj kolory.
sort(kolory)
# g. Połącz dwa wektory liczby i kolory. 
wektory <- c(liczby, kolory)
wektory

# Wygenerować poniższy ciąg
-5:5

# Wygenerować miesiące
month.name

# Wygenerować litery z alfabetu używając: letters
letters

# Wygenerować poniższy ciąg. Sprawdzić działanie. 
month.name[ -(5:9) ]

# Wygenerować poniższe ciągi i sprawdź ich działanie.
seq(from=0, to=20, by=1)
seq(from=10, to=30, by=1)
seq(from=0, to=20, length.out=15)

# Funkcja runif. Sprawdź w pomocy. Wprowadź ciąg
runif(5, min=1, max=20)

# Wygeneruj ciąg liczbowy z rozkładu rnorm, który pokaże 10 liczb od 1 do 20.
print("rnorm")
rnorm(1:20,n=10)

# Wygenerować ciąg liczb od 1 do 45
1:45

# Wygenerować za pomocą rozkładu normalnego dowolny ciąg liczbowy dla 5 liczb.
rnorm(n=5)

# Wygenerować litery z alfabetu od litery 1 do 10.
letters[1:10]

# Wygenerować tylko miesiące od czerwca do sierpnia. 
month.name[6:8]

# Masz trapez o długości podstaw a = 5 i b = 6 oraz wysokości h = 3. Stwórz nowy obiekt
# pole_trapezu, który będzie zawierał obliczone pole tego trapezu
a = 5
b = 6
h = 3
pole_trapezu = ((a + b) * h ) / 2
pole_trapezu

# Masz koło o promieniu r=3. Stwórz nowy obiekt pole_kola, który będzie zawierał obliczone pole koła.
r = 3
pole_kola = pi*r^2
pole_kola


# LAB1
print("Hello world")

# Pracujesz w Austrii. Po powrocie do Polski przywiozłeś(aś) 6000 EUR. Aktualny kurs kupna EUR
# wynosi 4,45. Ile zarobiłeś(aś) w przeliczeniu na PLN? Wylicz to w R.
zarobek <- 6000 * 4.45 
zarobek
# Oblicz pierwiastek z 9.
sqrt(9)

# Sprawdź działanie i zobacz różnice między operatorami /, %%,%/%. Wykonaj działanie na dwóch
# parach liczb całkowitych. 
11/2
11%%2
11%/%2

# Oblicz resztę z dzielenia 20 przez 3.
20%%3

# Oblicz cosinus liczby pi, gdzie pi jest stałą wbudowaną w R.
cos(pi)

# Pokaż trzy miejsca po przecinku. 
round(2.34534, 3)

# Zaokrąglij w dół:
floor(2.34534)

# Zaokrąglij w górę:
ceiling(2.34534)

# Wartość bezwzględna.
abs(-1.25)

# Wprowadź dane do R poleceniem
x = c(1,8,4,5,2,4,5,2,1)
x

# Oblicz:
# • sumę wszystkich elementów,
sum(x)
# • wartość średnią wszystkich elementów,
mean(x)
# • iloczyn elementów,
prod(x)
# • logarytm dziesiętny wszystkich elementów,
log10(x)
# • różnicę pomiędzy największym i najmniejszym elementem wektora x,
max(x)-min(x)
# • ile elementów jest większych od 3,
sum(x>3)
# • zsumuj elementy większe od 3
sum(x[x>3])
# • pokaż ile jest elementów wektora x,
length(x)
# • posortuj rosnąco,
sort(x)
# • posortuj malejąco ( decreasing=TRUE)
sort(x,decreasing=TRUE)
# • pokaż jaki jest zakres (min i max?).
min(x) 
max(x)
# • wskaż indeks najmniejszego elementu
which.min(x)
# • wskaż indeks największego elementu
which.max(x)
# • wskaż indeksy gdzie występuje najmniejszy element
which(x==min(x))
