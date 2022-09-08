# rrr
rrr

```{r}
#1 Utwórz wektor i nazwij go zarobki. Przypisz do niego następujące wartości: 2300, 1800, 4500, 3700, 1300, 7200, 6100. Nazwij każdy element wektora dowolnym imieniem. Wyświetl wektor z nazwanymi elementami, najmniejszą wartość, jego długość i oblicz średnią wartość.
zarobki <- c(jan=2300, tomek=1800, maciek=4500, grzegorz=3700, bartek=1300, kacper=7200, janusz=6100)
zarobki
min(zarobki)
length(zarobki)
mean(zarobki)

#2 Wczytaj dołączony plik Hotel_booking_satisfaction.csv do obiektu o nazwie hotele.  Wyświetl utworzony obiekt hotele.
hotele <- read.table(file="C:\\Users\\barto\\Desktop\\europa_hotel.csv", sep=",", dec=".",header=TRUE)
hotele

#3 Z ramki danych hotele wyświetl: •3 pierwsze wiersze, •3 ostatnie wiersze, •nazwy wszystkich kolumn.
head(hotele,3)
tail(hotele,3)
names(hotele)

#4 Wyświetl dane dla kolumny „Gender” z obiektu hotele.
gener <- hotele %>%
  select(Gender)
gener

#5 Wyświetl informację, czy wartości z kolumny „Cleanliness” są równe bądź większe od 3.
clean <- hotele %>%
    mutate(isCleanliness=if_else(Cleanliness>=3,"tak","nie"))
clean

#6 Oblicz średni wynik dla kolumny „Food.and.drink” i zaokrąglij go do dwóch miejsc po przecinku.
sredni <- hotele %>%
  summarise(sredni_wynik=round(mean(Food.and.drink),2))
sredni

#7 Uporządkuj dane według kolumny „pupose_of_travel”.
uporz <- hotele %>%
  arrange(purpose_of_travel)
uporz

#8 Wyświetl liczebność według danych z kolumny „Type.Of.Booking” i zapisz to do wektora, który nazwiesz swoim nazwiskiem.
wektor<-hotele%>%
  group_by(Type.Of.Booking)%>%
 summarise(liczba=n())%>%
  select(liczba)
pulczynski=c(wektor)
pulczynski

#9 Przygotuj wykres kolumnowy przedstawiający liczebność danych z kolumny „Stay.comfort”. Wstaw tytuł wykresu, nazwij osie, ustaw różne kolory dla poszczególnych słupków. Wygeneruj plik graficzny z wykresem.
wykres<-hotele%>%
  group_by(Stay.comfort)%>%
 summarise(liczba=n())
wykres
barplot(wykres$liczba, main = "Stay.comfort", ylab="responses", ylim = c(0,35000),col=c("red", "green", "blue","orange","pink", "yellow"), names.arg = c("0","1","2","3","4","5") )
#10 W nazwach kolumn w obiekcie hotele zamień „.” na „_”, wykorzystując funkcję gsub.
colnames(hotele) <- gsub("\\.+", "_", colnames(hotele))
hotele

#11 Utwórz funkcję o nazwie podatek, która obliczy, ile podatku należy zapłacić od podanego towaru i zwróci odpowiedź: „Od (podana wartość) złotych należy zapłacić (wyliczona wartość) złotych podatku”. Wyliczona wartość ma być zaokrąglona do 2 miejsc po przecinku. Przyjmij, że podatek wynosi 13%.
podatek <- function(kwota)
{
  print(paste("Od ",kwota," złotych należy zapłacić ", round(kwota*0.13,2)," złotych podatku."))
}
podatek(100)

#12 Napisz program, który sprawdzi, czy liczba jest nieparzysta. Przy spełnieniu tego warunku od podanej wartości odejmie 3i poda odpowiedź, a w przeciwnym razie wyświetli komunikat: „Uwaga! Liczba jest parzysta”.
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
