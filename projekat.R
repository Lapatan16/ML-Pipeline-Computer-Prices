## komentari sa dve tarabe su samo za nas necemo kaciti na git

# ukljucivanje svih potrebnih biblioteka

library(tidyverse)
library(ggplot2)
library(scales)

# ucitavanje skupa podataka i prikaz osnovnih stvari
data = read.csv("computer_prices_all.csv")
data
names(data) ## prikazuje nazive kolona
str(data) ## prikazuje koliko redova i kolona ima u skupu i tip podatka svake promenljive

# analiza raspodele ciljne promenljive
summary(data$price) ## sve osnovne podatke o price srednja vr medijana min max i slicno

# Histogram ciljne promenljive price
ggplot(data, aes(x = price)) +
  geom_histogram(bins = 50, fill = "#1f78b4", color = "black", alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Distribucija promenljive 'price'",
    x = "Cena uređaja (USD)",
    y = "Broj uređaja"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
## alpha je samo kolika ce biti prozirnost stubica 0.2 npr dosta svetlo 0.8 dosta tamno
## theme minimal pozadina nije siva vec bela moglo i bez toga, bins je broj stubica na koji ce grafik biti podeljen
## ovo scale x i y samo sluzi da na hiljadam bude , da bude citljivije ne 10000 vec 10,000
## ovo theme samo centrira i bolduje naslov nista vise

# Boxplot za outliere
ggplot(data, aes(y = price)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Boxplot cene uređaja",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
## slicno kao proslo samo nam ovo pokazuje i koliko imamo potencijalnih outliera tj uredjaja sa dosta visokom cenom koji odskacu od ostatka podataka

# Vizuelizacija podataka (crtamo grafike kako bismo izvukli korisne informacije, nakon toga ih obradjujemo)

# boxplot cene uređaja u odnosu na godinu izdavanja, godine se krecu od 2018 do 2025
ggplot(data, aes(x = factor(release_year), y = price)) +
  geom_boxplot(fill = "#1f78b4", color = "black", alpha = 0.7, outlier.colour = "red") +
  labs(
    title = "Cena uređaja u odnosu na godinu izdavanja",
    x = "Godina izdavanja",
    y = "Cena (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# boxplot cene uređaja u odnosu na tip

ggplot(data, aes(x = device_type, y = price, fill = device_type)) +
  geom_boxplot(alpha = 0.8, outlier.colour = "red", outlier.shape = 8) +
  labs(
    title = "Cena uređaja u odnosu na tip uređaja",
    x = "Tip uređaja",
    y = "Cena (USD)"
  ) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

# grafik cene u odnosu na ram memoriju
ggplot(data, aes(x = ram_gb, y = price)) +
  geom_point(color = "black", size = 1) +
  labs(
    title = "Odnos RAM memorije i cene uređaja",
    x = "RAM memorija (GB)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# grafik cene u odnosu na broj jezgara procesora
ggplot(data, aes(x = cpu_cores, y = price)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Odnos broja jezgara procesora i cene uređaja",
    x = "Broj jezgara procesora",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

# boxplot cene u zavisnosti od ranga procesora
ggplot(data, aes(x = as.factor(cpu_tier), y = price)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Odnos između CPU Tier-a i cene uređaja",
    x = "CPU Tier (rang procesora)",
    y = "Cena (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# boxplot cne u zavisnosti od ranga grafičke kartice

ggplot(data, aes(x = as.factor(gpu_tier), y = price)) +
  geom_boxplot() +
  labs(
    title = "Odnos između GPU Tier-a i cene uređaja",
    x = "GPU Tier (rang grafičke kartice)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafik odnosa cene u odnosu na vram memoriju

ggplot(data, aes(x = vram_gb, y = price)) +
  geom_point() +
  labs(
    title = "Odnos između VRAM memorije i cene uređaja",
    x = "Video memorija (GB)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafik cene u zavisnosti od količine memorije

ggplot(data, aes(x = storage_gb, y = price)) +
  geom_point() +
  labs(
    title = "Odnos između kapaciteta skladišta i cene uređaja",
    x = "Kapacitet skladišta (GB)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot-ovi cene u zavisnosti od tipa memorije

ggplot(data, aes(x = storage_type, y = price)) +
  geom_boxplot() +
  labs(
    title = "Raspodela cena u odnosu na tip skladišta podataka",
    x = "Tip skladišta (storage_type)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_y_continuous(limits = c(0, 11000))

# Grafik cene uređaja u odnosu na veličinu ekrana

ggplot(data, aes(x = display_size_in, y = price)) +
  geom_point() +
  labs(
    title = "Odnos veličine ekrana i cene uređaja",
    x = "Veličina ekrana (inči)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafik cene uređaja u odnosu na rezolicuju

ggplot(data, aes(x = resolution, y = price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Raspodela cena u odnosu na rezoluciju ekrana",
    x = "Rezolucija ekrana",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1)
  )

# grafik zavisnosti cene od frekvencije osvezavanja tj refresh rate-a

ggplot(data, aes(x = refresh_hz, y = price)) +
  geom_point(alpha = 0.5, color = "black") +
  labs(
    title = "Odnos frekvencije osvežavanja ekrana i cene uređaja",
    x = "Frekvencija osvežavanja (Hz)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# grafik zavisnosti cene od kapaciteta baterije uređaja

ggplot(data, aes(x = battery_wh, y = price)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Odnos kapaciteta baterije (Battery Wh) i cene uređaja",
    x = "Kapacitet baterije (Wh)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Grafik zavisnosti cene uređaja od njegove težine

ggplot(data, aes(x = weight_kg, y = price)) +
  geom_point() +
  labs(
    title = "Odnos težine uređaja i cene uređaja",
    x = "Težina uređaja (kg)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Grafik zavisnosti cene od brzine procesora

ggplot(data, aes(x = cpu_base_ghz, y = price)) +
  geom_point() +
  labs(
    title = "Odnos brzine procesora i cene uređaja",
    x = "Osnovna brzina procesora (GHz)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Grafik zavisnosti cene od brzine procesora

ggplot(data, aes(x = cpu_brand, y = price)) +
  geom_boxplot() +
  labs(
    title = "Cena uređaja u odnosu na brend procesora",
    x = "Brend procesora",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Boxplot-ovi zavisnosti cene u odnosu na operativni sistem uređaja

ggplot(data, aes(x = os, y = price)) +
  geom_boxplot() +
  labs(
    title = "Cena uređaja u odnosu na operativni sistem",
    x = "Operativni sistem",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Boxplot-ovi zavisnosti cene u odnosu na proizvođača uređaja

ggplot(data, aes(x = brand, y = price)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Cena uređaja u odnosu na brend proizvođača",
    x = "Brend uređaja",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

##############################
##############################

# FAZA: ČIŠĆENJE I OBRADA PODATAKA

datav2 = data
datav2
# za svaki slučaj radimo sa rezervom originalnog skupa, ako nesto pogrešimo imamo original podatke netaknute

# 1) provera NA vrednosti

colSums(is.na(datav2))
# vidimo da u datasetu ne postoje NA tj. vrednosti koje fale već su svi podaci za svih 100k redova popunjeni

# 2) pogrešno unete vrednosti

# analizom skupa podataka vizuelno i analizom grafika iz prethodnog koraka nisu oučene neke pogrešne vrednosti npr. tip uređaja da negde bude Device negde device
# ili uređaj od 100kg, negativna vrednost cene, memorije ili nečeg sličnog

# 3) nelogične vrednosti

nrow(filter(datav2, os == "macOS" & price < 1000))
# sa grafika odnosa cene i os-a (slika broj) su nam bili sumnjivi uređaji sa macOS jeftiniji od 1000$ i sada vidimo da su oni uglavnom od nekog drugog proizvođaca što u stvarnosti nije moguće

macos_nije_apple = datav2 %>% filter(os == "macOS" & brand != "Apple")
nrow(macos_nije_apple)
# postoji 16032 reda kojima je proizvođač uređaja neka kompanija koja nije apple, a imaju mac os što nije moguće i nije ni zakonski, apple ne dozvoljava instaliranje mac os na proizvodima drugih kompanija
# ako se ne gleda samo zvanično, postoje zajednice koje se bave podizanjem mac os na ne-apple računare i laptopove, ali to ništa nije oficijalno i ovde su podaci samo o novim uređajima
# iako je 16032 dosta dobar deo od 100k podaci su nerealni i netačni, pa će biti uklonjeni

datav2 = datav2 %>% filter(!(os == "macOS" & brand != "Apple"))
nrow(datav2)
# sada nam ostaje 83968, uklonjeno je oko 16% postojećih podataka
# znamo i da apple uređaji ne mogu imati neki drugi os osim ako npr imaju intel procesor što je i bio slučaj do pre nekoliko godina, pa ćemo proveriti i takve

apple_intel_procesor <- datav2 %>% filter(brand == "Apple" & cpu_brand == "Intel")
nrow(apple_intel_procesor)
# apple uređaja sa intel procesorima nema

apple_apple_procesor <- datav2 %>% filter(brand == "Apple" & cpu_brand == "Apple")
nrow(apple_apple_procesor)
# apple uređaja imamo 11915 i pošto ih nema sa intel procesorom, tražimo sve koji nemaju macos

apple_nije_macos = datav2 %>% filter(brand == "Apple" & os != "macOS")
nrow(apple_nije_macos)
# ovakvih redova ima 9740, ti podaci nisu realni i brišemo ih, uklonjeno je oko 11,5% postojećih podataka

datav2 = datav2 %>% filter(!(brand == "Apple" & os != "macOS"))
nrow(datav2)
# za prethodne 2 stvari smo koristili domensko znanje, koje nam je mnogo pomoglo da uočimo nepravilnosti i da ih potom ispitamo

# proveravamo još neke nelogične vrednosti
desktop_with_battery = datav2 %>% filter(device_type == "Desktop" & battery_wh > 0)
nrow(desktop_with_battery)
# provera da li postoji računar sa baterijom, nema ih

laptop_no_battery <- datav2 %>% filter(device_type == "Laptop" & battery_wh == 0)
nrow(laptop_no_battery)
# ili možda laptop bez baterije, takođe ih nema

# 4) analiza i potencijalno izbacivanje outlier-a

preskupi_2021 = datav2 %>% filter(release_year == 2021 & price > 9000)
nrow(preskupi_2021)
# sa grafika zavisnosti cene od godine izdavanja uređaja (slika broj 9) u 2021 postoji uredjaj sa cenom od preko 9000$, a te godine još nisu postojale toliko skupe komponente, pa bi trebalo da sklonimo ovaj uređaj
# pošto je ovde rezultat 0 znači da je ovaj uređaj već sklonjen ranije zbog nekih nelogičnih vrednosti, što dodatno potrvđuje da ovaj uređaj nije bio realan da postoji

preskupi_laptopovi = datav2 %>% filter(device_type == "Desktop" & price > 8000)
preskupi_racunari = datav2 %>% filter(device_type == "Laptop" & price > 10000)
# sa grafika zavisnosti cene od tipa uređaja (slika broj 11) ostavićemo laptopove koji koštaju do 10000$, jer oni sadrže integrisane i skupe komponente, pa i mogu mnogo koštati (oni preko 10k su već nerealni sa bilo kakvim komponentama i brišemo ih) 
# računari su jeftiniji od laptopova tako da do 8000$ je maksimalna granica otprilike koliko mogu da koštaju, pa ćemo sve sa cenom preko 8000$ skloniti

datav2 = datav2 %>% filter(!((device_type == "Desktop" & price > 8000) | (device_type == "Laptop" & price > 10000)))
# obrisali smo 4 ovakva podatka

skupi_slab_gpu = datav2 %>% filter(gpu_tier == 1 & price > 6000)
# sa grafika zavisnosti cene od ranga grafičke kartice (slika broj 19) postoje uređaji koji koštaju preko 6000$, a najnižeg su nivoa grafičke kartice, što nije moguće, kakve god da su im druge komponente i ovaj podatak se dosta ističe od drugih, pa ćemo ga obrisati

datav2 = datav2 %>% filter(!(gpu_tier == 1 & price > 6000))
# obrisali smo jedan podatak

jefitini_ogromna_rezolucija = datav2 %>% filter(resolution %in% c("3440x1440", "3840x2160") & price < 500)
# sa grafika zavisnosti cene od rezolucije ekrana (slika broj 29) postoje uređaji sa maksimalnom rezolucijom i cenom ispod 500$, što je nemoguće kakve god da su druge komponente, pa ćemo ih obrisati

datav2 = datav2 %>% filter(!(resolution %in% c("3440x1440", "3840x2160") & price < 500))
# obrisana su 2 podatka

prejeftini_macovi = datav2 %>%
  filter(os == "macOS" & price < 700)
nrow(prejeftini_macovi)
# sa grafika zavisnosti cene od os-a (slika broj 41) postoje uređaji sa macOS ispod 700 što je nerealno jeftino čak i za polovne modele, a ovde pričamo o novima
# rezultat ovog koda će biti 0 takvih podataka, jer je to bio uređaj koji je prethodno uklonjen zbog nečeg drugog, što nam drugi put potvrđuje da ovaj uređaj nije realan da postoji
  
preskupi_chromeos = datav2 %>%
  filter(os == "ChromeOS" & price > 9000)
nrow(preskupi_chromeos)
# sa grafika zavisnosti cene od os-a (slika broj 41) vidimo da postoje uređaji koji imaju chromeOS koji imaju slabiji uređaji i uopšteno ga nema kod skupljih, rezultat za uređaj sa ovim os-om kakve god da su druge komponente, pa ćemo obrisati
# rezultat ovog koda jeste 0, što nam govori da ovaj podatak i treba izbaciti, što smo ovde i drugi put dokazali, jer su uređaji sklonjeni prilikom nekog od prethodnih čišćenja podataka

# nakon svih čišćenja podataka ostalo je 74221 podataka, odnosno uklonjeno je oko 26% podataka

### Multivarijantni modeli

ggplot(data = data) + geom_point(mapping = aes(x = weight_kg, y = price, color = release_year))

ggplot(data = data) + geom_point(mapping = aes(x = cpu_tier, y = ram_gb, color = os))
# na osnovu toga koji je operativni sistem mozemo pretpostaviti koliko rama ima i cpu_tier
# vidimo da Windows podrazumeva u vecini slucajeva jaci racunar, dok linux i macOS nisu toliko zahtevni

ggplot(data = data) + geom_point(mapping = aes(x = cpu_tier, y = price, color = cpu_brand), position = "jitter")
# sa grafika mozemo videti da su Apple procesori generalno skuplji od ostalih za isti tier
# dok su AMD i Intel podjednake cene

ggplot(data = data) + geom_point(mapping = aes(x = storage_gb, y = price, color = device_type), position = "jitter", alpha = 1/5)

cor(data[,c(4, 9, 10, 11, 12, 13, 16, 17, 18, 20, 21, 23, 25, 26, 27, 28, 30, 31, 32, 33)])



















