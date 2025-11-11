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

# FAZA: ČIŠĆENJE PODATAKA

data_clean = data
data_clean
# za svaki slucaj radimo sa rezervom originalnog skupa ako nesto pogresimo imamo original podatke netaknute

filter(data_clean, os == "macOS" & price < 1000)
# sa grafika odnosa cena i os-a su nam bili sumnjivi uredjaji sa macOS jeftiniji od 1000$ i sada vidimo da su oni uglavnom od drugog nekog proizvodjaca sto je nemoguce

macos_nije_apple = data_clean %>% filter(os == "macOS" & brand != "Apple")
nrow(macos_nije_apple)
# postoji 16032 reda kojima je proizvodjac uredjanja neka kompanija a da nije apple a imaju mac os sto nije moguce i nije ni zakonski
# ako se ne gleda samo zvanicno postoje zajednice koje se bave podizanjem mac os na ne apple pc i laptopove ali to nista nije oficijalno i ovde su podaci samo o novim stvarima
# iako je 16032 dosta dobar deo od 100k podaci su nerealni i netacni pa ce biti sklonjeni

data_clean = data_clean %>% filter(!(os == "macOS" & brand != "Apple"))
nrow(data_clean)
# ocistili smo ih


apple_intel <- data_clean %>% filter(brand == "Apple" & cpu_brand == "Intel")
apple_applecpu <- data_clean %>% filter(brand == "Apple" & cpu_brand == "Apple")
# sada gledamo i obrnuto kao podrazumevani os posto pricamo samo ovde o novim uredjajima apple uredjaji mogu imati windows ili nesto da nije macos samo ako imaju intel procesor a ne apple silicon
nrow(apple_intel)
nrow(apple_applecpu)
# ne postoje apple uredjaji sa intel cpu-om tako da sklanjamo sve sto je od apple-a a ima nesto da nije macos

apple_nije_macos = data_clean %>% filter(brand == "Apple" & os != "macOS")
nrow(apple_nije_macos)
# ima ovakvih redova 9740 brisemo ih jer ne mogu da postoje
data_clean = data_clean %>% filter(!(brand == "Apple" & os != "macOS"))
nrow(data_clean)
# za ive 2 prethodne stvari koristili smo domensko znanje

# proveravamo neke nelogicne vrednosti, sa grafika vidimo da ne postije negativne vrednosti cene, neke memorije vram, ram i slicno, pa to ne proveravamo, ali proveravamo ove ispod
desktop_with_battery = data_clean %>% filter(device_type == "Desktop" & battery_wh > 0)
nrow(desktop_with_battery)
# da li postoji racunar sa baterijom, nema ih
laptop_no_battery <- data_clean %>% filter(device_type == "Laptop" & battery_wh == 0)
nrow(laptop_no_battery)
# ili mozda laptop bez baterije, takodje ih nema

n_before <- nrow(data_clean)
data_clean <- data_clean %>%
  filter(!(release_year == 2021 & price > 9000))
cat("Obrisano outliera (2021/2022 preko 9000$):", n_before - nrow(data_clean), "\n")
# u 2021 postoji uredjaj sa cenom od preko 9000$ a te godine jos nisu postojale toliko skupe komponente, pa bi trebalo da sklonimo ovaj uredjaj, posto je ovde rezultat 0 znaci da je ovaj uredjaj vec sklonen ranije zbog macos ili apple koji ima neki drugi os, sto dodatno potrvdjuje da ovaj uredjaj nije bio realan da postoji

data_clean <- data_clean %>%
  filter(!(device_type == "Desktop" & price > 8000)) %>%
  filter(!(device_type == "Laptop" & price > 9500))

cat("Obrisano outliera po tipu uređaja:", n_before - nrow(data_clean), "\n")
# sa grafika tipa uredjaja i cene ostavicemo laptopove koji kostaju do 10k dolara jer su oni integrisani i skupih komponenti pa i mogu mnogo kostati (preko 10k brisemo) ali racunari preko 8k dolara su nerealni i njih cemo skloniti

n_before <- nrow(data_clean)

###########
data_clean <- data_clean %>%
  # uređaji sa vrlo malo RAM-a, a previsokom cenom
  filter(!(ram_gb < 16 & price > 6000)) %>%
  # uređaji sa ogromnim RAM-om, a vrlo niskom cenom
  filter(!(ram_gb > 100 & price < 2000))

cat("Obrisano RAM outliera:", n_before - nrow(data_clean), "\n")
# uredjaji sa manje od 16gb rama i cenom preko 6000$ nisu realni kao i uredjaji od preko 100gb rama i cenom manjom od 2k dolara to uopste ne spada u cenovni rang koji treba kao ni ovo prvo i takve vrednosti uklanjamo
########### ipak bih rekao da ovde nema nekih preteranih vrednosti odredjenih outliera ima za skoro svaku vrednost ram-a, ali su u nekim granicama normale
n_before <- nrow(data_clean)

data_clean <- data_clean %>%
  filter(!(gpu_tier == 1 & price > 6000))
cat("Obrisano GPU tier outliera:", n_before - nrow(data_clean), "\n")
# postoje uredjaji koji kostaju preko 6000$, a najnizeg su nivoa graficke kartice, što nije moguće, kakve god da su im druge komponente i outlier je ističe se od drugih tačaka, pa ćemo to obrisati

n_before <- nrow(data_clean)
data_clean <- data_clean %>%
  filter(!(resolution %in% c("3840x2160", "3440x1440") & price < 500))
cat("Obrisano GPU tier outliera:", n_before - nrow(data_clean), "\n")
# uređaji sa maksimalno rezolucijom i cenom ispod 500$ nisu mogući čak i sa najjefitnijim komponentama, pa ćemo ih obrisati

n_before <- nrow(data_clean)

data_clean <- data_clean %>%
  filter(!(os == "macOS" & price < 700))
# postoje uređaji sa macOS ispod 700 što je nerealno jeftino čak i za polovne modele, a ovde pričamo o novima, a rezultat ovog kod će biti 0 takvih podataka, jer je to bio uređaj neke druge marke sa macOS što nam drugi put potvrđuje da ovaj uređaj nije realan da postoji
  
data_clean = data_clean %>%
  filter(!(os == "ChromeOS" & price > 9000))
# brisemo i uređaje koji sadrže chromeOS i koštaju mnogo što je nemoguće čak i sa vrhunskim komponentama, rezultat je opet 0, jer je ovaj uređaj prethodno obrisan zbog drugih analiza

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



















