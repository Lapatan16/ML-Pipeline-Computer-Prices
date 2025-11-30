# UVOD

# Ukljucivanje svih potrebnih biblioteka

library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)
library(corrplot)
library(ggcorrplot)
library(stringr)

# Ucitavanje skupa podataka i prikaz osnovnih stvari

data = read.csv("computer_prices_all.csv")
data
names(data) 
str(data)

# Analiza raspodele ciljne promenljive

summary(data$price)

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

# Boxplot ciljne promenljive price

ggplot(data, aes(y = price)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Boxplot cene uređaja",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# VIZUELIZACIJA PODATAKA

# Distribucija tipova uređaja

ggplot(data, aes(device_type)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribucija tipova uređaja",
    x = "Tip uređaja",
    y = "Broj uređaja"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Distribucija CPU tier kategorija

ggplot(data, aes(cpu_tier)) +
  geom_bar(fill = "orange", color = "black") +
  labs(
    title = "Distribucija CPU tier kategorija",
    x = "CPU tier",
    y = "Broj uređaja"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Distribucija GPU tier kategorija

ggplot(data, aes(gpu_tier)) +
  geom_bar(fill = "firebrick", color = "black") +
  labs(
    title = "Distribucija GPU tier kategorija",
    x = "GPU tier",
    y = "Broj uređaja"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Distribucija operativnih sistema

ggplot(data, aes(os)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "Distribucija operativnih sistema",
    x = "Operativni sistem",
    y = "Broj uređaja"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafik cene uređaja u odnosu na godinu izdavanja, godine se krecu od 2018 do 2025

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

# Grafik cene u odnosu na tip uređaja

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

# Grafik cene uređaja u odnosu na RAM memoriju

ggplot(data, aes(x = ram_gb, y = price)) +
  geom_point(color = "black", size = 1) +
  labs(
    title = "Cena uređaja u odnosu na RAM memoriju",
    x = "RAM memorija (GB)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Grafik cene uređaja u odnosu na broj jezgara procesora

ggplot(data, aes(x = cpu_cores, y = price)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Cena uređaja u odnosu na broj jezgara procesora",
    x = "Broj jezgara procesora",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )

# Grafik cene uređaja u odnosu na rang procesora

ggplot(data, aes(x = as.factor(cpu_tier), y = price)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Cena uređaja u odnosu na CPU Tier",
    x = "CPU Tier (rang procesora)",
    y = "Cena (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafik cene uređaja u odnosu na rang grafičke kartice

ggplot(data, aes(x = as.factor(gpu_tier), y = price)) +
  geom_boxplot() +
  labs(
    title = "Cena uređaja u odnosu na GPU Tier",
    x = "GPU Tier (rang grafičke kartice)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafik cene uređaja u odnosu na vram memoriju

ggplot(data, aes(x = vram_gb, y = price)) +
  geom_point() +
  labs(
    title = "Cena uređaja u odnosu na VRAM memoriju",
    x = "Video memorija (GB)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafik cene uređaja u odnosu na količinu memorije

ggplot(data, aes(x = storage_gb, y = price)) +
  geom_point() +
  labs(
    title = "Cena uređaja u odnosu na kapaciteta skladišta",
    x = "Kapacitet skladišta (GB)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafik cene uređaja u odnosu na tip memorije

ggplot(data, aes(x = storage_type, y = price)) +
  geom_boxplot() +
  labs(
    title = "Cena uređaja u odnosu na tip skladišta podataka",
    x = "Tip skladišta (storage_type)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafik cene uređaja u odnosu na veličinu ekrana

ggplot(data, aes(x = display_size_in, y = price)) +
  geom_point() +
  labs(
    title = "Cena uređaja u odnosu na veličinu ekrana",
    x = "Veličina ekrana (inči)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafik cene uređaja u odnosu na rezoluciju ekrana

ggplot(data, aes(x = resolution, y = price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Cena uređaja u odnosu na rezoluciju ekrana",
    x = "Rezolucija ekrana",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1)
  )

# Grafik cene u odnosu na frekvenciju osvežavanja tj refresh rate

ggplot(data, aes(x = refresh_hz, y = price)) +
  geom_point(alpha = 0.5, color = "black") +
  labs(
    title = "Cena uređaja u odnosu na frekvenciju osvežavanja ekrana",
    x = "Frekvencija osvežavanja (Hz)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Grafik cene u odnosu na kapacitet baterije uređaja

ggplot(data, aes(x = battery_wh, y = price)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Cena uređaja u odnosu na kapacitet baterije (Battery Wh)",
    x = "Kapacitet baterije (Wh)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Grafik cene uređaja u odnosu na njegovu težinu

ggplot(data, aes(x = weight_kg, y = price)) +
  geom_point() +
  labs(
    title = "Cena uređaja u odnosu na njegovu težinu",
    x = "Težina uređaja (kg)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Grafik cene uređaja u odnosu na brzinu procesora

ggplot(data, aes(x = cpu_base_ghz, y = price)) +
  geom_point() +
  labs(
    title = "Cena uređaja u odnosu na brzinu procesora",
    x = "Osnovna brzina procesora (GHz)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Grafik cene uređaja u odnosu na proizvođača procesora

ggplot(data, aes(x = cpu_brand, y = price)) +
  geom_boxplot() +
  labs(
    title = "Cena uređaja u odnosu na proizvođača procesora",
    x = "Brend procesora",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Grafik cene u odnosu na operativni sistem uređaja

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

# Grafik cene u odnosu na proizvođača uređaja

ggplot(data, aes(x = brand, y = price)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Cena uređaja u odnosu na proizvođača",
    x = "Brend uređaja",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Kombinacije podataka radi boljeg prepoznavanja trendova

# Uticaj tipa eksterne memorije na količinu memorije i cenu uređaja

ggplot(data, aes(x = storage_gb, y = price, color = storage_type)) +
  geom_point(alpha = 1/3) +
  facet_wrap(~ storage_type) +
  theme_minimal() + labs(
    title = "Uticaj tipa eksterne memorije na količinu memorije i cenu uređaja",
    x = "Količina memorije",
    y = "Cena u dolarima"
  )+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Uticaj marke procesora na cenu po rangu

ggplot(data, aes(x = cpu_tier, y = price, color = cpu_brand)) +
  geom_point(alpha = 1/3) +
  facet_wrap(~ cpu_brand) +
  theme_minimal() + labs(
    title = "Uticaj marke procesora na cenu po rangu",
    x = "Rang procesora (tier)",
    y = "Cena u dolarima"
  )+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Uticaj marke računara i garancije u mesecima na cenu

ggplot(data, aes(x = warranty_months, y = price, color = brand)) +
  geom_point(alpha = 1/3) +
  facet_wrap(~ brand) +
  theme_minimal() + labs(
    title = "Uticaj marke računara i garancije u mesecima na cenu",
    x = "Garancija u mesecima",
    y = "Cena u dolarima"
  )+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Uticaj marke i ranga grafičke kartice na cenu

ggplot(data, aes(x = gpu_tier, y = price, color = gpu_brand)) +
  geom_point(alpha = 1/3) +
  facet_wrap(~ gpu_brand) +
  theme_minimal() + labs(
    title = "Uticaj marke i ranga grafičke kartice na cenu",
    x = "Rang grafičke kartice (tier)",
    y = "Cena u dolarima"
  )+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Uticaj ranga procesora i ranga grafičke kartice na cenu

ggplot(data, aes(x = cpu_tier, y = price, color = gpu_tier)) +
  geom_point(alpha = 1/3) +
  facet_wrap(~ gpu_tier) +
  theme_minimal() + labs(
    title = "Uticaj ranga procesora i ranga grafičke kartice na cenu",
    x = "Rang procesora (tier)",
    y = "Cena u dolarima"
  )+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# ČIŠĆENJE I OBRADA PODATAKA

datav2 = data
datav2

# 1) provera NA vrednosti

colSums(is.na(datav2))

# 2) pogrešno unete vrednosti

# Analizom skupa podataka vizuelno i analizom grafika iz prethodnog koraka nisu oučene neke pogrešne vrednosti npr. tip uređaja da negde bude Device negde device
# ili uređaj od 100kg, negativna vrednost cene, memorije ili nečeg sličnog

# 3) nelogične vrednosti

nrow(filter(datav2, os == "macOS" & price < 1000))

macos_nije_apple = datav2 %>% filter(os == "macOS" & brand != "Apple")
nrow(macos_nije_apple)

datav2 = datav2 %>% filter(!(os == "macOS" & brand != "Apple"))
nrow(datav2)

ggplot(data = datav2) + geom_point(mapping = aes(x = os, y = brand)) + theme_minimal() + labs(
  title = "Operativni sistemi na različitim proizvođačima",
  x = "Operativni sistem",
  y = "Marka proizvođača"
) +
theme(
  plot.title = element_text(hjust = 0.5, face = "bold")
)

apple_intel_procesor = datav2 %>% filter(brand == "Apple" & cpu_brand == "Intel")
nrow(apple_intel_procesor)

apple_apple_procesor = datav2 %>% filter(brand == "Apple" & cpu_brand == "Apple")
nrow(apple_apple_procesor)

apple_nije_macos = datav2 %>% filter(brand == "Apple" & os != "macOS")
nrow(apple_nije_macos)

datav2 = datav2 %>% filter(!(brand == "Apple" & os != "macOS"))
nrow(datav2)

ggplot(data = datav2) + geom_point(mapping = aes(x = os, y = brand)) + theme_minimal() + labs(
  title = "Operativni sistemi na različitim proizvođačima",
  x = "Operativni sistem",
  y = "Marka proizvođača"
)

# Proveravamo još neke nelogične vrednosti

desktop_with_battery = datav2 %>% filter(device_type == "Desktop" & battery_wh > 0)
nrow(desktop_with_battery)

laptop_no_battery = datav2 %>% filter(device_type == "Laptop" & battery_wh == 0)
nrow(laptop_no_battery)

# 4) Analiza i potencijalno izbacivanje outlier-a

preskupi_2021 = datav2 %>% filter(release_year == 2021 & price > 9000)
nrow(preskupi_2021)

preskupi_laptopovi = datav2 %>% filter(device_type == "Desktop" & price > 8000)
preskupi_racunari = datav2 %>% filter(device_type == "Laptop" & price > 10000)
nrow(preskupi_laptopovi)
nrow(preskupi_racunari)

datav2 = datav2 %>% filter(!((device_type == "Desktop" & price > 8000) | (device_type == "Laptop" & price > 10000)))

ggplot(data = datav2) + geom_point(mapping = aes(x = device_type, y = price)) + theme_minimal() + labs(
  title = "Odnos tipa uređaja i cene",
  x = "Tip uređaja",
  y = "Cena (USD)"
)

skupi_slab_gpu = datav2 %>% filter(gpu_tier == 1 & price > 6000)
nrow(skupi_slab_gpu)

datav2 = datav2 %>% filter(!(gpu_tier == 1 & price > 6000))

ggplot(data = datav2) + geom_point(mapping = aes(x = gpu_tier, y = price)) + theme_minimal() + labs(
  title = "Odnos ranga grafičke kartice i cene",
  x = "Rang grafičke kartice",
  y = "Cena (USD)"
)

jeftini_ogromna_rezolucija = datav2 %>% filter(resolution %in% c("3440x1440", "3840x2160") & price < 500)
nrow(jeftini_ogromna_rezolucija)

datav2 = datav2 %>% filter(!(resolution %in% c("3440x1440", "3840x2160") & price < 500))

ggplot(data = datav2) + geom_point(mapping = aes(x = resolution, y = price)) + theme_minimal() + labs(
  title = "Odnos rezolucije ekrana i cene",
  x = "Rezolucija ekrana",
  y = "Cena (USD)"
)

prejeftini_macovi = datav2 %>% filter(os == "macOS" & price < 700)
nrow(prejeftini_macovi)

preskupi_chromeos = datav2 %>% filter(os == "ChromeOS" & price > 9000)
nrow(preskupi_chromeos)

# EDA

numericke_kolone = datav2 %>% select_if(is.numeric)
names(numericke_kolone)

matrica_korelacije = cor(numericke_kolone, use = "complete.obs")
matrica_korelacije

sort(matrica_korelacije[,"price"], decreasing = TRUE)

ggcorrplot(
  matrica_korelacije,
  hc.order = TRUE,           
  type = "full",             
  lab = TRUE,                
  lab_size = 2.5,            
  colors = c("red", "white", "blue"),
  outline.col = "gray",
  ggtheme = ggplot2::theme_minimal()
) +
labs(
  title = "Korelaciona matrica numeričkih promenljivih",
  subtitle = "Prikaz svih parova promenljivih (gornji i donji trougao)"
) +
theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  plot.subtitle = element_text(hjust = 0.5, size = 12),
  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
)

datav2$device_type = as.factor(datav2$device_type)
datav2$brand = as.factor(datav2$brand)
datav2$os = as.factor(datav2$os)
datav2$storage_type = as.factor(datav2$storage_type)
datav2$cpu_brand = as.factor(datav2$cpu_brand)
datav2$gpu_brand = as.factor(datav2$gpu_brand)
datav2$display_type = as.factor(datav2$display_type)
datav2$resolution = as.factor(datav2$resolution)
datav2$wifi = as.factor(datav2$wifi)
datav2$form_factor = as.factor(datav2$form_factor)
datav2$cpu_tier = factor(
  datav2$cpu_tier,
  levels = sort(unique(datav2$cpu_tier)),
  ordered = TRUE
)
datav2$gpu_tier = factor(
  datav2$gpu_tier,
  levels = sort(unique(datav2$gpu_tier)),
  ordered = TRUE
)

# nakon iscrtavanja početnih grafika i njihove analize, čišćenja i sređivanja podataka i naravno uz pomoć domenskog znanja u EDA fazi izdvojićemo nekoliko grafika koje smatramo da su najbitniiji

# 1) Grafik cene u odnosu na rang procesora

ggplot(datav2, aes(x = cpu_tier, y = price)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Cena u odnosu na CPU Tier",
       x = "CPU Tier", y = "Cena (USD)") +
  theme_minimal()

# ANOVA test za cpu_tier obeležje

anova_cpu = aov(price ~ cpu_tier, data = datav2)
summary(anova_cpu)

# 2) Grafik cene u odnosu na rang grafičke kartice

ggplot(datav2, aes(x = gpu_tier, y = price)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Cena u odnosu na GPU Tier",
       x = "GPU Tier", y = "Cena (USD)") +
  theme_minimal()

# ANOVA test za gpu_tier obeležje

anova_gpu = aov(price ~ gpu_tier, data = datav2)
summary(anova_gpu)

# 3) Grafik cene u odnosu na vrste operativnog sistema

ggplot(datav2, aes(x = os, y = price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Cena u odnosu na operativni sistem",
       x = "Operativni sistem", y = "Cena (USD)") +
  theme_minimal()

# 4) Grafik cene u odnosu na veličinu ram memorije

ggplot(datav2, aes(x = ram_gb, y = price)) +
  geom_point() +
  labs(title = "Cena u odnosu na veličinu ram memorije",
       x = "Veličina RAM-a", y = "Cena (USD)") +
  theme_minimal()

# 5) Grafik cene u odnosu na broj jezgara procesora

ggplot(datav2, aes(x = cpu_cores, y = price)) +
  geom_point() +
  labs(title = "Cena u odnosu na broj jezgara procesora",
       x = "Broj jezgara", y = "Cena (USD)") +
  theme_minimal()

# 6) Grafik cene u odnosu na base GHZ

ggplot(datav2, aes(x = cpu_base_ghz, y = price)) +
  geom_point() +
  labs(title = "Cena u odnosu na base GHZ",
       x = "CPU base (GHZ)", y = "Cena (USD)") +
  theme_minimal()

# 7) Grafik cene u odnosu na proizvođača uređaja

ggplot(datav2, aes(x = brand, y = price)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.4) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Cena u odnosu na brend uređaja",
    x = "Brend",
    y = "Cena (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 8) Grafik cene u odnosu na vrstu uređaja

ggplot(datav2, aes(x = device_type, y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Cena laptopova i desktop računara",
       x = "Tip uređaja", y = "Cena (USD)") +
  theme_minimal()

# 9) Uticaj tipa eksterne memorije na količinu memorije i cenu uređaja

ggplot(datav2, aes(x = storage_gb, y = price, color = storage_type)) +
  geom_point(alpha = 1/3) +
  facet_wrap(~ storage_type) +
  theme_minimal() + labs(
    title = "Uticaj tipa eksterne memorije na količinu memorije i cenu uređaja",
    x = "Količina memorije",
    y = "Cena u dolarima"
  )

# 10) Uticaj ranga procesora i ranga grafičke kartice na cenu

ggplot(datav2, aes(x = cpu_tier, y = price, color = gpu_tier)) +
  geom_point(alpha = 1/3) +
  facet_wrap(~ gpu_tier) +
  theme_minimal() + labs(
    title = "Uticaj ranga procesora i ranga grafičke kartice na cenu",
    x = "Rang procesora (tier)",
    y = "Cena u dolarima"
  )

# FEATURE SELECTION

datav3 = datav2

str(datav3)

# uklanjanje modela

datav3$model = NULL

# uklanjanje gpu_modela

datav3$gpu_model = NULL

# uklanjanje form_factora

datav3$form_factor = NULL

# uklanjanje display_type

datav3$display_type = NULL

# uklanjanje display_size

datav3$display_size_in = NULL

# uklanjanje charger_watts

datav3$charger_watts = NULL

# uklanjanje psu_wats

datav3$psu_watts = NULL

# uklanjanje wifi

datav3$wifi = NULL

# uklanjanje bluetooth

datav3$bluetooth = NULL

# uklanjanje težine

datav3$weight_kg = NULL

# provera za garanciju

ggplot(data = datav3) + geom_point(mapping = aes(x = warranty_months, y = price))

# garancija ostaje jer se vidi da se sa porastom garancije smanjuje cena

str(datav3)

# FEATURE ENGINEERING

# 1) Prvi novi feature: cpu_power_score je kombinacija broja jezgara i osnovne frekvencije procesora

datav3$cpu_power_score = datav3$cpu_cores * datav3$cpu_base_ghz

# Distribucija CPU Power Score

ggplot(datav3, aes(x = cpu_power_score)) +
  geom_histogram(bins = 60, fill = "steelblue", alpha = 0.7, color = "black") +
  labs(
    title = "Distribucija CPU Power Score",
    x = "CPU Power Score (cores × GHz)",
    y = "Broj uređaja"
  ) +
  theme_minimal()

# Grafik cene uređaja u odnosu na cpu_power_score

ggplot(datav3, aes(x = cpu_power_score, y = price)) +
  geom_point(alpha = 0.3, color = "darkred") +
  labs(
    title = "Odnos CPU Power Score-a i cene uređaja",
    x = "CPU Power Score",
    y = "Cena (USD)"
  ) +
  theme_minimal()

# Računanje korelacije sa cenom

cor(datav3$cpu_power_score, datav3$price)

# 2) Drugi novi feature: cgt_score je kombinacija dva najbitnija feature-a za performanse uređaja, pošto imamo raspoređene uređaje po rangu za oba ova pojedinačno zanima nas kakav će ishod biti kada se spoje

datav3$cgt_score = as.numeric(datav3$cpu_tier) * as.numeric(datav3$gpu_tier)

# pošto su cpu tier i gpu tier ranije još promenjeni u ordinalne factor promenljive koristimo njihovu nummeričku vrednost kako bismo ih pomnožili

# Distribucija CGT Score

ggplot(datav3, aes(x = cgt_score)) +
  geom_histogram(fill = "steelblue", bins = 40, color = "black", alpha = 0.7) +
  labs(
    title = "Distribucija CGT Score",
    x = "CGT Score",
    y = "Broj uređaja"
  ) +
  theme_minimal()

# Grafik cene uređaja u odnosu na cgt_score

ggplot(datav3, aes(x = cgt_score, y = price)) +
  geom_point(alpha = 0.3, color = "darkred") +
  labs(
    title = "Odnos CGT Score-a i cene uređaja",
    x = "CGT Score",
    y = "Cena (USD)"
  ) +
  theme_minimal()

# Korelacija sa cenom

cor(datav3$cgt_score, datav3$price)

# Treći novi feature: cpu_generation

# Podela procesora po generacijama

datav3 = datav3 %>% mutate(cpu_generation = case_when( 
    str_detect(cpu_model, regex("i9|Ryzen 9|Pro|Max", ignore_case = FALSE)) ~ 4,
    str_detect(cpu_model, regex("i3|Ryzen 3|M1", ignore_case = FALSE)) ~ 1, 
    str_detect(cpu_model, regex("i5|Ryzen 5|M2", ignore_case = FALSE)) ~ 2, 
    str_detect(cpu_model, regex("i7|Ryzen 7|M3", ignore_case = FALSE)) ~ 3, 
    TRUE ~ NA_real_ 
  ))

# Distribucija generacije procesora

ggplot(datav3, aes(x = cpu_generation)) +
  geom_histogram(fill = "steelblue", bins = 4, color = "black", alpha = 0.7) +
  labs(
    title = "Distribucija generacije procesora",
    x = "Generacija procesora",
    y = "Broj uređaja"
  ) +
  theme_minimal()

# Grafik cene uređaja u odnosu na cpu_generation

ggplot(datav3, aes(x = cpu_generation, y = price)) +
  geom_point(alpha = 0.3, color = "darkred") +
  labs(
    title = "Odnos generacije procesora i cene uređaja",
    x = "Generacija procesora",
    y = "Cena (USD)"
  ) +
  theme_minimal()

cor(datav3$cpu_generation, datav3$price) 
# Korelacija sa cenom

# Pretvaranje cpu_generation u factor

datav3$cpu_generation = factor(
  datav3$cpu_generation,
  levels = sort(unique(datav3$cpu_generation)),
  ordered = TRUE
)

# Sa komandom str se može videti kako izgleda finalna struktura našeg skupa podataka.

str(datav3)

# PRIPREMA ZA MODELOVANJE 

datav4 = datav3

# pravimo datav4 što će biti i naš finalni skup podataka

# Logaritamska transformacija

datav4$log_price = log1p(datav4$price)

ggplot(datav4, aes(x = log_price)) +
  geom_histogram(bins = 50, fill = "#1f78b4", color = "black", alpha = 0.7) +
  labs(
    title = "Distribucija log-transformisane cene",
    x = "log(1 + price)",
    y = "Broj uređaja"
  ) +
  theme_minimal()

datav4$log_price

# Train/test podela

set.seed(123)

n = nrow(datav4)

train_index = sample(seq_len(n), size = 0.8 * n)
train_data = datav4[train_index, ]
test_data = datav4[-train_index, ]

nrow(train_data)
# train podaci su 59376 redova

nrow(test_data)
# test podaci su 14845

summary(train_data$log_price)
summary(test_data$log_price)

true_price = test_data$price

# u ovoj promenljivoj čuvamo stvarno cenu uređaja iz testnog skupa

# LINEARNA REGRESIJA

# Pravljenje modela, računanje i ispis metrika

# MODEL 1

model_1 = lm(log_price ~ cpu_tier, data=train_data)

pred_1 = expm1(predict(model_1, test_data))

m1_rmse = sqrt(mean((pred_1 - true_price)^2))
m1_mae  = mean(abs(pred_1 - true_price))

m1_rmse; m1_mae

summary(model_1)

# MODEL 2

model_2 = lm(log_price ~ cpu_tier + gpu_tier, data=train_data)

pred_2 = expm1(predict(model_2, test_data))

m2_rmse = sqrt(mean((pred_2 - true_price)^2))
m2_mae  = mean(abs(pred_2 - true_price))

m2_rmse; m2_mae

summary(model_2)

# MODEL 3

model_3 = lm(log_price ~ cpu_tier + gpu_tier + ram_gb, data=train_data)

pred_3 = expm1(predict(model_3, test_data))

m3_rmse = sqrt(mean((pred_3 - true_price)^2))
m3_mae  = mean(abs(pred_3 - true_price))

m3_rmse; m3_mae

summary(model_3)

# MODEL 4

model_4 = lm(log_price ~ cpu_tier + gpu_tier + ram_gb +
                cpu_power_score, data=train_data)

pred_4 = expm1(predict(model_4, test_data))

m4_rmse = sqrt(mean((pred_4 - true_price)^2))
m4_mae  = mean(abs(pred_4 - true_price))

m4_rmse; m4_mae

summary(model_4)

# MODEL 5

model_5 = lm(log_price ~ cpu_tier + gpu_tier + ram_gb +
                cpu_power_score + cgt_score, data=train_data)

pred_5 = expm1(predict(model_5, test_data))

m5_rmse = sqrt(mean((pred_5 - true_price)^2))
m5_mae  = mean(abs(pred_5 - true_price))

m5_rmse; m5_mae

summary(model_5)

# MODEL 6

model_6 = lm(log_price ~ cpu_tier + gpu_tier + ram_gb +
                cpu_power_score + cgt_score + storage_gb,
              data=train_data)

pred_6 = expm1(predict(model_6, test_data))

m6_rmse = sqrt(mean((pred_6 - true_price)^2))
m6_mae  = mean(abs(pred_6 - true_price))

m6_rmse; m6_mae

summary(model_6)

# MODEL 7

model_7 = lm(log_price ~ cpu_tier + gpu_tier + ram_gb +
                cpu_power_score + cgt_score + storage_gb + brand,
              data=train_data)

pred_7 = expm1(predict(model_7, test_data))

m7_rmse = sqrt(mean((pred_7 - true_price)^2))
m7_mae  = mean(abs(pred_7 - true_price))

m7_rmse; m7_mae

summary(model_7)

# MODEL 8

model_8 = lm(log_price ~ cpu_tier + gpu_tier + ram_gb +
                cpu_power_score + cgt_score + storage_gb +
                brand + os,
              data=train_data)

pred_8 = expm1(predict(model_8, test_data))

m8_rmse = sqrt(mean((pred_8 - true_price)^2))
m8_mae  = mean(abs(pred_8 - true_price))

m8_rmse; m8_mae

summary(model_8)

# MODEL 9

model_9 = lm(log_price ~ cpu_tier + gpu_tier + ram_gb +
                cpu_power_score + cgt_score + storage_gb +
                brand + os + device_type,
              data=train_data)

pred_9 = expm1(predict(model_9, test_data))

m9_rmse = sqrt(mean((pred_9 - true_price)^2))
m9_mae  = mean(abs(pred_9 - true_price))

m9_rmse; m9_mae

summary(model_9)

# MODEL 10

model_10 = lm(log_price ~ cpu_tier + gpu_tier + ram_gb +
                cpu_power_score + cgt_score + storage_gb +
                brand + os + device_type + cpu_generation,
              data=train_data)

pred_10 = expm1(predict(model_10, test_data))

m10_rmse = sqrt(mean((pred_10 - true_price)^2))
m10_mae  = mean(abs(pred_10 - true_price))

m10_rmse; m10_mae

summary(model_10)

# MODEL 11

model_11 = lm(log_price ~ cpu_tier + gpu_tier + ram_gb +
                cpu_power_score + cgt_score + storage_gb +
                brand + os + device_type + cpu_generation + vram_gb,
              data=train_data)

pred_11 = expm1(predict(model_11, test_data))

m11_rmse = sqrt(mean((pred_11 - true_price)^2))
m11_mae  = mean(abs(pred_11 - true_price))

m11_rmse; m11_mae

summary(model_11)

# MODEL 12

model_12 = lm(log_price ~ cpu_tier + gpu_tier + ram_gb +
                cpu_power_score + cgt_score + storage_gb +
                brand + os + device_type + cpu_generation + vram_gb + release_year,
              data=train_data)

pred_12 = expm1(predict(model_12, test_data))

m12_rmse = sqrt(mean((pred_12 - true_price)^2))
m12_mae  = mean(abs(pred_12 - true_price))

m12_rmse; m12_mae

summary(model_12)

# RANDOM FOREST

install.packages("ranger")
library(ranger)

# Treniranje modela

rf_model = ranger(
  log_price ~ cpu_tier + gpu_tier + ram_gb +
    cpu_power_score + cgt_score + storage_gb +
    brand + os + device_type + cpu_generation +
    vram_gb + release_year + battery_wh + warranty_months,
  data = train_data,
  num.trees = 500,               
  mtry = floor(sqrt(13)),        
  min.node.size = 5,             
  sample.fraction = 0.75,        
  importance = "impurity",
  seed = 123
)

# Predikcije na test skupu

rf_pred = expm1(predict(rf_model, test_data)$predictions)
true_price = test_data$price

# Računanje i ispis metrika

rf_rmse = sqrt(mean((rf_pred - true_price)^2))
rf_mae  = mean(abs(rf_pred - true_price))
rf_r2   = 1 - sum((rf_pred - true_price)^2) /
  sum((true_price - mean(true_price))^2)

rf_rmse
rf_mae
rf_r2

# Feature importance

summary(rf_model)

imp = data.frame(
  feature = names(rf_model$variable.importance),
  importance = rf_model$variable.importance
) %>% arrange(desc(importance))

print(imp)

ggplot(imp, aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Feature Importance",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal(base_size = 14)

# XGBOOST

install.packages("xgboost")
library(xgboost)

# Priprema podataka

numeric_cols = names(datav4)[sapply(datav4, is.numeric)]
numeric_cols = setdiff(numeric_cols, c("price", "log_price"))

train_matrix = as.matrix(train_data[, numeric_cols])
test_matrix  = as.matrix(test_data[, numeric_cols])

train_label = train_data$log_price
test_label  = test_data$price    

# Treniranje modela

xgb_model = xgboost( 
  data = train_matrix,
  label = train_label,
  nrounds = 100,
  objective = "reg:squarederror",
  tree_method = "hist",   
  verbose = 0
)

# Predikcija na test skupu

xgb_pred_log = predict(xgb_model, newdata = test_matrix)

xgb_pred = expm1(xgb_pred_log)

true_price = test_data$price

xgb_rmse = sqrt(mean((xgb_pred - true_price)^2))
xgb_mae  = mean(abs(xgb_pred - true_price))
xgb_r2   = 1 - sum((xgb_pred - true_price)^2) /
  sum((true_price - mean(true_price))^2)

xgb_rmse
xgb_mae
xgb_r2

# Feature importance

xgb_imp_raw = xgb.importance(
  model = xgb_model,
  feature_names = colnames(train_matrix)
)

xgb_imp = xgb_imp_raw %>%
  select(feature = Feature, importance = Gain) %>%
  arrange(desc(importance))

print(xgb_imp)

# Grafik

ggplot(xgb_imp, aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "XGBoost Feature Importance (Gain)",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal(base_size = 14)

# LASSO REGRESIJA (L1 Regularizacija)

library(glmnet)

# Treniranje

x = model.matrix(
  log_price ~ cpu_tier + gpu_tier + ram_gb +
    cpu_power_score + cgt_score + storage_gb +
    brand + os + device_type + cpu_generation + vram_gb + release_year,
  data = train_data
)[, -1]

y = train_data$log_price

lasso_model = cv.glmnet(
  x, y,
  alpha = 1,      
  nfolds = 10,
  type.measure = "mse"
)

lasso_model$lambda.min

x_test = model.matrix(
  log_price ~ cpu_tier + gpu_tier + ram_gb +
    cpu_power_score + cgt_score + storage_gb +
    brand + os + device_type + cpu_generation + vram_gb + release_year,
  data = test_data
)[, -1]

# Predikcija

lasso_pred = predict(lasso_model, newx = x_test, s = "lambda.min")
lasso_pred_real = expm1(lasso_pred)

lasso_rmse = sqrt(mean((lasso_pred_real - test_data$price)^2))
lasso_mae  = mean(abs(lasso_pred_real - test_data$price))
lasso_r2   = 1 - sum((lasso_pred_real - test_data$price)^2) /
  sum((test_data$price - mean(test_data$price))^2)

lasso_rmse
lasso_mae
lasso_r2

lasso_coef = coef(lasso_model, s = "lambda.min")

lasso_imp = data.frame(
  feature = rownames(lasso_coef),
  coefficient = as.numeric(lasso_coef)
)

lasso_imp = lasso_imp %>% filter(feature != "(Intercept)")

lasso_imp$importance = abs(lasso_imp$coefficient)

lasso_imp = lasso_imp %>% arrange(desc(importance))

ggplot(lasso_imp, aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "LASSO Feature Importance",
    x = "Feature",
    y = "Importance (koeficijent)"
  ) +
  theme_minimal(base_size = 14)
