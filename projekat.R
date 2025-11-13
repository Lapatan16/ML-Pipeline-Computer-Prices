## komentari sa dve tarabe su samo za nas necemo kaciti na git

# ukljucivanje svih potrebnih biblioteka

library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)
library(corrplot)
library(ggcorrplot)

# ucitavanje skupa podataka i prikaz osnovnih stvari
data = read.csv("computer_prices_all.csv")
data
names(data) ## prikazuje nazive kolona
str(data) ## prikazuje koliko redova i kolona ima u skupu i tip podatka svake promenljive

# analiza raspodele ciljne promenljive
summary(data$price) ## sve osnovne podatke o price srednja vr medijana min max i slicno

# Histogram ciljne promenljive price

# Histogram prikazuje raspodelu ciljne promenljive price. Ovde takođe možemo videti 
# da raspodela nije simetrična, već pozitivno asimetrična. Kao što je malopre 
# spomenuto najveći broj uređaja se nalazi u srednjem cenovnom rangu, manji u nižem, 
# a najmanji broj uređaja u skupljem cenovnom rangu. Ovo je i očekivano jer većina 
# korisnika kupuje upravo uređaje srednjeg ranga. 

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

# Boxplot i potvrđuje ono što smo videli na histogramu i ovde vidimo tačke koje na 
# histogramu nisu bile prikazane, jer ih je jako malo sa visokom cenom i te tačke 
# potencijalno predstavljaju outlier-e. Oni mogu značajno da utiču na to koliko je naš 
# model dobar što će i biti provereno kasnije.
# Pošto linearna regresija pretpostavlja da je raspodela ciljne promenljive približno 
# normalna, a kod nas to nije slučaj i  zbog toga će kasnije biti primenjena logaritamska
# transformacija. Ona približava raspodelu normalnoj tj. čini da histogram ima zvonastu 
# strukturu. Samim tim se smanjuje uticaj skupljih uređaja i model postaje bolji i 
# stabilniji.


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

# Na osnovu boxplot-ova možemo videti da su medijane uglavnom slične kroz godine ili 
# se minimalno povećavaju. Iako bi se trebalo pretpostaviti da će noviji uređaji biti 
# malo skuplji ovo je sasvim u redu. Postoji dosta tačaka koje odskaču od većine, to 
# su sve verovatno profesionalni uređaji sa veoma jakim performansama i skupim 
# komponentama, ali će kasnije biti ispitani. Iako modeli srednjeg cenovnog ranga 
# blago rastu jeftini modeli uglavnom ostaju sličnih cena što znači da u tom delu ne 
# dolazi do nekih većih promena.

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

# Na osnovu boxplot dijagrama možemo videti da postoji razlika u ceni između 
# desktop računara i laptopova, ali nije velika. Medijana cene laptopova je nešto 
# veća što je i očekivano, jer laptopovi imaju integrisane komponente i prenosivi su, 
# pa su skuplji od računara. Postoji dosta cena koje odskaču od većine (srednje klase) 
# i to su verovatno profesionalni računari i gaming laptopovi.

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

# Sa grafika možemo videti da tačke formiraju vertikalne linije što znači da imamo 
# samo određene vrednosti koje mogu biti vrednosti RAM memorije i posle ćemo moći 
# ovu numeričku promenljivu da prebacimo u kategorijsku. Postoje uređaji koji imaju 
# nešto manje od 100GB RAM-a, a dosta su skuplji nego uređaji sa preko 100GB RAM-a, 
# može biti da su ostale komponente dosta skuplje npr. procesor, grafička kartica, 
# eksterna memorija, itd…

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

# Sa grafika se može videti da sa porastom broja jezgara blago raste i cena uređaja što 
# je i logično jer broj jezgara je indikator procesorske moći, ali tako je negde do 12 
# ili 16 jezgara i nakon toga se cena uglavnom smanjuje sa povećanjem broja jezgara što 
# je potrebno ispitati. Možda neke druge komponente imaju uticaj kod uređaja sa manje 
# od 20 jezgara. Takođe postoji nekoliko primera sa manje od 10 jezgara, a cenom preko 
# 8000$ i verovatno je isti razlog kao za ove prethodne, a takođe je moguće da je došlo 
# do nekih grešaka pri unosu, što je potrebno dodatno ispitati.

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

# Sa grafika možemo videti boxplot-ove koji prikazuju cene uređaja u odnosu na klasu 
# procesora koju imaju. 1 je najlošija, 6 je najbolja klasa. Medijana se porastom klase 
# postepeno povećava, što je i logično, što je bolji procesor cena je veća. Za svaku 
# kategoriju postoje potencijalni outlieri tj. cene dosta niže ili više od prosečnih za 
# tu kategoriju, što je u redu , jer su druge komponente najverovatnije jeftinije. 
# Postoje uređaji najviše klase, dosta jeftiniji od uređaja nižih klasa, to može značiti 
# da je neka druga komponenta zaslužna za takvu cenu, a može biti i da su podaci 
# pogrešno uneti.

ggplot(data, aes(x = as.factor(cpu_tier), y = price)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Odnos između CPU Tier-a i cene uređaja",
    x = "CPU Tier (rang procesora)",
    y = "Cena (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# boxplot cene u zavisnosti od ranga grafičke kartice

# Sa grafika možemo videti kakve su cene uređaja u odnosu na grafičku karticu i 
# takođe kao i kod procesora postoji 6 kategorija i još pravilnije sa porastom klase 
# raste i cena. Potencijalnih outliera ima, ali biće ispitani, posebno ovaj uređaj sa 
# cenom od 7500$, a najlošijom klasom grafičke kartice. U globalu ova osobina je dosta 
# bitna za krajnju cenu uređaja.

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

# VRAM memorija jeste u stvari zasebna memorija grafičke kartice, koja se koristi 
# za generisanje slika, koje se prikazuju na ekranu. Više ovakvog ram-a znači i 
# veću cenu, ali nije sasvim pravilno, postoje uređaji sa malo VRAM-a sa izuzetno 
# visokim cenama. Što se tiče potencijalnih outlier-a postoje uređaji sa 0GB VRAM-a, 
# moguće da nemaju uopšte VRAM i da budu poprilično skupi.  Svaki deo ima outlier-e 
# ali su svi u granicama normale.

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

# Standardne vrednosti količine memorije jesu upravo 512GB i 1TB što grafik, 
# sa slike i potvrđuje, većina uređaja se nalazi u ovom opsegu. Svakako je velika 
# razlika u ceni u  zavisnosti od tipa memorije (SSD ili HDD). Nekih nerealnih 
# vrednosti uglavnom nema.

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

# Boxplot-ovi prikazuju zavisnost cene od tipa memorije, HDD je najstarija i najsporija, 
# SSD novija od nje i brža, Hybrid je njihova kombinacija, a NVMe je najbrža i najbolja 
# verzija SSD memorije. Ovo obeležje ne utiče značajno na cenu uređaja, medijane su 
# skoro pa slične, malo je medijana niža kod HDD memorije što je i logično jer je to 
# najsporija i najjeftinija memorija. Postoje uređaji sa velikom cenom, a najslabijom 
# vrstom memorije, ali je verovatno ima dosta, jer su ostale komponente skuplje i jače. 
# Dobar primer je RAM, ako imamo mnogo RAM-a, onda nam brzina eksterne memorije nije 
# presudna, te je bolje uzeti sporiju i jeftiniju eksternu memoriju.

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

# Grafik prikazuje zavisnost cene u odnosu na veličinu ekrana izraženu u inčima. 
# U skupu podataka je više laptopova, jer je većina uređaja između 15 i 17 inča, što 
# je karakteristično za laptopove. Veličina ekrana ima uticaj na cenu, ali ne preteran, 
# uticaj je verovatno veći u kombinaciji sa drugim obeležjima. Postoje laptopovi sa 
# cenama preko 8000$ što je malo čudno, ali veličina ekrana i nije bitan indikator u 
# svetu laptopova i tehnologije, već kakva je unutrašnjost uređaja.

ggplot(data, aes(x = display_size_in, y = price)) +
  geom_point() +
  labs(
    title = "Odnos veličine ekrana i cene uređaja",
    x = "Veličina ekrana (inči)",
    y = "Cena uređaja (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafik cene uređaja u odnosu na rezoluciju

# Sa grafika vidimo da medijana blago raste sve do rezolucije 2880x1800, nakon toga 
# opada i na samom kraju ponovo raste. Kod svake rezolucije postoje jako skupi modeli 
# što je sasvim u redu. Takođe je bitno napomenuti da ima nekoliko uređaja sa izuzetno 
# malom cenom oko 500$, a sa najvećom rezolucijom 3840X2160, što ne prati trend nikako 
# i može predstaviti veliki problem u treniranju modela.

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

# Sa grafika se vidi da najveći broj uređaja ima 60Hz frekvenciju osvežavanja ekrana 
# i cena je uglavnom u srednjem i nižem rangu. Sa povećanjem frekvencije ne raste cena,
# za svaku frekvenciju većina uređaja ima cenu do 4000$, dosta većih vrednosti ima svugde, najviše kod 60 i 120Hz. Nema nekog pravilnog povećanja ili smanjenja, pa ovo obeležje i nije toliko bitno.

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

# Sa grafika vidimo uređaje sa 0Wh baterije i to su desktop računari svi ostali uređaji
# su laptopovi. Sa većim kapacitetom baterije bi cene trebale da rastu , ali to baš i 
# nije linearno malo rastu, malo se smanjuju. Postoje laptopovi sa oko 60Wh baterijom 
# cene preko 7000$ i potrebno je ovakve uređaje ispitati. Takođe treba proveriti 
# uređaje ispod 700$ sa skoro 100Wh.

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

# Sa grafika vidimo da je većina uređaja težine od 1 do 3.5kg i ima ih sa raznim cenama. 
# Uređaji manji od kilogram mogu potencijalno biti outlieri i potrebno ih je dodatno 
# ispitati. Težina uglavnom i nije neko merilo cene, postoji dosta laganih uređaja 
# raznih cena, isto važi i za dosta teže uređaje. 

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

# Sa grafika vidimo da veza cene i brzine procesora i nije bas linearna, postepeno 
# raste do određenog dela, ali ima i dosta uređaja sa velikom brzinom procesora ,a 
# malom cenom, u redu je ako su im npr. druge komponente jeftinije ili može takođe 
# zavisiti od proizvođača procesora.

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

# Cena uređaja u odnosu na brend procesora

# Možemo videti cene uređaja u odnosu na proizvođača procesora. Apple prednjači u 
# odnosu na AMD i Intel što je i očekivano, jer oni obično i dolaze sa skupljim 
# komponentama. Svi imaju dosta skupe uređaje što je potrebno ispitati, posebno 
# uređaje iznad 9 hiljada dolara, kojih je samo nekoliko.

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

# Medijana ChromeOS je najniža što je i očekivano jer je relativno mlad operativni 
# sistem, sa ne toliko širokom upotrebom, a uređaji sa macOS su očekivano najskuplji 
# zbog cene svojih komponenti i svog uticaja na tržištu. Uređaja sa dosta većim 
# cenama ima svugde, najviše sa Windows operativnim sistemom, jer je on najpoznatiji 
# i najkorišćeniji operativni sistem. Izdvaja se uređaj koji ima macOS i cenu ispod 
# 1000$ i uređaj sa ChromeOS i cenom preko 6000$. To su ekstremne vrednosti, koje bi 
# mogle značajno da umanje preciznost modela.

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

# Medijane su uglavnom slične, Razer ima malo veću od ostalih, a Apple dosta ali 
# i očekivano, Apple uređaji su uvek najskuplji. Kod HP, Lenovo i MSI brenda postoje 
# modeli skuplji od 9000$ što je potrebno ispitati i potencijalni outlieri jesu i 
# modeli Dell i Razer marke koji su jeftiniji od 700$ dolara. Ovo je važan prediktor, 
# ali je uglavnom dosta bitniji u kombinaciji sa drugim prediktorima.

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

####
# kombinacije podataka radi boljeg prepoznavanja trendova
###

# Uticaj tipa eksterne memorije na količinu memorije i cenu uređaja

# Na scatter dijagramu imamo odnos količine eksterne memorije u GB i cene u dolarima, 
# podeljene po tipu eksterne memorije. To nam omogućava da vidimo za svaku vrstu eksterne
# memorije koja količina se najčešće uzima i koliko košta u odnosu na ostale vrste. 
# Sa grafika možemo uočiti da nema pravila i da se sve četiri vrste eksterne memorije 
# kupuju u sličnoj meri. Vidimo takođe da je HDD generalno jeftiniji od ostalih.

ggplot(data, aes(x = storage_gb, y = price, color = storage_type)) +
  geom_point(alpha = 1/3) +
  facet_wrap(~ storage_type) +
  theme_minimal() + labs(
    title = "Uticaj tipa eksterne memorije na količinu memorije i cenu uređaja",
    x = "Količina memorije",
    y = "Cena u dolarima"
  )

# Uticaj marke procesora na cenu po rangu

# Na scatter grafiku je prikazan uticaj marke po rangu procesora na cenu. 
# Imamo tri marke procesora i šest nivoa procesora. Odmah na startu vidimo da je 
# Apple najskuplji po svim nivoima. Vidimo da sve marke imaju dosta outliera, gde Intel 
# ima najviše i to najčešće za tier vrednosti od 2 do 4. To nam govori, ono što smo već 
# mogli da naslutimo da je Intel najfleksibilniji procesor, koji se kombinuje sa dosta
# drugih komponenti, koje mogu uticati na cenu uređaja.

ggplot(data, aes(x = cpu_tier, y = price, color = cpu_brand)) +
  geom_point(alpha = 1/3) +
  facet_wrap(~ cpu_brand) +
  theme_minimal() + labs(
    title = "Uticaj marke procesora na cenu po rangu",
    x = "Rang procesora (tier)",
    y = "Cena u dolarima"
  )

# apple drzi konstantu cenu, dok ostali malo odskacu i imaju vise ekstremnih ocena
# sve marke prate trend da je visi rang znaci i visu cenu

# koji brand daje najvise garancija i kako to utice na cenu

# Na scatter dijagramu je prikazan uticaj marke računara i garancije u mesecima 
# koje oni pružaju na cenu uređaja. Sa grafika možemo jasno videti da je Apple 
# najskuplji po svim vrednostima garancije. Takođe, možemo videti da garancija ne 
# utiče toliko na cenu računara, kao što bismo prvo pretpostavili. 
# Kod svih brendova garancija od 40+ meseci ima uglavnom nižu cenu od svih ostalih. 
# Najveća zarada kod svih brendova se postiže sa garancijom od 24 meseca, 
# najverovatnije jer je to najčešća vrednost garancije i samim tim imamo najviše 
# primera za tu vrednost. Možemo videti da kod svake marke računara imamo nekoliko 
# outliera, gde Lenovo ima najekstremniju vrednost za garanciju od 36 meseci ima cenu 
# od 9.000+ dolara

ggplot(data, aes(x = warranty_months, y = price, color = brand)) +
  geom_point(alpha = 1/3) +
  facet_wrap(~ brand) +
  theme_minimal() + labs(
    title = "Uticaj marke računara i garancije u mesecima na cenu",
    x = "Garancija u mesecima",
    y = "Cena u dolarima"
  )

# Uticaj marke i ranga grafičke kartice na cenu

# Na scatter dijagramu imamo prikazan uticaj marke i ranga grafičke kartice na cenu. 
# Rang grafičke kartice nam predstavlja dobar prediktor sam po sebi, ali u kombinaciji 
# sa markom bi mogao da postane još bolji prediktor. Vidimo sa slike, kao i do sad, 
# da je Apple skuplji po svim rangovima od ostalih. AMD, Intel i NVIDIA imaju skoro 
# identične cene po svim rangovima. Sa slike se jasno može primetiti da kod svake 
# marke imamo outliere na skoro sve rangove. NVIDIA ima najviše outliera po svim 
# rangovima i jednu ekstremnu vrednost za rang šest, gde je cena viša od 10.000 dolara.

ggplot(data, aes(x = gpu_tier, y = price, color = gpu_brand)) +
  geom_point(alpha = 1/3) +
  facet_wrap(~ gpu_brand) +
  theme_minimal() + labs(
    title = "Uticaj marke i ranga grafičke kartice na cenu",
    x = "Rang grafičke kartice (tier)",
    y = "Cena u dolarima"
  )

# Uticaj ranga procesora i ranga grafičke kartice na cenu

# Na scatter dijagramu je prikazan odnos ranga procesora i ranga grafičke kartice 
# i njihovog uticaja na cenu. Prvo što primećujemo sa slike je da neke vrednosti 
# procesora za određene vrednosti grafičke kartice ne postoje. To nije greška, 
# to nam u stvari govori o tome kako određene komponente komuniciraju jedna sa 
# drugom i kako se kombinuju. Vidimo da npr. za rang 1 grafičke kartice imamo 
# podatke samo do četvrtog ranga procesora, isto tako možemo videti da za rang 6 
# grafičke kartice imamo samo rangove 5 i 6 procesora. Možemo zaključiti da sa 
# porastom ranga jedne komponente raste i rang druge komponente, a sa obzirom na to 
# da su rangovi obe komponente dobar prediktor, možemo zaključiti da ćemo samo 
# porastom jedne od te dve komponente povećati cenu i bez razmatranja druge komponente. 
# Možemo takođe primetiti da imamo nekoliko outliera, ali ni jedan po X osi, što nam 
# ponovno potvrđuje njihovu međusobnu vezu i kompatibilnost komponenti.

ggplot(data, aes(x = cpu_tier, y = price, color = gpu_tier)) +
  geom_point(alpha = 1/3) +
  facet_wrap(~ gpu_tier) +
  theme_minimal() + labs(
    title = "Uticaj ranga procesora i ranga grafičke kartice na cenu",
    x = "Rang procesora (tier)",
    y = "Cena u dolarima"
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
# sa grafika odnosa cene i os-a (slika broj 41) su nam bili sumnjivi uređaji sa macOS jeftiniji od 1000$ i sada vidimo da su oni uglavnom od nekog drugog proizvođaca što u stvarnosti nije moguće

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
nrow(preskupi_laptopovi)
nrow(preskupi_racunari)
# sa grafika zavisnosti cene od tipa uređaja (slika broj 11) ostavićemo laptopove koji koštaju do 10000$, jer oni sadrže integrisane i skupe komponente, pa i mogu mnogo koštati (oni preko 10k su već nerealni sa bilo kakvim komponentama i brišemo ih) 
# računari su jeftiniji od laptopova tako da do 8000$ je maksimalna granica otprilike koliko mogu da koštaju, pa ćemo sve sa cenom preko 8000$ skloniti

datav2 = datav2 %>% filter(!((device_type == "Desktop" & price > 8000) | (device_type == "Laptop" & price > 10000)))
# obrisali smo 4 ovakva podatka

skupi_slab_gpu = datav2 %>% filter(gpu_tier == 1 & price > 6000)
nrow(skupi_slab_gpu)
# sa grafika zavisnosti cene od ranga grafičke kartice (slika broj 19) postoje uređaji koji koštaju preko 6000$, a najnižeg su nivoa grafičke kartice, što nije moguće, kakve god da su im druge komponente i ovaj podatak se dosta ističe od drugih, pa ćemo ga obrisati

datav2 = datav2 %>% filter(!(gpu_tier == 1 & price > 6000))
# obrisali smo jedan podatak

jeftini_ogromna_rezolucija = datav2 %>% filter(resolution %in% c("3440x1440", "3840x2160") & price < 500)
nrow(jeftini_ogromna_rezolucija)
# sa grafika zavisnosti cene od rezolucije ekrana (slika broj 29) postoje uređaji sa maksimalnom rezolucijom i cenom ispod 500$, što je nemoguće kakve god da su druge komponente, pa ćemo ih obrisati

datav2 = datav2 %>% filter(!(resolution %in% c("3440x1440", "3840x2160") & price < 500))
# obrisana su 2 podatka

prejeftini_macovi = datav2 %>% filter(os == "macOS" & price < 700)
nrow(prejeftini_macovi)
# sa grafika zavisnosti cene od os-a (slika broj 41) postoje uređaji sa macOS ispod 700 što je nerealno jeftino čak i za polovne modele, a ovde pričamo o novima
# rezultat ovog koda će biti 0 takvih podataka, jer je to bio uređaj koji je prethodno uklonjen zbog nečeg drugog, što nam drugi put potvrđuje da ovaj uređaj nije realan da postoji
  
preskupi_chromeos = datav2 %>% filter(os == "ChromeOS" & price > 9000)
nrow(preskupi_chromeos)
# sa grafika zavisnosti cene od os-a (slika broj 41) vidimo da postoje uređaji koji imaju chromeOS koji imaju slabiji uređaji i uopšteno ga nema kod skupljih, rezultat za uređaj sa ovim os-om kakve god da su druge komponente, pa ćemo obrisati
# rezultat ovog koda jeste 0, što nam govori da ovaj podatak i treba izbaciti, što smo ovde i drugi put dokazali, jer su uređaji sklonjeni prilikom nekog od prethodnih čišćenja podataka

# nakon svih čišćenja podataka ostalo je 74221 podataka, odnosno uklonjeno je oko 26% podataka

### Multivarijantni modeli

ggplot(data = datav2) + geom_point(mapping = aes(x = weight_kg, y = price, color = release_year))

ggplot(data = datav2) + geom_point(mapping = aes(x = cpu_tier, y = ram_gb, color = os))
# na osnovu toga koji je operativni sistem mozemo pretpostaviti koliko rama ima i cpu_tier
# vidimo da Windows podrazumeva u vecini slucajeva jaci racunar, dok linux i macOS nisu toliko zahtevni

ggplot(data = datav2) + geom_point(mapping = aes(x = cpu_tier, y = price, color = cpu_brand), position = "jitter")
# sa grafika mozemo videti da su Apple procesori generalno skuplji od ostalih za isti tier
# dok su AMD i Intel podjednake cene

ggplot(data = datav2) + geom_point(mapping = aes(x = storage_gb, y = price, color = device_type), position = "jitter", alpha = 1/5)

cor(datav2[,c(4, 9, 10, 11, 12, 13, 16, 17, 18, 20, 21, 23, 25, 26, 27, 28, 30, 31, 32, 33)])

# EDA

numericke_kolone = datav2 %>% select_if(is.numeric)
names(numericke_kolone)
# izdvajamo samo kolone koje su numeričke, jer samo između njih možemo videti korelaciju, ima ih 20
# korelacija je statistička mera koja opisuje jačinu i smer linearne povezanosti između dve ili više numeričkih varijabli, tačnije da li se promenom jedne varijable menja i druga
# bliže 1 je sve više pozitivna korelacija tj. kako jedna raste i druga raste, kako je bliže -1, kako jedna raste druga opada, negativna korelacija, bliže 0, sve je manja povezanost
## select_if se koristi za selekciju kolona iz uslova koji ce se staviti unutar zagrada, a to je da li je tip podataka u koloni numericki i names ispisuje samo nazive kolona gde jeste

matrica_korelacije = cor(numericke_kolone, use = "complete.obs")
matrica_korelacije
# izračunavanje korelacije između svaka dva numerička obeležja i prikaz rezultata
## coor izracunava kolika je korelacija tj povezanost, a use kaze kako ce se raditi sa NA vrednostima, complete.obs kaze radi samo sa redovima koji nemaju NA, kod nas nigde nema NA tako da se koristi sve

sort(matrica_korelacije[,"price"], decreasing = TRUE)
# ispis matrice koja prikazuje kolika je korelacija svakog numeričkog atributa sa price, sortirano opadajuće, da se odmah vidi koja su obeležja pojedinačno najbolja

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
# iscrtavanje malopre pomenute matrice, samo grafički kako bi se lakše posmatralo
# što je više plavo jača je pozitivna korelacija, što je više crveno to je više negativno korelisano
# donji i gornji trougao su preslikvanje, pa su isti, ali su nacrtana oba kako bi grafik bio lepši i naravno na dijagonali su sve jedinice, svako obeležje ima korelaciju 1 sa samim sobom što je i logično
## sto se tice koda uglv neko bojenje i ostalo je sve pojasnjeno iznad
# jake pozitivne korelacije sa ciljnom promenljivom su gpu_tier, cpu_tier, ram_gb, cpu_cores, cpu threads, cpu_baze_ghz, cpu_boost_ghz i vram_gb
# cena najviše zavisi od hardverskih perfomansi CPU, GPU, RAM i slično i to će biti naši glavni prediktori za budući model
# slabe pozitivne korelacije su sa storage_gb, release_year, refresh_hz, bluetooth, warranty_months i slično. 
# same po sebi ne utiču mnogo, ali sa nečim u kombinaciji ovo se možda može povećati
# negativne korelacije su sa weight_kg, display_size_in, psu_watts i druge. Vrednosti su uglavnom dosta bliže 0, tako da i nema neke velike povezanosti
# snažne međusobne korelacije jesu između cpu_base_ghz i cpu_boost_ghz, cpu_tier sa bilo čim iz cpu dela, gpu_tier i vram i druge, uzimaćemo po našoj proceni bitniju od svake dve kako ne bismo došli do multikolinearnonsti
# kada su dve ili vise promenljive međusobno visoko korelisane kazemo da su multikolinearne

# sve kategorijske promenljive pretvaramo u factor. Factor je poseban tip promenljive koji služi da predstavi kategorijske promenljive, kažemo R-u da ova obeležja nemaju numeričko značenje već su podaci podeljeni po grupama
# umesto da kategorijske promenljive budu string-ovi pretvaraju se u factor kako bi grafici mogli da se pravilno iscrtaju, da se podaci lakse skladište i da bi mogle da se definišu i ordered promenljive
# ordered znači da postoji prirodan redosled i bitno je kojim redom ide koja kategorija

datav2$device_type = as.factor(datav2$device_type)
# imamo samo dve moguće vrednosti Desktop i Laptop pa ovo obeležje pretvaramo u factor
datav2$brand = as.factor(datav2$brand)
# ima nekoliko proizvođača uređaja, kategorijsko je obeležje pa pretvaramo u factor
datav2$os = as.factor(datav2$os)
# takođe ima samo nekoliko vrednosti i kategorijsko je obeležje
datav2$storage_type = as.factor(datav2$storage_type)
datav2$cpu_brand = as.factor(datav2$cpu_brand)
datav2$gpu_brand = as.factor(datav2$gpu_brand)
datav2$display_type = as.factor(datav2$display_type)
datav2$resolution = as.factor(datav2$resolution)
datav2$wifi = as.factor(datav2$wifi)
datav2$form_factor = as.factor(datav2$form_factor)
# slično kao i za sve prethodno
datav2$cpu_tier = factor(
  datav2$cpu_tier,
  levels = sort(unique(datav2$cpu_tier)),
  ordered = TRUE
)
# pretvaramo u ordered obeležje, kategorijsko je i jako je bitan redosled
## ordered + TRUE znači da je ovo ordered factor promenljiva i levels = sort(unique(datav2$cpu_tier)) name kaže 
datav2$gpu_tier = factor(
  datav2$gpu_tier,
  levels = sort(unique(datav2$gpu_tier)),
  ordered = TRUE
)
# isto kao i za cpu_tier
# obeležja poput model, i  ne pretvaramo, jer nisu previše bitni za model i ima hiljade i hiljade različitih kategorija, nema nekih kategorija
# numerička obeležja ostaju onakva kakva i jesu

# nakon iscrtavanja početnih grafika i njihove analize, čišćenja i sređivanja podataka i naravno uz pomoć domenskog znanja u EDA fazi izdvojićemo nekoliko grafika koje smatramo da su najbitniiji

# 1) boxplot cene u zavisnosti od ranga procesora

ggplot(datav2, aes(x = cpu_tier, y = price)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Cena u odnosu na CPU Tier",
       x = "CPU Tier", y = "Cena (USD)") +
  theme_minimal()

# boxplot-ovi nam pokazuju da cena jasno raste sa povećanjem ranga procesora, medijana se postepeno povećava.
# svaki rang takođe može imati i skuplje i jeftinije uređaje, u zavisnosti od drugih komponenti, procesor je svakako jedna od najbitnijih komponenti uređaja, ali i jačina ostalih komponenti može znatno da smanji ili poveća cenu
# kod većih rangova veći je i raspon cena, skuplji uređaji mogu biti raznih vrsta gaming uređaji, premium brendovi i slično, negde je jak procesor i ostale komponente su slabije, negde je obrnuto, tako da i cene koje odskaču su sasvim realne
# cene koje odskaču kod nižih rangova su uglavnom gaming računari sa jakom grafičkom karticom ili sa mnogo memorije ili velikim RAM-om

# 2) boxplot cene u zavisnosti od ranga grafičke kartice

ggplot(datav2, aes(x = gpu_tier, y = price)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Cena u odnosu na GPU Tier",
       x = "GPU Tier", y = "Cena (USD)") +
  theme_minimal()

# boxplot-ovi nam pokazuju da je ovaj prediktor još važniji za cenu, medijane takođe pravilno rastu sa povećanjem ranga procesora
# niži rangovi (1 i 2) uglavnom ne prelaze neki srednji cenovni rang, kao i za prethodno uređaji većih rangovima sa velikim cenama su neke profesionalne radne stanice ili neki jako dobri gaming uređaji
# takođe kod nižih rangova nema uređaja koji su mnogo skupi, jer je gpu uglavnom i najskuplja komponenta uređaja (naravno uz procesor)

# 3) boxplot cene u zavisnosti od vrste operativnog sistema

ggplot(datav2, aes(x = os, y = price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Cena u odnosu na operativni sistem",
       x = "Operativni sistem", y = "Cena (USD)") +
  theme_minimal()

# boxplot-ovi nam pokazuju da uređaji sa mac operativnim sistemom imaju ubedljivo najvišu medijanu i takođe imaju i velik cenovni opseg tako da su njihovi uređaji ubedljivo najskuplji
# uređaji sa windows operativnim sistemom imaju i najveći cenovni opseg, najviše uređaja i koristi ovaj os i postoji gomila uređaja od najjeftinijih do najskupljih
# uređaji sa chrome operativnim sistemo imaju najmanju medijanu i najmanji broj outlier-a i to su uglavnom uređaji sa slabijim i jeftinijim komponentama i služe za obavljanje osnovnih zadataka, obrazovanje i slično
# uređaji sa linux os-om su uglavnom stabilni i nižih cena, većih od chrome os-a, ali ne sa nešto prezahtevnim hardverom
# ovaj prediktor i nije toliko jako povezan sa cenom kao prethodni, ali je dobar pokazatelj kakva je cena u odnosu na neku kategoriju

# 4) boxplot cene u zavisnosti od vrste operativnog sistema

ggplot(datav2, aes(x = storage_type, y = price)) +
  geom_boxplot(fill = "khaki") +
  labs(title = "Cena u odnosu na tip skladišta",
       x = "Tip skladišta", y = "Cena (USD)") +
  theme_minimal()