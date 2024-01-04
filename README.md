# "Küresel Gıda Güvenliği Endeksi ve Görselleştirilmesi"

Bu çalışma, Eskişehir Teknik Üniversitesi Veri Görselleştirme dersi çerçevesinde gerçekleştirilmiştir. Çalışmada, dünya genelindeki ülkelerin gıda güvenliği performansını değerlendirmeyi amaçlayan "Global Food Security Index" veri seti kullanılmıştır. Veri seti, farklı ülkelerin gıda güvenliği seviyelerini belirlemek için kullanılan çeşitli göstergeleri içermekte olup, bu göstergeler arasında Uygunluk, Erişilebilirlik, Kalite ve Güvenlik, Sürdürülebilirlik ve Adaptasyon bulunmaktadır. Aynı zamanda, veri seti analiz ve modelleme çalışmaları için geniş bir potansiyel sunmakta ve gıda güvenliği konusundaki küresel eğilimleri anlamak ve değerlendirmek için kullanışlı bir kaynak olarak nitelendirilebilir. 



## Veri Seti

[Bu veri seti](https://impact.economist.com/sustainability/project/food-security-index/), 2022 yılında dünya genelindeki 113 ülkede gıda güvenliği performansını değerlendirmek amacıyla oluşturulmuştur. Değerlendirme, beş ana başlık altında gerçekleştirilmiştir:

#### Uygunluk: 
Tüketicilerin gıda satın alma yeteneğini, fiyat şoklarına karşı duyarlılıklarını ve şoklar meydana geldiğinde tüketicilere destek olacak programların ve politikaların varlığını ölçer.

#### Erişilebilirlik: 
Tarımsal üretimi ve çiftlik kapasitelerini, arz kesintisi riskini, gıda dağıtımını sağlama kapasitesini ve tarımsal üretimi genişletme çabalarını ölçer.

#### Kalite ve Güvenlik:
Ortalama diyetlerin çeşitliliğini ve besin kalitesini, aynı zamanda gıdanın güvenliğini ölçer.

#### Sürdürülebilirlik ve Adaptasyon: 
Bir ülkenin iklim değişikliği etkilerine maruz kalma durumunu değerlendirir; doğal kaynak risklerine duyarlılığını ölçer; ve ülkenin bu risklere nasıl adapte olduğunu değerlendirir.

Çalışmada şu adımlar izlenmiştir:


- 113 ülkenin Sürdürülebilirlik ve Adaptasyon, Kalite ve Güvenlik, Erişilebilirlik, Uygunluk başlıklarındaki değerlerinin ortalaması alınarak Genel Skor oluşturulmuş ve bu Genel Skor, Dünya Haritası Grafiği ile görselleştirilmiştir.
- Kalite ve güvenlik konusunda En iyi 20 ülke ve En kötü 20 ülke bar grafiği ile görselleştirilmiştir
- Ülkeler kıtalarına göre yeni bir değişken olarak tanımlanmış ve kıtalara göre gıda erişilebilirliği kutu grafiği ile görselleştirilmiştir
- Gıdanın uygun oldugu ülkeler Lolipop grafiği ile görselleştirilmiştir
- Sürdürülebilirlik ve Adaptasyon en iyi 10 ve en kötü 10 ülke olarak incelenmişt ve görselleştirilmiştir


# Paketler
```{r, message=FALSE, warning=FALSE}
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("devtools")
install.packages("sf")
install.packages("tidyverse")
install.packages("ggridges")
install.packages("MetBrewer")
install.packages("ggflags", repos = c(
  "https://jimjam-slam.r-universe.dev",
  "https://cloud.r-project.org"))
install.packages("ggflags")
install.packages("countrycode")
install.packages("ggimage")
install.packages("countrycode")
install.packages("ggflags")
install.packages("geodata")
install.packages("patchwork")
install.packages("MetBrewer")
library(MetBrewer)
library(patchwork)
library(countrycode)
library (ggflags)
library (geodata)
library(ggimage)
library(countrycode)
library(ggflags)
library(MetBrewer)
library(ggridges)
library(sf)
library(devtools)
library(gridExtra)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)
```
### Gerekli Filtrelemeler ve isim değişikliği 
```{r}
world_coordinates <- map_data("world")

mismatches <- setdiff(gfsi$Country, world_coordinates$region)
mismatches

gfsi$Country <- gsub("United Kingdom", "UK", gfsi$Country)
gfsi$Country <- gsub("United States", "USA", gfsi$Country)
gfsi$Country <- gsub("Côte d'Ivoire", "Ivory Coast", gfsi$Country)
gfsi$Country <- gsub("Democratic Republic of Congo", "Congo", gfsi$Country)
data2 <- filter(gfsi, `Overall score` > 75.5)
data3 <- filter(gfsi, `Overall score` < 47.7)

datalolipop1 <- filter(gfsi, `Availability` > 71.5)
datalolipop2 <- filter(gfsi, `Availability` < 41.4)
datalolipop <- rbind(datalolipop1, datalolipop2)

data4 <- filter(gfsi, `Availability` > 71.5)
data5 <- filter(gfsi, `Availability` < 41.4)

data6 <- filter(gfsi, `Sustainability and Adaptation` > 69.5)
data7 <- filter(gfsi, `Sustainability and Adaptation` < 38.4)
colnames(data6)[colnames(data6) == "Sustainability and Adaptation"] <- "SustainabilityandAdaptation"
colnames(data7)[colnames(data7) == "Sustainability and Adaptation"] <- "SustainabilityandAdaptation"

data2$iso2 <- countrycode(data2$Country, "country.name", "iso2c")
data2$continent <- countrycode(data2$iso2, "iso2c", "continent")

data2$continent[data2$continent == "Americas"] <- "Amerika"
data2$continent[data2$continent == "Asia"] <- "Asya"
data2$continent[data2$continent == "Europe"] <- "Avrupa"
data2$continent[data2$continent == "Oceania"] <- "Okyanusya"
data2$Country[data2$Country == "Canada"] <- "Kanada"
data2$Country[data2$Country == "Denmark"] <- "Danimarka"
data2$Country[data2$Country == "USA"] <- "Amerika Birleşik Devletleri"
data2$Country[data2$Country == "Finland"] <- "Finlandiya"
data2$Country[data2$Country == "Belgium"] <- "Belçika"
data2$Country[data2$Country == "France"] <- "Fransa"
data2$Country[data2$Country == "Norway"] <- "Norveç"
data2$Country[data2$Country == "Ireland"] <- "İrlanda"
data2$Country[data2$Country == "Sweden"] <- "İsveç"
data2$Country[data2$Country == "Netherlands"] <- "Hollanda"
data2$Country[data2$Country == "Spain"] <- "İspanya"
data2$Country[data2$Country == "Austria"] <- "Avusturya"
data2$Country[data2$Country == "Germany"] <- "Almanya"
data2$Country[data2$Country == "Portugal"] <- "Portekiz"
data2$Country[data2$Country == "Costa Rica"] <- "Kosta Rika"
data2$Country[data2$Country == "Japan"] <- "Japonya"
data2$Country[data2$Country == "Czech Republic"] <- "Çek Cumhuriyeti"
data2$Country[data2$Country == "Switzerland"] <- "İsviçre"
data2$Country[data2$Country == "New Zealand"] <- "Yeni Zelanda"

data3$iso2 <- countrycode(data3$Country, "country.name", "iso2c")
data3$continent <- countrycode(data3$iso2, "iso2c", "continent")
data3$continent[data3$continent == "Americas"] <- "Amerika"
data3$continent[data3$continent == "Asia"] <- "Asya"
data3$continent[data3$continent == "Europe"] <- "Avrupa"
data3$continent[data3$continent == "Oceania"] <- "Okyanusya"
data3$continent[data3$continent == "Africa"] <- "Afrika"
data3$Country[data3$Country == "Ethiopia"] <- "Etiyopya"
data3$Country[data3$Country == "Cameroon"] <- "Kamerun"
data3$Country[data3$Country == "Nigeria"] <- "Nijerya"
data3$Country[data3$Country == "Zambia"] <- "Zambiya"
data3$Country[data3$Country == "Syria"] <- "Suriye"
data3$Country[data3$Country == "Niue"] <- "Niyu"
data3$Country[data3$Country == "Chad"] <- "Çad"
data3$Country[data3$Country == "Ivory Coast"] <- "Fildişi Sahili"
data3$Country[data3$Country == "Congo"] <- "Kongo"
data3$Country[data3$Country == "Sierra Leone"] <- "Sierra Leone"
data3$Country[data3$Country == "Mozambique"] <- "Mozambik"
data3$Country[data3$Country == "Guinea"] <- "Gine"
data3$Country[data3$Country == "Madagascar"] <- "Madagaskar"

data4$Country[data4$Country == "Japan"] <- "Japonya"
data4$Country[data4$Country == "China"] <- "Çin"
data4$Country[data4$Country == "Singapore"] <- "Singapur"
data4$Country[data4$Country == "Portugal"] <- "Portekiz"
data4$Country[data4$Country == "Switzerland"] <- "İsviçre"
data4$Country[data4$Country == "Canada"] <- "Kanada"
data4$Country[data4$Country == "United Arab Emirates"] <- "Birleşik Arap Emirlikleri"
data4$Country[data4$Country == "Costa Rica"] <- "Kosta Rika"
data4$Country[data4$Country == "Qatar"] <- "Katar"
data4$Country[data4$Country == "UK"] <- "Birleşik Krallık"
data5$Country[data5$Country == "Congo"] <- "Kongo"
data5$Country[data5$Country == "Botswana"] <- "Botsvana"
data5$Country[data5$Country == "Chad"] <- "Çad"
data5$Country[data5$Country == "Nigeria"] <- "Nijerya"
data5$Country[data5$Country == "Venezuela"] <- "Venezuela"
data5$Country[data5$Country == "Sierra Leone"] <- "Sierra Leone"
data5$Country[data5$Country == "Cameroon"] <- "Kamerun"
data5$Country[data5$Country == "Yemen"] <- "Yemen"
data5$Country[data5$Country == "Syria"] <- "Suriye"

datalolipop$Country[datalolipop$Country == "Japan"] <- "Japonya"
datalolipop$Country[datalolipop$Country == "China"] <- "Çin"
datalolipop$Country[datalolipop$Country == "Singapore"] <- "Singapur"
datalolipop$Country[datalolipop$Country == "Portugal"] <- "Portekiz"
datalolipop$Country[datalolipop$Country == "Switzerland"] <- "İsviçre"
datalolipop$Country[datalolipop$Country == "Canada"] <- "Kanada"
datalolipop$Country[datalolipop$Country == "United Arab Emirates"] <- "Birleşik Arap Emirlikleri"
datalolipop$Country[datalolipop$Country == "Costa Rica"] <- "Kosta Rika"
datalolipop$Country[datalolipop$Country == "Qatar"] <- "Katar"
datalolipop$Country[datalolipop$Country == "UK"] <- "Birleşik Krallık"
datalolipop$Country[datalolipop$Country == "Congo"] <- "Kongo"
datalolipop$Country[datalolipop$Country == "Botswana"] <- "Botsvana"
datalolipop$Country[datalolipop$Country == "Chad"] <- "Çad"
datalolipop$Country[datalolipop$Country == "Nigeria"] <- "Nijerya"
datalolipop$Country[datalolipop$Country == "Venezuela"] <- "Venezuela"
datalolipop$Country[datalolipop$Country == "Sierra Leone"] <- "Sierra Leone"
datalolipop$Country[datalolipop$Country == "Cameroon"] <- "Kamerun"
datalolipop$Country[datalolipop$Country == "Yemen"] <- "Yemen"
datalolipop$Country[datalolipop$Country == "Syria"] <- "Suriye"

data6$iso2 <- countrycode(data6$Country, "country.name", "iso2c")
data6$continent <- countrycode(data6$iso2, "iso2c", "continent")
data7$iso2 <- countrycode(data7$Country, "country.name", "iso2c")
data7$continent <- countrycode(data7$iso2, "iso2c", "continent")

data7$continent[data7$continent == "Americas"] <- "Amerika"
data7$continent[data7$continent == "Asia"] <- "Asya"
data7$continent[data7$continent == "Europe"] <- "Avrupa"
data7$continent[data7$continent == "Oceania"] <- "Okyanusya"
data7$continent[data7$continent == "Africa"] <- "Afrika"

data6$continent[data6$continent == "Americas"] <- "Amerika"
data6$continent[data6$continent == "Asia"] <- "Asya"
data6$continent[data6$continent == "Europe"] <- "Avrupa"
data6$continent[data6$continent == "Oceania"] <- "Okyanusya"
data6$continent[data6$continent == "Africa"] <- "Afrika"
data6$Country[data6$Country == "Norway"] <- "Norveç"
data6$Country[data6$Country == "Finland"] <- "Finlandiya"
data6$Country[data6$Country == "New Zealand"] <- "Yeni Zelanda"
data6$Country[data6$Country == "Ireland"] <- "İrlanda"
data6$Country[data6$Country == "Costa Rica"] <- "Kosta Rika"
data6$Country[data6$Country == "United Kingdom"] <- "Birleşik Krallık"
data6$Country[data6$Country == "UK"] <- "Birleşik Krallık"
data6$Country[data6$Country == "Germany"] <- "Almanya"
data6$Country[data6$Country == "France"] <- "Fransa"
data6$Country[data6$Country == "Czech Republic"] <- "Çek Cumhuriyeti"
data6$Country[data6$Country == "Austria"] <- "Avusturya"
data7$Country[data7$Country == "El Salvador"] <- "El Salvador"
data7$Country[data7$Country == "Pakistan"] <- "Pakistan"
data7$Country[data7$Country == "Serbia"] <- "Sırbistan"
data7$Country[data7$Country == "Chad"] <- "Çad"
data7$Country[data7$Country == "Sudan"] <- "Sudan"
data7$Country[data7$Country == "Haiti "] <- "Haiti"
data7$Country[data7$Country == "Cambodia"] <- "Kamboçya"
data7$Country[data7$Country == "Botswana"] <- "Botsvana"
data7$Country[data7$Country == "Paraguay"] <- "Paraguay"

```

## Grafikler

### Dünya Haritası Grafiği
```
gfsi$score_category <- cut(gfsi$`Overall score`, breaks = c(30, 40, 50, 60, 70, 80, 90, 100),
                           labels = c("30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90+"),
                           include.lowest = TRUE)

# Renk skalası tanımları
renk_skalasi <- scale_fill_manual(
  name = "Genel Skor",
  values = c("30-40" = "#D4D66C", "40-50" = "#8EBF64", "50-60" = "#6EC0BC", 
             "60-70" = "#5597C2", "70-80" = "#566D98", "80-90" = "#275789", "90+" = "#244994")
)

# Harita oluşturma
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(x = long, y = lat, map_id = region),
    color = "white", size = 0.5
  ) +
  
  geom_map(
    data = gfsi,
    map = world_coordinates,
    aes(fill = score_category, map_id = Country), 
    color = "white", size = 0.5
  ) +
  
  coord_fixed(ratio = 1.5) +
  
  renk_skalasi +  # Önce tanımlanan renk skalasını ekleyin
  
  labs(title = "Dünya Haritasi", fill = "Genel Skor", x = "", y = "") +
  
  theme_classic() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )



```
![dunyaharitasi](https://github.com/berkaycayan/Global-Food-Security-Index-Data-Visualization/assets/130244458/0c4fbe29-5eda-4650-80cd-4ba4330f0611)

Bu grafikte, kalite, güvenlik, sürdürülebilirlik, erişilebilirlik ve uygun fiyat kriterlerinde 113 ülkenin performansını değerlendirmek üzere genel skorlar oluşturulmuştur. Bu genel skorlar, her bir kriterin ortalaması alınarak elde edilmiştir. Bu kapsamlı analiz, ülkelerin genel sıralamasını belirlemede önemli bir ölçüt sunmaktadır 
### Bar Grafikleri
```{r}
gfsi <- data2 %>% 
  ggplot(aes(x = reorder(Country, qualityandsafety), y = qualityandsafety, fill = continent)) + 
  geom_flag(y = -3, aes(image = iso2)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
 
  labs(
    title = "Ülke ve Kıta Bazında Kalite ve Güvenlik",
    x = "Ülkeler",
    y = "Kalite ve Güvenlik",
    fill = "Kıta"
  )

my_palette <- c("Amerika" = "#E57EA7", "Asya" = "#7E8AC2", "Avrupa" = "#F8AA2C", "Okyanusya" = "#8EBE64")


gfsi + scale_fill_manual(values = my_palette) +
  coord_flip() +
  expand_limits(y = -3) +
  theme_classic()
```

![bargraph](https://github.com/berkaycayan/Global-Food-Security-Index-Data-Visualization/assets/130244458/1a1aacb5-0485-4dfa-8cd7-2df7300162e8)

Dünya çapında kalite ve güvenlik konularında öne çıkan en üst 20 ülke, bar grafiği ile görselleştirilmiştir. Bu çerçevede, kalite ve güvenlik performansı açısından en önde gelen ülkeler arasında Kanada, Danimarka ve ABD öne çıkmaktadır. Bu ülkeler, uluslararası standartlarda üst düzey kalite yönetimi ve etkili güvenlik politikaları ile dikkat çekmekte, küresel çapta güvenilirlik ve kalite anlamında lider konumda bulunmaktadır
```{r}

gfsi <- data3 %>% 
  ggplot(aes(x = reorder(Country, qualityandsafety), y = qualityandsafety, fill = continent)) + 
  geom_flag(y = -3, aes(image = iso2)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
 
  labs(
    title = "Ülke ve Kıta Bazında Kalite ve Güvenlik",
    x = "Ülkeler",
    y = "Kalite ve Güvenlik",
    fill = "Kıta"
  )

my_palette <- c("Amerika" = "#E57EA7", "Asya" = "#7E8AC2", "Avrupa" = "#F8AA2C", "Okyanusya" = "#8EBE64", "Afrika" = "#0084ff")

gfsi + scale_fill_manual(values = my_palette) +
  coord_flip() +
  expand_limits(y = -3) +
  theme_classic()

```
![bargraph2](https://github.com/berkaycayan/Global-Food-Security-Index-Data-Visualization/assets/130244458/9e13fb33-803b-4572-b1f8-786f040b8e81)

Dünya genelinde kalite ve güvenlik konularında zayıf performans gösteren en kötü 20 ülke, ilgi çekici grafiklerle ele alınmıştır. Bu bağlamda, kalite ve güvenlik başlıklarında en düşük performans sergileyen ülkeler sıralamasında öne çıkanlar Madagaskar, Haiti ve Gine olmuştur. Bu ülkeler, uluslararası standartlarda daha fazla çaba harcama gerekliliği olan alanlarda, kalite ve güvenlik konularında gelişim imkanlarına odaklanmak zorundadır

### Kutu Grafiği
```{r}
gfsi$iso2 <- countrycode(gfsi$Country, "country.name", "iso2c")
gfsi$continent <- countrycode(gfsi$iso2, "iso2c", "continent")

gfsi$continent[gfsi$continent == "Americas"] <- "Amerika (21)"
gfsi$continent[gfsi$continent == "Asia"] <- "Asya (32)"
gfsi$continent[gfsi$continent == "Europe"] <- "Avrupa (26)"
gfsi$continent[gfsi$continent == "Oceania"] <- "Okyanusya (2)"
gfsi$continent[gfsi$continent == "Africa"] <- "Afrika (32)"


ggplot(gfsi, aes(x = Affordability, y = continent, fill = continent)) +
  geom_boxplot(lwd = 1, col = "black") + 
  labs(title = "Kıtalara Göre Kutu Grafiği", 
       y = "Kıta", x = "Gıda Satın Alma Gücü") +
  scale_fill_manual(values = rep("orange", length(unique(gfsi$continent)))) +  
  theme_classic() +
  facet_wrap(~continent, scales = "free", ncol = 1) +
  theme(legend.position = "none", strip.text = element_blank())
```  

![gıdasatın](https://github.com/berkaycayan/Global-Food-Security-Index-Data-Visualization/assets/130244458/d5050e3a-ead8-4537-8205-67a2651a46c5)


Bu grafikte, kıtalara göre ülkelerin gıda satın alma güçleri kıyaslanmıştır. Kıta isimleri yanında parantez içinde bulunan sayılar, her kıtada kaç adet ülkenin yer aldığını göstermektedir. En yüksek ortalama gıda satın alma gücüne sahip kıtalar arasında Okyanusya ve Avrupa öne çıkmaktadır. Bu durum, bu kıtalardaki ülkelerin ekonomik güç ve gıda satın alma gücü açısından öne çıktığını göstermektedir

### Lolipop Grafiği
```{r}

color_palette1 <- c("#E41A1C", "#377EB8")

ggplot(datalolipop, aes(x = reorder(Country, Availability), y = Availability)) +
  geom_segment(aes(x = reorder(Country, Availability), xend = reorder(Country, Availability), y = 0, yend = Availability), color = color_palette1[1]) +
  geom_point(color = color_palette1[2], size = 4, alpha = 0.6) +
  global_food_theme +  # Global food theme'i uygula
  coord_flip() +
  labs(x = "Ülkeler", y = "Erişebilirlik",
       title = "Ülke Bazında Erişebilirlik") +
  ylim(0, 100) +  
  scale_color_manual(values = color_palette1)  # 
theme_classic()
```
![loli](https://github.com/berkaycayan/Global-Food-Security-Index-Data-Visualization/assets/130244458/2756956f-86d6-48c0-8a58-fdd9db04d1c3)

Bu analizde, lolipop grafiği kullanılarak gıdanın uygun olduğu ülkeler sıralanmıştır. Grafikteki 20 ülke arasında en uygun olanlar Japonya, Çin ve Singapur olarak öne çıkarken, en düşük uygunluk seviyesine sahip olanlar ise Suriye, Yemen ve Kamerun'dur. Bu görselleştirme, gıda uygunluğu açısından dünya genelindeki ülkeler arasındaki çeşitliliği vurgulamakta ve bu alandaki performansları öne çıkarmaktadır

### Saçılım Grafiği

```{r}
scatter_plot <- ggplot(data6, aes(x = SustainabilityandAdaptation, y = reorder(Country, SustainabilityandAdaptation), color = continent)) +
  geom_point(size = 4, alpha = 1) +  # alpha parametresini 1'e çıkardık (tam opak)
  labs(title = "Sürdürülebilirlik ve Adaptasyon Scatter Plot",  x = "", y = "", color = "Kıta") +
  scale_color_manual(values = c("Amerika" = "#E57EA7", "Asya" = "#7E8AC2", "Avrupa" = "#F8AA2C", "Okyanusya" = "#8EBE64", "Afrika" = "#0084ff")) +
  theme_minimal() +
  scale_x_continuous(limits = c(30, 90))

scatter_plot2 <- ggplot(data7, aes(x = SustainabilityandAdaptation, y = reorder(Country, SustainabilityandAdaptation), color = continent)) +
  geom_point(size = 4, alpha = 1) +  # alpha parametresini 1'e çıkardık (tam opak)
  labs(title = "", x = "Sürdürülebilirlik ve Adaptasyon", y = "Ülkeler", color = "Kıta") +
  scale_color_manual(values = c("Amerika" = "#E57EA7", "Asya" = "#7E8AC2", "Avrupa" = "#F8AA2C", "Okyanusya" = "#8EBE64", "Afrika" = "#0084ff")) +
  theme_minimal() +
  scale_x_continuous(limits = c(30, 90))

combined_plot <- scatter_plot / scatter_plot2 + plot_layout(ncol = 1)

print(combined_plot)

```

![sacatter](https://github.com/berkaycayan/Global-Food-Security-Index-Data-Visualization/assets/130244458/438a3648-da29-43d3-902c-355e87a19c4b)



Bu grafikte, Scatter plot yöntemi kullanılarak ülkelerin kıtalara göre sürdürülebilirlik ve adaptasyon durumları detaylı bir şekilde incelenmiştir. Bu analizde, en iyi durumda olan kıta açık ara Avrupa olarak öne çıkmaktadır. Sürdürülebilirlik konusunda öncü ülkeler arasında Norveç, Finlandiya ve Yeni Zelanda gibi ülkeler yer alırken, maalesef en düşük sürdürülebilirlik politikasına sahip olan ülkeler arasında Paraguay, Botsvana ve Kamboçya bulunmaktadır. Bu görselleştirme, kıtalara göre sürdürülebilirlik ve adaptasyon konularındaki farklılıkları vurgulayarak, ülkeler arasındaki çeşitliliği anlamamıza yardımcı olmaktadır




