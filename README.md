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

![bargraph](https://github.com/berkaycayan/Global-Food-Security-Index-Data-Visualization/assets/130244458/1a1aacb5-0485-4dfa-8cd7-2df7300162e8)

Dünya çapında kalite ve güvenlik konularında öne çıkan en üst 20 ülke, bar grafiği ile görselleştirilmiştir. Bu çerçevede, kalite ve güvenlik performansı açısından en önde gelen ülkeler arasında Kanada, Danimarka ve ABD öne çıkmaktadır. Bu ülkeler, uluslararası standartlarda üst düzey kalite yönetimi ve etkili güvenlik politikaları ile dikkat çekmekte, küresel çapta güvenilirlik ve kalite anlamında lider konumda bulunmaktadır

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

![loli](https://github.com/berkaycayan/Global-Food-Security-Index-Data-Visualization/assets/130244458/2756956f-86d6-48c0-8a58-fdd9db04d1c3)

Bu analizde, lolipop grafiği kullanılarak gıdanın uygun olduğu ülkeler sıralanmıştır. Grafikteki 20 ülke arasında en uygun olanlar Japonya, Çin ve Singapur olarak öne çıkarken, en düşük uygunluk seviyesine sahip olanlar ise Suriye, Yemen ve Kamerun'dur. Bu görselleştirme, gıda uygunluğu açısından dünya genelindeki ülkeler arasındaki çeşitliliği vurgulamakta ve bu alandaki performansları öne çıkarmaktadır

![scatterplot](https://github.com/berkaycayan/Global-Food-Security-Index-Data-Visualization/assets/130244458/aaa49088-7a0c-48bc-9d71-72c46719db2f)


Bu grafikte, Scatter plot yöntemi kullanılarak ülkelerin kıtalara göre sürdürülebilirlik ve adaptasyon durumları detaylı bir şekilde incelenmiştir. Bu analizde, en iyi durumda olan kıta açık ara Avrupa olarak öne çıkmaktadır. Sürdürülebilirlik konusunda öncü ülkeler arasında Norveç, Finlandiya ve Yeni Zelanda gibi ülkeler yer alırken, maalesef en düşük sürdürülebilirlik politikasına sahip olan ülkeler arasında Paraguay, Botsvana ve Kamboçya bulunmaktadır. Bu görselleştirme, kıtalara göre sürdürülebilirlik ve adaptasyon konularındaki farklılıkları vurgulayarak, ülkeler arasındaki çeşitliliği anlamamıza yardımcı olmaktadır




