
library(haven)
load_savfile <- read_spss("roses.sav")
saveascsv <- write.csv(load_savfile, file = "roses.csv", row.names = FALSE)
load_csvfile <-read.csv("roses.csv")
roses <- as.data.frame((load_csvfile))



library(dplyr)
library(stringi)

roses1 <- roses %>%
  select(Q18_1)

roses1 <- as.data.frame(lapply(roses1, tolower))


zawody1 <- c("informatyk", "lekarz", "psycholog", "weterynarz", "architek", "mechanik", "programist", "pi³karz",
             "naukow", "prawnik", "fotograf", "kuchar", "grafik", "t³umacz", "elektryk", "nauczyciel", "kosmety", "projektant",
             "stra¿ak", "stewardess", "aktor", "dietety", "polic", "fryzjer", "kierowc", "¿o³nie", "rolnik",
             "sportow", "piosenka", "cukiernik", "chirurg", "stolar", "muzyk", "sprzedawca", "dziennika", "pisar", "biolog", 
             "makija¿yst", "tance", "archeolog", "fizjoterapeut", "astronaut", "prezydent", "trener", "opiekun", "wiza¿", "szef",
             "farmaceut", "przewodnik", "ratownik", "chemik", "pilot", "kosmetolog", "in¿ynier", "artyst", "astrofizyk",
             "detektyw", "okulista", "bokser", "szachis", "polityk", "kardiolog", "logistyk", "tapicer", "mechatronik",
             "przedszkola", "stomatolog", "dentyst", "ogrodni", "murar", "œlusar", "bank")

zawody2 <- c("informatyk", "lekarz", "psycholog", "weterynarz", "architekt", "mechanik", "programista", "pi³karz",
             "naukowiec", "prawnik", "fotograf", "kucharz", "grafik", "t³umacz", "elektryk", "nauczyciel", "makija¿ysta", "projektant",
             "stra¿ak", "stewardessa", "aktor", "dietetyk", "policjant", "fryzjer", "kierowca", "¿o³nierz", "rolnik",
             "sportowiec", "piosenkarz", "cukiernik", "chirurg", "stolarz", "muzyk", "sprzedawca", "dziennikarz", "pisarz", "biolog", 
             "makija¿ysta", "tancerz", "archeolog", "fizjoterapeuta", "astronauta", "prezydent", "trener", "opiekun", "wiza¿ysta", "szef",
             "farmaceuta", "przewodnik", "ratownik", "chemik", "pilot", "kosmetolog", "in¿ynier", "artysta", "astrofizyk", 
             "detektyw", "okulista", "bokser", "szachista", "polityk", "kardiolog", "logistyk", "tapicer", "mechatronik",
             "przedszkolanka", "dentysta", "dentysta", "ogrodnik", "murarz", "œlusarz", "bankier")


hmm <- rep(NA, length(roses1$Q18_1))


for (i in 1:length(zawody1)){
  for (j in which(!is.na(hmm) & stri_detect_fixed(roses1$Q18_1, zawody1[i]))){
    hmm[j] <- paste(hmm[j], zawody2[i], sep = ", ")
  }
  
  hmm[stri_detect_fixed(roses1$Q18_1, zawody1[i]) & is.na(hmm)] <- zawody2[i]
}



#dodatkowa kolumna na ³adnie napisane zawodt
roses$hmm <- hmm

#Patrzê w jakich miejscach komputer nie umia³ przypisaæ zawodu
rameczka <- roses %>% 
  select(Q18_1 | hmm) %>% 
  filter(is.na(hmm) == TRUE) %>% 
  filter(Q18_1 != "") %>% 
  filter(stri_detect_fixed(Q18_1, "nie") == FALSE) %>% 
  filter(stri_detect_fixed(Q18_1, "Nie") == FALSE) %>% 
  filter(stri_detect_fixed(Q18_1, "NIE") == FALSE) 




#uzupe³niam rêcznie
roses$hmm[roses$Q18_1 == " Fryzierka"] <- "fryzjer"
roses$hmm[roses$Q18_1 == "Copywriterem"] <- "copywriter"
roses$hmm[roses$Q18_1 == "zatrudniona w zawodzie, gdzie potrzebna jest umiejêtnoœæ rysowania na ró¿nych urz¹dzeniach i normalnie na p³ótnie, kartkach"] <- "artysta"
roses$hmm[roses$Q18_1 == "coœ zwi¹zane z koñmi"] <- "koniara"
roses$hmm[roses$Q18_1 == "chcia³abym otworzyæ w³asny sklep, bzines"] <- "szef, sprzedawca"
roses$hmm[roses$Q18_1 == "chcialbym za³orzyc firme paleciarsk¹ albo ogulnobudowlan¹"] <- "szef, budowlaniec"
roses$hmm[roses$Q18_1 == "Budowlanka"] <- "budowlaniec"
roses$hmm[roses$Q18_1 == "Konstruktor"] <- "budowlaniec, in¿ynier"
roses$hmm[roses$Q18_1 == "Pracowaæ w biurze"] <- "pracownik biurowy"
roses$hmm[roses$Q18_1 == "pawel jumper"] <- "pawe³ jumper"
roses$hmm[roses$Q18_1 == "Infortmatykiem"] <- "informatyk"
roses$hmm[roses$Q18_1 == "Badacz kosmosu"] <- "astrofizyk"
roses$hmm[roses$Q18_1 == "Chcia³bym byæ niani¹"] <- "opiekun"
roses$hmm[roses$Q18_1 == "Technik Pojazdów Samochodowych"] <- "mechanik"
roses$hmm[roses$Q18_1 == "osob¹ zajmuj¹c¹ siê komputerami"] <- "informatyk"
roses$hmm[roses$Q18_1 == "Krawcow¹"] <- "rzemieœlnik"
roses$hmm[roses$Q18_1 == "biznesmen"] <- "szef"
roses$hmm[roses$Q18_1 == "Ja chcia³bym byæ zawodnikiem zjazdowym."] <- "sportowiec"
roses$hmm[roses$Q18_1 == "Patomorfolog/plastyk"] <- "lekarz, artysta"
roses$hmm[roses$Q18_1 == "Robotnika"] <- "robotnik"
roses$hmm[roses$Q18_1 == "Laborantem"] <- "chemik"
roses$hmm[roses$Q18_1 == "Chcia³bym Mieæ firmê Budowlan¹"] <- "szef, budowlaniec"
roses$hmm[roses$Q18_1 == "je¿dziæ zawodowo na eowerze"] <- "sportowiec"
roses$hmm[roses$Q18_1 == "operatorem koparko-³adowarki"] <- "budowlaniec"
roses$hmm[roses$Q18_1 == "trendwatcher"] <- "socjolog"
roses$hmm[roses$Q18_1 == "Menagerem lub kierownikem jakiejœ firmy"] <- "szef"
roses$hmm[roses$Q18_1 == "coœ zwi¹zane z biurem"] <- "pracownik biurowy"
roses$hmm[roses$Q18_1 == "odkrywca technologii"] <- "in¿ynier"
roses$hmm[roses$Q18_1 == "sprzedawc¹ ubrañ"] <- "sprzedawca"
roses$hmm[roses$Q18_1 == "ginekolog"] <- "lekarz"
roses$hmm[roses$Q18_1 == "podologiem"] <- "lekarz"
roses$hmm[roses$Q18_1 == "technikiem"] <- "in¿ynier"
roses$hmm[roses$Q18_1 == "kelnerem/sprzedawc¹"] <- "kelner, sprzedawca"
roses$hmm[roses$Q18_1 == "spedytorem"] <- "logistyk"
roses$hmm[roses$Q18_1 == "dekoratorem wnêtrz"] <- "artysta"
roses$hmm[roses$Q18_1 == "florystka"] <- "biolog"
roses$hmm[roses$Q18_1 == "Badaczem bananów"] <- "biolog"
roses$hmm[roses$Q18_1 == "Chcia³abym mieæ pracê zwi¹zan¹ z jêzykami obcymi."] <- "t³umacz"
roses$hmm[roses$Q18_1 == "Poicjantka"] <- "policjant"
roses$hmm[roses$Q18_1 == "popularnym radc¹ prawnym"] <- "prawnik"
roses$hmm[roses$Q18_1 == "Weterynaria"] <- "weterynarz"
roses$hmm[roses$Q18_1 == "Rolnictwo"] <- "rolnik"
roses$hmm[roses$Q18_1 == "Mój zawód musia³by byæ powi¹zany z matematyk¹ i rysowaniem."] <- "architekt"
roses$hmm[roses$Q18_1 == "Maryna¿em"] <- "marynarz"
roses$hmm[roses$Q18_1 == "prezesem firmy"] <- "szef"
roses$hmm[roses$Q18_1 == "pracowaæ na maszynach numerycznych"] <- "informatyk"
roses$hmm[roses$Q18_1 == "okulist¹"] <- "lekarz"
roses$hmm[roses$Q18_1 == "bizneswoman"] <- "szef"
roses$hmm[roses$Q18_1 == "pracowaæ w jakiœ wiêkszych korporacjach"] <- "pracownik biurowy"
roses$hmm[roses$Q18_1 == "osob¹ która jest w stycznoœci z natur¹"] <- "biolog"
roses$hmm[roses$Q18_1 == "weteryniarzem"] <- "weterynarz"
roses$hmm[roses$Q18_1 == "strazak"] <- "stra¿ak"
roses$hmm[roses$Q18_1 == "poliglot¹"] <- "t³umacz"
roses$hmm[roses$Q18_1 == "jeszcze do koñca nie wiem kim ale coœ zwi¹zanego ze sportem"] <- "sportowiec"
roses$hmm[roses$Q18_1 == "animatorem"] <- "animator"
roses$hmm[roses$Q18_1 == "Instruktor jazdy konnej"] <- "koniara"
roses$hmm[roses$Q18_1 == "Groomer"] <- "fryzjer"
roses$hmm[roses$Q18_1 == "tlumacz"] <- "t³umacz"
roses$hmm[roses$Q18_1 == "Sterownikiem robotów"] <- "in¿ynier"
roses$hmm[roses$Q18_1 == "Zawodniczk¹ siatkówki i zajmowaæ siê dzietetyk¹ lub pracowaæ na wydziale kryminologicznym"] <- "sportowiec, dietetyk, detektyw"
roses$hmm[roses$Q18_1 == "w³aœciciel firmy g³ównie z samochodami"] <- "szef"
roses$hmm[roses$Q18_1 == "œledczym"] <- "detektyw"
roses$hmm[roses$Q18_1 == "Fryzierem"] <- "fryzjer"
roses$hmm[roses$Q18_1 == "Rzemieœlnik"] <- "rzemieœlnik"
roses$hmm[roses$Q18_1 == "Technik logostyk"] <- "logistyk"
roses$hmm[roses$Q18_1 == "kafelkarz"] <- "budowlaniec"
roses$hmm[roses$Q18_1 == "Perkusista metalowy"] <- "muzyk"
roses$hmm[roses$Q18_1 == "marynarzem"] <- "marynarz"
roses$hmm[roses$Q18_1 == "E-sport"] <- "sportowiec"
roses$hmm[roses$Q18_1 == "od dzieciñstwa marzê o staniu na scenie"] <- "aktor"
roses$hmm[roses$Q18_1 == "chcia³abym wykonywaæ zawód przeczkolankê"] <- "przedszkolanka"
roses$hmm[roses$Q18_1 == "byæ austrona³t¹"] <- "astronauta"
roses$hmm[roses$Q18_1 == "Sêdzia pi³karski"] <- "sêdzia"
roses$hmm[roses$Q18_1 == "psychiatra/patolog s¹dowy/prokurator"] <- "psycholog, prawnik"
roses$hmm[roses$Q18_1 == "Tokarz"] <- "rzemieœlnik"
roses$hmm[roses$Q18_1 == "genetykiem"] <- "biolog"
roses$hmm[roses$Q18_1 == "fryzjrk¹"] <- "fryzjer"
roses$hmm[roses$Q18_1 == "architrkt"] <- "architekt"
roses$hmm[roses$Q18_1 == "Pilkarz"] <- "pi³karz"
roses$hmm[roses$Q18_1 == "asystentk¹ w biurze"] <- "pracownik biurowy"
roses$hmm[roses$Q18_1 == "Weterzynarzem"] <- "weterynarz"
roses$hmm[roses$Q18_1 == "Pracownikiem w biurze"] <- "pracownik biurowy"
roses$hmm[roses$Q18_1 == "Leœniczym"] <- "leœniczy"
roses$hmm[roses$Q18_1 == "Wychowawc¹ grupy przedszkolnej"] <- "przedszkolanka"
roses$hmm[roses$Q18_1 == "Raperem"] <- "piosenkarz"
roses$hmm[roses$Q18_1 == "cz³onkiem astrofizycznej grupy badawczej lub w jakiœ inny sposób byæ zwi¹zan¹ z badaniem wrzechœwiata"] <- "astrofizyk"
roses$hmm[roses$Q18_1 == "miechanik samochodowy"] <- "mechanik"
roses$hmm[roses$Q18_1 == "Pracowaæ w hotelu"] <- "hotelarz"
roses$hmm[roses$Q18_1 == "Wuefista"] <- "nauczyciel"
roses$hmm[roses$Q18_1 == "Ochroniarzem SOP"] <- "policjant"
roses$hmm[roses$Q18_1 == "Instruktork¹ tañca lub po prostu tañcerk¹"] <- "tancerz"
roses$hmm[roses$Q18_1 == "Rolnictwo/Górnik"] <- "rolnik, górnik"
roses$hmm[roses$Q18_1 == "Budowlaniec"] <- "budowlaniec"
roses$hmm[roses$Q18_1 == "Raper i tyke"] <- "piosenkarz"
roses$hmm[roses$Q18_1 == "zoolog"] <- "biolog"
roses$hmm[roses$Q18_1 == "Malarka"] <- "malarz"
roses$hmm[roses$Q18_1 == "zwi¹zany z technologi¹ lub nauk¹"] <- "in¿ynier"
roses$hmm[roses$Q18_1 == "Automatyk"] <- "in¿ynier"
roses$hmm[roses$Q18_1 == "Chcia³abym mieæ swoj¹ firmê lub byæ jakimœ managerem dobrej firmy/hotelu"] <- "szef, hotelarz"
roses$hmm[roses$Q18_1 == "Deweloperem"] <- "deweloper"
roses$hmm[roses$Q18_1 == "ekonomista"] <- "ekonomista"
roses$hmm[roses$Q18_1 == "Cyberbezpieczeñstwo"] <- "programista"
roses$hmm[roses$Q18_1 == "leka¿em"] <- "lekarz"
roses$hmm[roses$Q18_1 == "Zostaæ w³aœcicielem firmy wykorzystuj¹cej nowe technologie."] <- "szef"
roses$hmm[roses$Q18_1 == "fizyk j¹drowy/subatomowy"] <- "fizyk"
roses$hmm[roses$Q18_1 == "Instruktorka jazdy konnej"] <- "koniara"
roses$hmm[roses$Q18_1 == "W³aœciciel du¿ej firmy zajmuj¹cej siê e-commerce"] <- "szef"
roses$hmm[roses$Q18_1 == "Ekonomik/ksiêgowa"] <- "ekonomista, ksiêgowy"
roses$hmm[roses$Q18_1 == "byæ Rybakiem"] <- "rybak"
roses$hmm[roses$Q18_1 == "Fryzier"] <- "fryzjer"
roses$hmm[roses$Q18_1 == "geodetom"] <- "geodeta"
roses$hmm[roses$Q18_1 == "Hydraulika"] <- "hydraulik"
roses$hmm[roses$Q18_1 == "Grabiarzem"] <- "grabarz"
roses$hmm[roses$Q18_1 == "W przysz³oœci chcia³abym zostaæ dermatologiem."] <- "lekarz"
roses$hmm[roses$Q18_1 == "Lektorka"] <- "lektor"
roses$hmm[roses$Q18_1 == "Masa¿"] <- "masa¿ysta"
roses$hmm[roses$Q18_1 == "wojskowy"] <- "¿o³nierz"
roses$hmm[roses$Q18_1 == "Psi Behawiorysta"] <- "treser"
roses$hmm[roses$Q18_1 == "weteryna¿em"] <- "weterynarz"
roses$hmm[roses$Q18_1 == "rysowaæ komiksy"] <- "grafik"
roses$hmm[roses$Q18_1 == "malarz"] <- "malarz"
roses$hmm[roses$Q18_1 == "medycyna estetyczna"] <- "lekarz, makija¿ysta"
roses$hmm[roses$Q18_1 == "Nie wiem kim dok³adnie, ale kimœ zwi¹zanym z histori¹"] <- "historyk"
roses$hmm[roses$Q18_1 == "Pracowaæ w hodowli zwierz¹t np.egzotycznych"] <- "biolog"
roses$hmm[roses$Q18_1 == "dropshipping"] <- "logistyk"
roses$hmm[roses$Q18_1 == "prowadziæ agroturystykê"] <- "przewodnik, hotelarz"
roses$hmm[roses$Q18_1 == "Antyterroryst¹"] <- "policjant"
roses$hmm[roses$Q18_1 == "kierownik budowy"] <- "szef, budowlaniec"
roses$hmm[roses$Q18_1 == "prawnkiem"] <- "prawnik"
roses$hmm[roses$Q18_1 == "graæ w najlepszym klubie w polsce jakim jest stal mielec"] <- "pi³karz"
roses$hmm[roses$Q18_1 == "coœ zwi¹zanego z chemi¹"] <- "chemik"
roses$hmm[roses$Q18_1 == "Animatorka"] <- "animator"
roses$hmm[roses$Q18_1 == "coœ z gastronomi¹"] <- "kucharz"
roses$hmm[roses$Q18_1 == "mlotocyklist¹"] <- "sportowiec"
roses$hmm[roses$Q18_1 == "Kimœ kto jest zwi¹zany z jêzykiem angielskim"] <- "t³umacz"
roses$hmm[roses$Q18_1 == "Anglistk¹"] <- "nauczyciel"
roses$hmm[roses$Q18_1 == "Raper i producent muzyczny"] <- "piosenkarz"
roses$hmm[roses$Q18_1 == "Ortodontk¹"] <- "dentysta"
roses$hmm[roses$Q18_1 == "Stra¿ po¿arna"] <- "stra¿ak"
roses$hmm[roses$Q18_1 == "stra¿nik leœny"] <- "leœniczy"
roses$hmm[roses$Q18_1 == "wynalasca"] <- "in¿ynier"
roses$hmm[roses$Q18_1 == "Okulist¹"] <- "lekarz"
roses$hmm[roses$Q18_1 == "Pracownikiem firmy"] <- "pracownik biurowy"
roses$hmm[roses$Q18_1 == "komornik"] <- "komornik"
roses$hmm[roses$Q18_1 == "pracownikiem salonu Audi lub innej marki samochodów która mi siê podoba"] <- "sprzedawca"
roses$hmm[roses$Q18_1 == "Kryminologiem i kryminalistykiem"] <- "detektyw"
roses$hmm[roses$Q18_1 == "Koszykarzem"] <- "sportowiec"
roses$hmm[roses$Q18_1 == "Komik"] <- "komik"
roses$hmm[roses$Q18_1 == "Kimœ kto ma coœ wspólnego ze sportem"] <- "sportowiec"
roses$hmm[roses$Q18_1 == "Recepcjonistk¹"] <- "pracownik biurowy"
roses$hmm[roses$Q18_1 == "ekspedientka"] <- "sprzedawca"
roses$hmm[roses$Q18_1 == "Gastronomem"] <- "kucharz"
roses$hmm[roses$Q18_1 == "Kontrolerem ruchu lotniczego"] <- "logistyk"
roses$hmm[roses$Q18_1 == "Medykiem"] <- "lekarz"
roses$hmm[roses$Q18_1 == "Chemiczk¹"] <- "chemik"
roses$hmm[roses$Q18_1 == "Œledczym lub kryminologiem"] <- "detektyw"
roses$hmm[roses$Q18_1 == "diagnost¹ samochodowy,"] <- "mechanik"
roses$hmm[roses$Q18_1 == "elektro monter"] <- "elektryk"
roses$hmm[roses$Q18_1 == "Scenarzystk¹ lub psycholoszk¹"] <- "pisarz, psycholog"
roses$hmm[roses$Q18_1 == "chcia³abym byæ deweloperem"] <- "deweloper"
roses$hmm[roses$Q18_1 == "hydraulik"] <- "hydraulik"
roses$hmm[roses$Q18_1 == "Pi³kark¹"] <- "pi³karz"
roses$hmm[roses$Q18_1 == "saperem lub snajperem"] <- "¿o³nierz"
roses$hmm[roses$Q18_1 == "Adwokat"] <- "prawnik"
roses$hmm[roses$Q18_1 == "dzia³acz na rzecz ochrony  œrodowiska"] <- "ekolog"
roses$hmm[roses$Q18_1 == "Sekretar¹"] <- "pracownik biurowy"
roses$hmm[roses$Q18_1 == "geolog"] <- "geolog"
roses$hmm[roses$Q18_1 == "ksiêgow¹"] <- "ksiêgowy"
roses$hmm[roses$Q18_1 == "testerem gier komputerowych"] <- "tester"
roses$hmm[roses$Q18_1 == "niania"] <- "opiekun"
roses$hmm[roses$Q18_1 == "Pedagaog"] <- "nauczyciel"
roses$hmm[roses$Q18_1 == "solista/dyrygent"] <- "muzyk"
roses$hmm[roses$Q18_1 == "badaczem kosmosu"] <- "astrofizyk"
roses$hmm[roses$Q18_1 == "strzelcem wyborowym si³ specjalnych"] <- "¿o³nierz"
roses$hmm[roses$Q18_1 == "Oficerem"] <- "¿o³nierz"
roses$hmm[roses$Q18_1 == "Ksiêgow¹"] <- "ksiêgowy"
roses$hmm[roses$Q18_1 == "floryst¹"] <- "biolog"
roses$hmm[roses$Q18_1 == "Polonista/ Humanista"] <- "nauczyciel"
roses$hmm[roses$Q18_1 == "hotelark¹"] <- "hotelarz"
roses$hmm[roses$Q18_1 == "Astronem"] <- "astrofizyk"
roses$hmm[roses$Q18_1 == "Raperem"] <- "piosenkarz"
roses$hmm[roses$Q18_1 == "menad¿er lub w rejestracji"] <- "szef"
roses$hmm[roses$Q18_1 == "Nie wiem jeszcze, ale zastanawiam siê nad prawem."] <- "prawnik"
roses$hmm[roses$Q18_1 == "Specjalista do spraw transportu"] <- "logistyk"
roses$hmm[roses$Q18_1 == "g³ownie militaria konstruktor pojazdów"] <- "in¿ynier"
roses$hmm[roses$Q18_1 == "Mangaka, Animator"] <- "grafik, animator"
roses$hmm[roses$Q18_1 == "kierownikiem apteki"] <- "szef, farmaceuta"
roses$hmm[roses$Q18_1 == "twórc¹ gier komputerowych"] <- "programista"
roses$hmm[roses$Q18_1 == "Tatua¿ysta"] <- "makija¿ysta"
roses$hmm[roses$Q18_1 == "Kosmona³ta"] <- "astronauta"
roses$hmm[roses$Q18_1 == "Chcialabym pracowac w wojsku"] <- "¿o³nierz"
roses$hmm[roses$Q18_1 == "Rysownik/Malarz"] <- "malarz"
roses$hmm[roses$Q18_1 == "wojskowym."] <- "¿o³nierz"
roses$hmm[roses$Q18_1 == "Spawacz"] <- "spawacz"
roses$hmm[roses$Q18_1 == "Najlepiej kimœ po technikum ekonomistycznym"] <- "ekonomista"
roses$hmm[roses$Q18_1 == "Psychiatra"] <- "psycholog"
roses$hmm[roses$Q18_1 == "Biznes woman"] <- "szef"
roses$hmm[roses$Q18_1 == "Infomatykiem"] <- "informatyk"
roses$hmm[roses$Q18_1 == "stra¿nikiem granicznym"] <- "policjant"
roses$hmm[roses$Q18_1 == "Projektowaæ reklamy"] <- "grafik"
roses$hmm[roses$Q18_1 == "Spawaæ"] <- "spawacz"
roses$hmm[roses$Q18_1 == "ksiêgowym"] <- "ksiêgowy"
roses$hmm[roses$Q18_1 == "W korporacji"] <- "pracownik biurowy"
roses$hmm[roses$Q18_1 == "Fizykiem"] <- "fizyk"
roses$hmm[roses$Q18_1 == "astronomem."] <- "astrofizyk"


#dok³adam kategorie pod zawody (umys³owy, fizyczny, nauka, rozrywka)
zawody <- c("informatyk", "lekarz", "psycholog", "weterynarz", "architekt", "mechanik", "programista", "pi³karz", "naukowiec", "prawnik",
            "fotograf", "kucharz", "grafik", "t³umacz", "elektryk", "nauczyciel", "kosmetyczka", "projektant", "stra¿ak", "stewardessa",
            "aktor", "dietetyk", "policjant", "fryzjer", "kierowca", "¿o³nierz", "rolnik", "sportowiec", "piosenkarz", "cukiernik",
            "chirurg", "stolarz", "muzyk", "sprzedawca", "dziennikarz", "pisarz", "biolog", "makija¿ysta", "tancerz", "archeolog",
            "fizjoterapeuta", "astronauta", "prezydent", "trener", "opiekun", "wiza¿ysta", "szef", "farmaceuta", "przewodnik", "ratownik",
            "chemik", "pilot", "masa¿ysta", "kosmetolog", "in¿ynier", "artysta", "astrofizyk", "detektyw", "okulista", "bokser", "szachista", "polityk",
            "kardiolog", "logistyk", "tapicer", "mechatronik", "przedszkolank", "dentysta", "ogrodnik", "murarz", "œlusarz", "bankier",
            "copywriter", "koniara", "budowlaniec", "pracownik biurowy", "pawe³ jumper", "rzemieœlnik", "robotnik", "marynarz", "socjolog", 
            "animator", "sêdzia", "fizyk", "leœniczy", "hotelarz", "malarz", "deweloper", "ekonomista", "rybak", "geodeta", "hydraulik",
            "grabarz", "lektor", "treser", "historyk", "komornik", "komik", "ekolog", "geolog", "ksiêgowy", "tester", "spawacz")
length(zawody)
kategorie <- c("umys³owy", "umys³owy", "umys³owy", "umys³owy", "umys³owy", "fizyczny", "umys³owy", "rozrywka", "nauka", "umys³owy", "nauka",
               "rozrywka", "umys³owy", "umys³owy", "umys³owy", "umys³owy", "rozrywka", "rozrywka", "fizyczny", "fizyczny", "rozrywka",
               "umys³owy", "fizyczny", "rozrywka", "fizyczny", "fizyczny", "fizyczny", "rozrywka", "rozrywka", "rozrywka", "umys³owy",
               "fizyczny", "rozrywka", "umys³owy", "umys³owy", "rozrywka", "nauka", "rozrywka", "rozrywka", "nauka", "umys³owy",
               "umys³owy", "umys³owy", "rozrywka", "fizyczny", "rozrywka", "umys³owy", "umys³owy", "fizyczny", "fizyczny", "nauka",
               "umys³owy", "rozrywka", "umys³owy", "rozrywka", "nauka", "umys³owy", "umys³owy", "rozrywka", "rozrywka", "umys³owy",
               "umys³owy","umys³owy", "fizyczny", "umys³owy", "fizyczny", "umys³owy", "fizyczny", "fizyczny", "fizyczny", "umys³owy",
               "umys³owy", "rozrywka", "fizyczny", "umys³owy", "rozrywka", "fizyczny", "fizyczny", "fizyczny", "nauka", "rozrywka",
               "rozrywka", "nauka", "fizyczny", "rozrywka", "rozrywka", "umys³owy", "nauka", "fizyczny", "umys³owy", "fizyczny", "fizyczny",
               "rozrywka", "rozrywka", "rozrywka", "nauka", "umys³owy", "rozrywka", "nauka", "nauka", "umys³owy", "rozrywka", "fizyczny")
length(kategorie)

kategoria <- rep(NA, length(roses$hmm))


for (i in 1:length(zawody)){
  for (j in which(!is.na(kategoria) & stri_detect_fixed(roses$hmm, zawody[i]))){
    kategoria[j] <- paste(kategoria[j], kategorie[i], sep = ", ")
  }
  
  kategoria[stri_detect_fixed(roses$hmm, zawody[i]) & is.na(kategoria)] <- kategorie[i]
}



#dodatkowa kolumna na kategorie
roses$kategoria <- kategoria




roses$Q18_1 <- tolower(roses$Q18_1)
roses$hmm[stri_detect_fixed(roses$Q18_1, "nie") & is.na(roses$hmm) & roses$Q18_1 != "uczniem" & roses$Q18_1 != "jebanie disa"] <- "nie wiem"

rameczka <- roses %>% 
  select(Q18_1 | hmm) %>% 
  filter(is.na(hmm) == TRUE) %>% 
  filter(Q18_1 != "") 

roses$hmm[(stri_detect_fixed(roses$Q18_1, "zastanawiam") |  roses$Q18_1 == "nwm" | roses$Q18_1 == "jeszcze nw")& is.na(roses$hmm)] <- "nie wiem"





write.csv(roses, file = "roses_1.csv")

