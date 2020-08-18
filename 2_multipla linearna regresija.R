
### 2. vaja (14. 10. 2019)

### MULTIPLA LINEARNA REGRESIJA

### 2.1 MULTIPLA REGRESIJA

  # Prvi primer, ki ga bomo naredili, je tipiČno napovedovalni model.
  # Na osnovi dosežkov na testu nizov (TN - splošna inteligentnost) ter dosežkov na vprašalnikih prijaznosti (PRI),
  # natanČnosti (NAT) in vztrajnosti (VZT) bomo napovedovali delovno uspešnost nižjih vodij.
  # če napovedujemo delovno uspešnost, lahko nek kandidat nižjo delovno uspešnost kompenzira s koliČino delovnih
  # izkušenj.
  # Je aditiven model - seštevamo utežene vrednosti napovednikov. UČinki se seštevajo, nižji se kompenzirajo z višjimi.

  # Regresija je lahko napovedovalna metoda (posamezne osebe) ALI pojasnjevalna metoda (moČ posameznih napovednikov).
  # V našem priemru nas ne zanimajo konkretni ljudje, zanima nas moČ posameznih napovednikov (relativna pojasnjevalana 
  # moČ posameznih napovedi).
  # Selekcijski model, s katerim bodo za kandidate izbrali tiste, za katere je najbolj verjetno, da bodo uspešni.

  # Podatki
  # Preizkušanci so bili ljudje, ki so delali v proizvodnji in so vodili skupino ljudi. Ali lahko uspešnost teh vodij
  # napovemo na podlagi njihovih osebnostnih lastnosti (gledamo rezultate na testih in vprašalnikih navedenih zgoraj).

######### ponovi linearno regresijo #########


# ----------------------------------------------------------------------------------------------------------------------
### DomaČa naloga do 21. oktobra:
  # Preglejte podatke za multiplo regresijo. Preverite, ali so v podatkih manjkajoČe vrednosti in ali se pojavljajo
  # vrednosti, bistveno nižje ali višje od priČakovanega razpona (pribl. +/- 3 SD). Preglejte obliko porazdelitve in
  # razmislite, v kolikšni meri se pri posameznih spremenljivkah prilega normalni (pomagajte si s funkcijami hist ali
  # histkriv, carÃ¢ÂÂ·qqPlot, boxplot ali carÃ¢ÂÂ·Boxplot, psychÃ¢ÂÂ·describe). Preverite prisotnost uni-in multivariatnih osamelcev
  # (za slednje si pomagajte s funkcijo mahalanobis ali mahalqq). Preglejte korelacijsko matriko in ocenite, ali so
  # odnosi med spremenljivkami približno linearni (uporabite funkciji psychÃ¢ÂÂ·corr.test in carÃ¢ÂÂ·spm).

po <- read.table("mr1.txt", header=T, dec=",")


 ## Ali so v podatkih manjkajoČe vrednosti?
is.na(po)
mv <- rowSums(is.na(po))

 ## Ali se pojavljajo vrednosti, bistveno nižje ali višje od priČakovanega razpona (pribl. +/- 3 SD)?
describe(po)

po$TN[po$TN > 99.55+3*15.76]
po$TN[po$TN < 99.55-3*15.76]

po$PRI[po$PRI > 50.07+3*10.52]
po$PRI[po$PRI < 50.07-3*10.52]
  # 50.07-3*10.52 = 18.51
ftabela <- table(po$PRI)
ftabela

po$NAT[po$NAT > 49.53+3*8.83]
po$NAT[po$NAT < 49.53-3*8.83]

po$VZT[po$VZT > 48.88+3*9.72]
po$VZT[po$VZT < 48.88-3*9.72]

po$DU[po$DU > 39.27+3*6.90]
po$DU[po$DU < 39.27-3*6.90]

 ## Preglejte obliko porazdelitve in razmislite, v kolikšni meri se pri posameznih spremenljivkah prilega normalni
  # (pomagajte si s funkcijami hist ali histkriv, carÃ¢ÂÂ·qqPlot, boxplot ali carÃ¢ÂÂ·Boxplot, psychÃ¢ÂÂ·describe)

hist(po$TN)
histkriv(po$TN)
qqPlot(po$TN)
boxplot(po$TN)
Boxplot(po$TN)
describe(po$TN)

histkriv(po$PRI)
qqPlot(po$PRI)
Boxplot(po$PRI)
describe(po$PRI)

histkriv(po$NAT)
qqPlot(po$NAT)
Boxplot(po$NAT)
describe(po$NAT)

histkriv(po$VZT)
qqPlot(po$VZT)
Boxplot(po$VZT)
describe(po$VZT)

histkriv(po$DU)
qqPlot(po$DU)
Boxplot(po$DU)
describe(po$DU)

 ## Preverite prisotnost uni-in multivariatnih osamelcev (za slednje si pomagajte s funkcijo mahalanobis ali mahalqq).
mahalqq(po[,2:5])

 ## Preglejte korelacijsko matriko in ocenite, ali so odnosi med spremenljivkami približno linearni (uporabite funkciji
  # psych:: corr.test in carÃ¢ÂÂ·spm).
psych:: corr.test(po[,2:5])
car:: spm(po[,2:5])
# ----------------------------------------------------------------------------------------------------------------------


### 3. vaja (21. 10. 2019)

x <- read.table("mr1.txt", header = TRUE)

 ## PREGLED
apply(x, 2, hist)
psych:: describe(x)
car:: qqPlot(x$DU)

psych:: corr.test(x)
  # želimo si, da bi bile korelacije med napovedniki Čim manjše.
car:: spm(x)

### REGRESIJSKI MODEL
model1 <- lm(DU ~ TN + PRI + NAT + VZT, data = x)
  # S funkcijo lm() izdelamo "regresijski objekt".
  # Najprej napišemo odvisno spremenljivko oz. kriterij (spremenljivko, ki jo pojasnjujeno) in potem neodvisne
  # spremenljivke oz. prediktorje (napovednike, pojasnjevalne spremnljivke).
  # Argument "data =" potrebujemo, ker so spremenljivke del podatkovnega okvirja.

povzetek <- summary(model1)
povzetek
  # S pomoČjo funkcije summary() iz model1 izvleČemo podatke, ki so pomembni, kljuČne rezultate. Z zgornjim ukazom torej
  # dobimo povzetek.
  # Imamo nekaj o porazdelitvi ostankov/rezidualov/napak. K temu, k porazdelitvi ostankov, se bomo vrnili kasneje.
  # Potem pa imamo že prvi bistveni del rezultatov - to so KOEFICIENTI.
  # U(i) = a + b1*I(i) + b2*P(i) + b3*N(i) + b4*V(i) //+ ei// -> [narobe obrnjena strešica nad prvim U]
  # ei je napaka - lahko reČemo, da je napovedana vrednst
  # Naš model ima 4 nagibe in eno konstanto - 5 parametrov (opisani so v prvem stolpcu).

  # Tabelo koeficientov bi lahko izlušČili tudi takole:
povzetek$coefficients
 ## Kaj ti KOEFICIENTI pomenijo?
  # Izraz "estimate" pomeni, da gre za oceno - gre za vzorČno oceno (poskušamo oceniti populacijske vrednosti).
  # Najprej imamo KONSTANTO ali PRESEčIŠčE, ki ima v našem primeru vrednost 11,7.
  # Kaj nam to pove? Je napovedana vrednost za osebo, ki ima vrednost 0 na vseh napovednikih. V našem vzorcu nimamo
  # nikogar, ki bi imel vrednost niČ na kateremkoli testu. Tudi najnižji možni rezultati na posameznih testih niso niČ 
  # (to veš, Če pogledaš v priroČnik za te teste).
  # V našem primeru konstanta (preseČišČe) nima pomena, je ne moremo interpretirati, nima relevantne interpretativne
  # vrednosti.
  
  # Kaj pomeni nagib 0,15 pri PRIjaznosti? Nagib nam pri linearni regresiji pove, za koliko se spremeni napovedana 
  # vrednost odvisne spremenljivke, Če bi se dosežek na neodvisni spremenljivki poveČal za eno toČko.
  # Tu pa nam NAGIB pove ... za koliko bi se poveČala napovedana vrednost, Če bi se dosežek na PRI poveČal za 1 toČko,
  # dosežek na ostalih testih pa bi ostal enak.
  # Boljša razlaga oz. lažje predstavljivo je takole:
  # Predstavljajmo si, da imamo 2 osebi, ki se na PRIjaznosti razlikujeta za 1 toČko, vse ostale rezultate pa imata 
  # enake (TN, NAT in VZT) - pomeni, da bi tista oseba, ki ima višji rezultat na PRI, imela celoten dosežek (torej vse 
  # skupaj, kar pomeni delovno uspešnost - DU) za 0,15 toČke višji.

  # Paziti moramo samo na to, da se vprašamo, ali je sploh smiselno, da si predstavljamo dve taki osebi. Je sploh
  # možno, da bi 2 taki osebi obstajali? Ni razloga, da ne bi obstajali, je pa malo verjetno, da ju bomo našli.

  # NajveČja nagiba sta pri PRI in VZT, pri TN in NAT pa sta nagiba (naj)manjša. Ne moremo kar avtomatiČno razlagati,
  # da sta napovednika z višjimi nagibi, boljša napovednika - odvisno je tudi od variabilnosti napovednika. Ne moremo
  # sklepati neposredno.
  # K interpretaciji nagibov se bomo vrnili kasneje.

 ## STANDARDNA NAPAKA NAPOVEDI (Std. Error) nam pove, kako natanČne so ocene. Nanje vpliva tudi napaka vzorČenja.
  # VeČja kot je standardna napaka, veČja bi bila nihanja, Če bi to ponavljali v isti populaciji (Če bi iz iste populacije
  # vzeli veliko št. vzorcev in izvedli analize oz. Če bi iz iste populacije veČkrat vzeli enako velik vzorec in izvedli
  # analize - izraČunaš parametre in dobiš razliČne nagibe)
  # -> Standardna napaka napovedi (SE) je enaka standardni deviaciji (SD) regresijskih nagibov za posamezno spremenljivko
  # preko razliČnih vzorcev.
  # želimo si, da je SE Čim manjša.
  # Sicer pa posameznih standardnih napak spet ne moremo kar tako primerjati med sabo.
  # //?če imamo model, ki ne vsebuje napovednikov (U = a), je napovedana vrednost konstanta. Najboljša napoved je, da vsem
  # napovemo M, aritmetiČno sredino, vsa odstopanja od povpreČja so napaka napovedi.
  # Napaka ne sme biti veČja od SD uspešnosti (napovedi), pri nas je SD s 6,9 zmanjšana na 6,23.?//

 ## T-VREDNOST je razmerje med koeficientom in njegovo standardno napako.
  # Testiramo statistiČno znaČilnost regresijskih nagibov:
  #   H0: bi = 0.
  # če H0 drži, potem se b1/SE(b1) porazdeljuje po t-porazdelitvi.

  # Kaj bi to (H0) s praktiČnega, interpretativnega vidika pomenilo? če kontroliramo ostale napovednike, potem ta
  # napovednik ne korelira veČ s kriterijem, ne napoveduje veČ kriterija. če kontroliramo ostale napovednike, tak
  # napovednik ne pojasni niČesar, kar ne pojasnjujejo že ostali napovedniki (niČ novega ne pojasnjuje - kar pojasnjuje
  # ta, pojsnjujeo tudi drugi, ne poveČa pojasnjenje variance). Tak napovednik torej nima dodane vrednosti.

  # Pogledamo P-VEREDNOSTI za to H0. StatistiČno znaČilno je vse, razen natanČnosti.
  # Imamo napovednik, ki ni statistiČno znaČilen.

  # Za natanČnost torej tega ne moremo trditi - da se nagib za natanČnost v populaciji razlikuje od 0. Kar pomeni, da
  # natanČnost ne pojasni niČesar, kar ne bi pojasnjevale že ostale tri spremenljivke, nima dodane vrednosti
  # (SD je zelo velika).
  # Tako visok nagib bi dobili v 37 % vzorcev, Če bi bili ostali nagibi 0.

  # Kaj pomeni, da imamo en napovednik, ki niČesar ne pojesnjuje?
  # Odvisno od tega, ali imamo napovedovalni model ali pojasnjevalni model.
  # Pri napovedovanju želimo enostaven model, s Čim manj napovedniki, ki bi pojasnjevali Čim veČ variance, da imajo
  # dodano vrednost pri napovedovanju.
  # Pri pojasnjevanjevalnih modelih pa je dobro ohraniti napovednike, ki nimajo ststistiČno znaČilnega nagiba.

 ## KOEFICIENT DETERMINACIJE (r^2)
  # Še ena mera natanČnosti je r-kvadrat (multiple R-squared) oz. koeficient determinacije, ki nam pove delež pojasnjene
  # variance - v našem primeru pojasnimo 20,7 % variance.
povzetek$r.squared
 ## MULTIPLA KORELACIJA
sqrt(povzetek$r.squared)
  # če korenimo r^2, dobimo multiplo korelacijo (v našem primeru 0,455).

  # Problem koef. determinacije je, da nam daje pristranske ocene deleža pojasnjene variance. V povpreČju je ta vrednost
  # višja od populacijske vrednosti, zato po navadi izraČunamo še ...
 ## PRILAGOJENJI R^2 (Adjusted R-squared), ki pa je nepristranska ocena populacijske pojasnjene variance.
  # Ta prilagojeni r-kvadrat je po navadi nižji od obiČajnega, odvisno od N in napovednikov.
  # Dobro je imeti Čim veČ ljudi in Čim manj napovednikov (vsaj 30 ljudi na napovednik).

 ## Na koncu povzetka imamo še TEST STATISTIčNE ZNAčILNOSTI pojasnjene variance ("Multiple R-squared").
  # Tu pa testiramo niČelno hipotezo, ki je:
  #     H0: R^2(pop) = 0
  # (z nobenim napovednikm v populaciji Čisto niČesar ne pojasnimo, naš model je popolnoma neuporaben).
  # V našem primeru H0 zavrnemo, vrednost F statistike pri prostostnih stopnjah 4 in 142 je 9,24, p < 0,001.
  # F(4, 142) = 9,24; p < 0,001
  # F je funkcija razmerja med pojasnjeno in nepojasnjeno varianco. Df pa podobno kot pri anovi ...
  #   Prve df (4) so št. parametrov minus 1 (4 nagibi in preseČišČe).
  #   Druge df (142) pa so numerus minus št. parametrov, ki jih ocenimo (147-5).
  # Z našimi napovedniki pojasnimo vsaj del/vsaj nekaj variance, naš model ni brez vrednosti.

 ## V Objektu, ki ga dobimo s funkcijo summary() imamo naslednje elemente:
  #     - koeficienti (s stand. napakami, t in p vrednostmi): povzetek$coefficients
  #     - standardna napaka napovedi: povzetek$sigma
  #     - delež pojasnjene variance (r-kvadrat ali koef. determinacije): R2 <- povzetek$r.squared
  #     - multipla korelacija: R <- sqrt(R2)
  #     - delež pojasnjene variance z Wherryjevim popravkom (prilagojeni R2): R2c <- povzetek$adj.r.squared
  #     - vektor z F statistiko in prostostnimi stopnjami: povzetek$fstatistic


  # Za OBSEžNEJŠI POVZETEK regresijskega modela lahko uporabimo funkcijo "regtabela" (v datoteki z dodatnimi funkcijami;
  # zahteva paket car). Vse inferenČne statistike v funkciji "regtabela" predpostavljajo normalno porazdelitev ostankov!
rt1 <- regtabela(model1)
  # V funkcijo damo objekt, ki smo ga dobili s funkcijo lm() - regresijski objekt.
  # Funkcija je iz datoteke z dodatnimi funkcijami "funkcijeMVM".
  # če hoČemo to tabelo vkljuČiti v poroČilo, uporabimo funkcijo kex, datoteko s tem kopiramo npr. v Excel.
kex(rt1)

  # Objekt, ki ga dobimo s funkcijo regtabela(), vsebuje naslednje podatke:
  #   - b: regresijski koeficienti
  #   - SE: njihove standardne napake
  #   - t: t vrednosti (Waldov test stat. znaČilnosti regresijskih koeficientov)
  #   - p: p-vrednost za dvostranski test stat. znaČilnosti regresijskih koeficientov
  #   - IZ: spodnja in zgornja meja 95% intervala zaupanja za regr. koeficiente
  #   - beta: standardizirani regresijski koeficienti
  #   - kspr: kvadrirana semiparcialna korelacija
  #   - VIF: faktor poveČanja kvadrata stand. napake (variance inflation factor)
  #   - Tol.: toleranca

 ## INTERVALI ZAUPANJA
  # Tu so parametriČni IZ, ki predpostavljajo, da veljajo vse predpostavke. Kar v našem primeru bolj ali manj
  # velja. IZ so koristni, ker nam podajo malo bolj oprijemljivo oceno natanČnosti posameznih regresijakih nagibov
  # (razpon vrednosti, ki se nam zdijo verjetne, dopušČamo, da se v njem nahaja populacijska vrednost).
  # Naše vrednsoti IZ od 2.47 do 20.96. želimo si, da bi bili obe meji blizu skupaj - tu niso.
  # IZ dopušČa 0 pri NAT, H0 nismo zavrnili, reg. nagib je lahko -b, 0 in +b.

  # če ne zaupamo osnovnim predpostavkam (npr. normalnost, homoscendastiČnost), lahko IZ doloČimo z zankanjem.
IZ_zankanje <- regzank(model1)
  # Edini argument je, kar smo dobili z regresijo. (V funkciji je nastavljeno na 999.)
  # Funkcija izpiše:
  #     - vzorČne nagibe
  #     - parametriČne standardne napake
  #     - Pristr. = pristranskost ... v kolikšni meri vzorČne ocene odstopajo od bootstrap ocen (dobro je, da so
  #                 Čim manjše)
  #     - SE_bs ... standardna napaka pridobljena z zankanjem
  #     - Na koncu so še IZ pridoljeni z zankanjem.

  # Razlike med rezultati (za IZ) obeh funkcij so majhne.

 ## BETA - standardizirani regresijski koeficienti
  # Prej smo videli, da koeficientov napovednikov ne moremo kar tako primerjati, ker je njihova variabilnost razliČna.
  # Če variance izenaČimo, pa koeficiente lahko primerjamo. Variance izenaČimo tako, da standardiziramo napovednike.
  # Standardiziramo (napovedana standardizirana uspešnost):
  #     Zv = beta1*z(I) + beta2*z(P) + beta3*z(N) + beta4*z(V) <- [narobe obrnjena strešica nad prvim Zv]
  # Kje se je izgubila konstanta?
  # [a = Y - sum(bjXj)]
  # [a = napovedan Y - vsota bj*X(povpr.)j]
  # a = [narobe obrnjena strešica nad prvim Y] Y - [znak za sum ~ tisti E]bj[nad X znak za povpreČje, tista ravna Črta]Xj
  # Konstanta je enaka niČ, to je nujnost.

  # če imata dve osebi enake rezultate, le pri vztrajnosti se razlikujeta za 1 SD, se bosta v napovedani uspešnosti
  # razlikovali za manj kot Četrtino standardnega odlklona (za 0,240).

  # Beta koef. torej uporabljamo za neposredno primerjavo napovednikov.
  # VČasih beta koeficiente uporabljamo za presojo, kateri napovednik je bolj pomemben. VeČji beta - veČja napovedna
  # vrednost napovednika.
  # Bolj se spremeni vrednost napovedane spremenljivke ob standarni spremembi napovednika.

  # Bolj neposredna vrednost (za presojo, kateri napovednik je bolj pomemben) je ...
 ## Kvadrirana SEMIPARCIALNA KORELACIJA (kspr) - pove nam, za koliko se zmanjša keof. determinacije, Če nek napovednik
  # izloČimo iz modela.
  # če bi izloČili VZT, bi pojasnili le 15 % variance (celotna poj. var. oz. r^2 je 20,7 % - kspr(VZT) je 5,5 %).
  # Če bi izloČili NAT, bi izgubili manj kot pol odstotne toČke (0,446), izloČitev NAT bi le zanemarljivo zmanjšala 
  # napovedno moČ modela/delež pojasnjene variance. 
  # Te vrednsoti (kspr) so deleži variance, ki jih pojasnjimo s posameznimi napovediki in z nobenim drugim.
  # če jih seštejemo ne bomo prišli na 20 %. PaČ razlika je med vsoto posameznih pojasnjenih varianc in skupno pojasnjeno 
  # varianco - lahko tudi po dva napovednika skupaj prispevata k pojasnjeni varianci (tam je pa samo za vsakega posebej).
  # VIšje korelacije med napovedniki - nižje kspr.
  # NAT ima tako majhno dodano vrednost - ima najvišje korelacije z vsemi drugimi napovedniki.
  # Na koreliranost med napovedniki se nanašata zdanja dva stolpca v tabeli.

 ## VIF (Variance Inflation Factor) oz. faktor poveČanja variance nagibov - za koliko se poveČa, zaradi koreliranosti
  # napovednikov. //?SE(bi)^2 ...?// pri lestvici NAT za 39 %, pri VZT za 5 %.
  # NAT ni slab napovednik uspešnosti, ampak tu skoraj nimam dodane vrednosti, ker visoko korelira z drugimi napovedniki.

 ## TOLERANCA nam pove odstotek variance napovednika, ki ga ne moremo pojasniti z drugimi napovedniki, kolikšen
  # delež variance napovednika ne moremo pojasniti z drugimi. Fino je torej, da je Čim višja - torej nizko korelira z
  # drugimi.
  # Toleranca se niža, ker so napovedniki povezani. Nižja kot je toleranca, veČja bo standardna napaka reg. nagiba (za 
  # koliko, pa nam pove VIF) - zato je nizka toleranca problematiČna. 

 ## Kaj od vse te množice rezultatov bi navedli v rezultatih?
  # Nagibe, standardne napake, IZ, Če želimo tudi p vrednosti, eno od mer pomambnosti (v Člankih pogosto beta koef.,
  # so pa semiparcialne korelacije mogoČe bolj razumljive).


### 4. vaja (28. 10. 2019)

 ## OCENJEVANJE PARAMETROV: Intervali zaupanja za regresijske parametre

  # IZ lahko doloČimo analitiČno po formuli (te pa predpostavljajo, da držijo vse predpostavke). če pa predpostavkam
  # ne zaupamo povsem, jih lahko doloČimo s pomoČjo zankanja.
  # Avtorji so v paket car dodali novo funkcijo za izraČun IZ (z bca popravkom - popravkom za pristranskost in
  # normalnost vzoČne ocene).

  # PARAMETRIčNI IZ
  # če nas zanimajo parametriČni IZ, lahko uporabimo funkcijo "confint()".
confint(model1, level = .95)
   # Enaki IZ kot pri regtabeli rt1.

  # NEPARAMETRIčNI IZ
  # če pa hoČemo dobiti IZ z zankanjem, vzamemo funkcijo "Boot"iz paketa car.
zankanje_parametri <- Boot(model1, R = 5000)
  # Nujen argument je regresijski objekt, ki smo ga dobili s funkcijo lm(); potem lahko poveČamo število vzorcev R;
  # hoČemo pa tudi, da nam rezultate zapiše v nek objekt (zankanje_parametri).
confint(zankanje_parametri)
  # Ven dobimo bca 95-% IZ.
  # Funkcija confint() je zelo prilagodljiva in prepozna argumente.
  
  # Razlike med parametriČnimi in neparametriČnimi IZ so praktiČno zanemarljive, majhne. Kar pomeni, da je z vidika
  # ocene natanČnosti posameznih nagibov, vseeno ali vzamemo parametriČne IZ ali IZ pridobljene z zankanjem.

 ## PRIMERJAVA MODELOV

  # Ker je naš model napovedni model ... je naš cilj izdelati Čim bolj enostaven model s Čim veČjo napovedno moČjo.
  # Lahko imamo v modelu spremenljivke, ki so v modelu za kontrolo - recimo, da hoČemo kontrolirati uČinke starosti,
  # da hoČemo model s kontrolo starosti. če pa nagib ni statistiČno znaČilen in nimamo nekega posebnega (vsebinskega)
  # razloga, da bi spremenljivko ohranili v modelu, jo izloČimo iz modela (veČ kot imamo napovednikov, manj
  # stabilne so ocene parametrov).

  # Iz modela izloČimo napovednik z neznaČilnim nagibom:
model2 <- lm(DU ~ TN + PRI + VZT, data = x)
  # V model torej nismo vkljuČili vseh spremenljivk.
  # Prva stvar, ki jo je mogoČe smiselno narediti, je, da pogledamo osnovne rezultate, ki smo jih s tem modelom dobili.
povzetek2 <- summary(model2)
  # Vidimo, da imajo vsi trije napovedniki, statistiČno znaČilne nagibe.
  # VzorČni r-kvadrat je nekaj Čez 20 %, za zelo majhen delež se je zmanjšal - izgubili smo za 4 desetine odstotne
  # pojasnjene variance.
  # Testiramo:
  #     H0: R(M2)^2 = 0
  #     oz. r-kvadrat za popoulacijo je enak 0
  # Ker so vsi nagibi vseh napovednikov statistiČno znaČilni, je tudi pojasnjena varianca stat. znaČ. razliČna od niČ.
  # F(3, 143) = 12,1; p < 0,001
  # če bi bila pojasnjena varianca 0, bi samo v štirih od milijonov vzorcev primerov dobili tak rezultat, tako velik
  # koef. determinacije kot tukaj.

  # Ali smo zdaj, ko smo izloČili en napovednik, poslabšali model oz. ali se je napovedna moČ modela zmanjšala
  # oz. ali je zmanjšanje pojasnjene variance statistiČno znaČilno?
  #   H0: [velika delta oz. trikotniČek]R^2 = 0 ali R(M2)^2 = R(M1)^2
  #       oz. razlika v pojasnjeni varianci med modeloma je 0 ali R^2 za model2 je enak R^2 za model1
  #   H1: R(M2)^2 < R(M1)^2
  #       oz. alternativna hipoteza je, da je R^2 za model2 manjši kot R^2 za model1 (odvzem NAT zmanjša tudi poj. var.)
  # Kako preverimo to hipotezo? Spet naredimo F-test.
anova(model2, model1)
  # Navedemo oba modela, ki ju želimo primerjati: najprej manjši model - tistega z manj napovedniki, bolj deskriptivnega
  # in potem veČji model - tistega z veČ napovedniki.
  # Ta test je primeren zgolj za gnezdene modele. To so taki modeli, kjer en model vsebuje vse napovednike, ki jih 
  # vsebuje tudi drugi model, en model vsebuje vse napovednike prvega modela, bistveno je to, da en model lahko dobimo
  # iz drugega (ali tako da bodisi dodamo bodisi odvzemamo napovednike iz modela) -> napovedniki enega modela so tako
  # podmnožica drugega modela.
  # Vedno, kadar primerjaš razliČne modele med sabo, poglej, da bodo imeli neko zaporedje gnezdenih modelov - vedno
  # dodajamo ali izkljuČujemo napovednike, nikoli ne delamo obojega v istem koraku.

  # PAZI! Nekaj terminološke zmede ...
  # VeČji model (naš model1) oznaČi kot Model 2, v drugem koraku so torej rezultati za veČji model.
  # Manjši model (naš model2) pa oznaČi kot Model 1.

  # Kako dobimo F statistiko?
  # Manjša je od 1, takoj vidimo, da ni statistiČno znaČilna.
  # Vsota kvadratov nam pove, kolikšna je razlika v vsoti kvadratov napak, ko iz modela izkljuČimo natanČnost.

  # sprememba R^2 = R^2(M1) - R^2(M2) = 0.20661 - 0.20215 = 0,0045
  # [velika delta oz. trikotniČek]R^2 = 0,0045; F(1, 142) = 0,80; p = 0,37
  # Zakaj je pri df pri F statistiki najprej 1 df? Ker se modela razlikujeta za en napovednik/parameter.
  # če bi imela oba modela enako napovedno moČ (da natanČnost ne bi imela nobene dodane vrednosti pri napovedovanju),
  # potem bi pri 3 % primerov dobili tako (veliko) razliko v pojasnjeni varianci v populaciji.
  # Model z vkljuČeno natanČnostjo ne pojasni (statistiČno znaČilno) veČ variance kot model brez nje - poslediČno je
  # bolj smiselno ohraniti manjši model.

  # Stranski uČinek zmanjšanja modela ... za ponazoritev primerjajmo model1 in model2.
summary(model1)
summary(model2)
  # Standardna napaka v veČjem modelu je veČja kot standardna napaka v manjšem modelu (v manjšem modelu je manjša).
  # Napovedniki imajo v manjšem modelu manjšo standardno napako. Standardne napake so manjše, ko smo iz modela izloČili
  # NAT. Tudi IZ so ožji. Ocene regresijskih modelov so boljše. NatanČnost je bila korelirana z ostalimi napovedniki in
  # je zmanjševala moČ ostalih napovednikov. Napovedniki so (v modelu 2) manj korelirani med seboj, zato se standardne
  # napake zmanjšajo.

rt2 <- regtabela(model2)
  # Toleranca je skoraj 1 (dve tisoČini posameznega napovednika - TN) - zelo majhen delež variance posameznega napovednika
  # pojasnimo z odnosom z ostalimi napovedniki.
  # Zaradi koreliranost se standardne napake nagibov zanemarljivo poveČajo.

  # Majhen ekskurz ...
  # Funkcijo anova() lahko uporabimo vedno, ko med sabo primerjamo modele. VČasih že v naprej predvidevamo primerjavo
  # modelov, Če vemo, da ga bomo nadgrajevali.

  # Pojasnjevalni modeli - v kolikšni meri izboljšamo moČ modela. [Manca]
  # Po navadi to poČnemo pri napovednih modelih, zanima nas, za koliko se poveČa napovedna moČ, Če dodamo nov napovednik.
  # Po navadi Čisto v 1. koraku v model dodamo kontrolne spremenljivke (ko si eksplicitno želimo kontrolirati
  # spol, izobrazbo ali kaj podobnega) in izraČunamo R^2.
  # V 2. koraku pa vkljuČimo prvi vsebinsko zanimiv napovednik ali pa skupino napovednikov. Ponovno izraČunamo R^2 (za ta
  # nov model s kontrolnimi spremenljivkami in dodanimi napovedniki) in s funkcijo anova testiramo, ali je poveČanje
  # pojasnjene variance statistiČno znaČilno, ohranili bomo seveda samo tiste napovednike, ki statistiČno znaČilno
  # poveČajo pojasnjeno varianco.
  # 3. korak: Potem spet dodamo nove napovendnike in izraČunamo pojasnjeno vararianco in testiramo statistiČno znaČilnost
  # poveČanja pojasnjene variance itd.).

  # Vrstni red dodajanja napovednikov je doloČen - poglobimo se v teorijo in ga doloČimo. Vrstni red torej doloČimo na
  # osnovi teorije, kateri napovediki so najbolj temeljni in pomembni. Imamo hierarhijo napovednikov in gremo po vrsti.
  # Takemu naČinu reČemo hierarhiČno vkljuČevanje napovednikov (legitimen naČin konstrukcije modela).
  # Poglobiti se moramo v problem. če je problem nov, imamo na voljo avtomatizirane postopke za vkljuČevanje
  # napovednikov - samodejna gradnja modela. Lahko so nevarni, saj izkorišČajo napako vzorČenja. Omejiti moramo število
  # napovednikov, model moramo navzkrižno validirati na novem vzorcu.

 ## SAMODEJNA GRADNJA MODELOV (postopna gradnja modelov)
  # Funkcija step() poišČe model, ki optimizira Akaikejev informacijski kriterij (AIC) Ã¢ÂÂ optimalno ravnotežje med
  # parsimoniČnostjo in pojasnjevalno moČjo.
  # Akaikejev informacijski kriterij (AIC) - ta statistika je mera informativnosti modela. Kombinira pojasnjevalno moČ in
  # enostavnost modela, to dvoje si želimo uravnovestiti in ta mera poskuša narediti ravno to.
step(model1)
  # Funkcija primerja razliČne modele in izbere tistega z najbolj optimalno vrednostjo AIC (to je tisti, ki ima
  # najnižjo vrednost). Najboljši model je torej tisti z najnižjim AIC.
  # V funkcijo vpišemo najveČji model.
  # PriČne z vsemi napovedniki in nato izloČa posamezne napovednike in gleda, kaj se zgodi z modelom. V našem primeru je
  # vsota kvadratov za napako najmanjša pri modelu z najveČ napovedniki in vrednost AIC je najmanjša, Če izloČimo
  # NAT - takrat je model najboljši. če izloČimo kaj drugega, model poslabšamo.
  # V prvem koraku izloČimo NAT, v drugem koraku pa vidimo, da bi z izloČitvijo kateregakoli napovednika model le poslabšali
  # in je najbolje, da ostanemo pri vseh treh napovednikih.

  # Ta postopek uporabljaj le z veliko previdnosti in Če res ne gre drugaČe.
  # Samo v izrazito eksploratornih situacijah! (Nimamo hipotez o relativni pomembnosti napovednikov.)

 ## NAPOVEDOVANJE dosežkov posameznih oseb
  # OČitno je, da bomo za napovednovanje uporabili drugi model.
  # Vzemimo, da imamo neko novo osebo, za katero bi želeli napovedati njeno uspešnost. Recimo, da imamo nekoga, ki je
  # šele zaČel delati in želimo narediti oceno njegove bodoČe uspešnosti. Rezultati te osebe so taki: TN = 85, PRI = 60
  # in VZT = 60.
describe(x)
  # Pogledamo kakšna je ta oseba glede na ostale (pod/nad povpreČjem pri posazemnih spremenljivkah).
  # Z višjo prijaznostjo in vztrajnostjo kompenzira manjšo/nižjo inteligentnost.

  # Lahko izraČunamo na roke ... (konstanta + nagib*doesžek + ...)
12.32812 + 0.09719*85 + 0.17002*60 + 0.17913*60
  # Oseba je malenkost nad povpreČjem pri uspešnosti (torej Če združimo vse skupaj).
  # Nimamo pa podatka o natanČnosti te ocene ... rabimo intervalno oceno, podajamo napovedni interval, kjer se bo z
  # doloČeno verjetnostjo nahajala ta mera uspešnosti.

  # Uporabiti moramo funkcijo predict().
  # Da bomo to funkcijo lahko uporabili, moramo zapisati podatke te naše nove osebe v nek podatkovni okvir.
  # Najlažje je, da jih zapišemo kot vektor in ta vektor zlepimo skupaj z že obstojeČim podatkovnim okvirjem.

  # Kako naredimo nov podatkovni okvir?
xn <- rbind(c(85, 60, NA, 60, NA), x)
  # V prvi vrstici podatkovnega okvirja xn imamo podatke te nove osebe.

  # NAPOVEDNI INTERVAL
  # (s kolikošno verjetnostjo bo padla vrednost odvisne spremejljivke /..... bo padla dejanska vrednost napo...???/
  # /s 95% verjenostjo se bo nahajala uspešnost/)
napoved <- predict(model2, xn, interval = "prediction", level = .95)
  # Vsaj 2 argumenta: model na osnovi katerega napovedujemo + podatki za katere želimo napovedati vrednosti.
  # če hoČemo imeti še interval, dodamo ta argument (imamo pa 2 možnosti: napovedni interval ali interval zaupanja).

xn <- cbind(xn, napoved)
  # Napoved imamo v zadnjih treh stolpcih podatkovnega okvirja xn.
  # Napovedana vrednost (fit) je 41,5.
  # Zakaj ni isto kot smo prej izraČunali na roke? Ker smo prej vzeli zaokrožene vrednosti).
  # 95 % oseb s takim vzorcev odgovrov ima dejasnko uspešnost med 29 in 54 toČkam.
  # Možno je torej, da bo imela ta oseba uspešnost za 1 standardni odklon pod povpreČjem ali 2 standardna odkolona nad
  # povpreČjem (gledamo DU iz describe(x) in te nove vrednosti). Napovedni interval je zelo širok.

  # Obstaja pa še ena možnost ...
  # INTERVAL ZAUPANJA ZA NAPOVEDANO VREDNOST
napoved_IZ <- predict(model2, xn, interval = "confidence", level = .95)
  # Fit je napovedana vrednosti, IZ pa je ožji. IZ za populacijsko napovedano vrednost.
  # Interval nam pove, kako toČno, natanČno smo ocenili napovedano vrednost (kako natanČno smo ocenili parametre).
  # Na katerem intervalu se bo nanašala povpreČna vrednost vseh teh oseb s takim vzorcem vrednosti napovednikov
  # (paČ Če imajo osebe enake podatke kot naša oseba). (?)

  # V praksi nas ponavadi najbolj zanima napovedni interval, ki nam pove, s kolikšno verjetnostjo se bo nahajala
  # oseba, s stakšnim vzorcev rezultatov. (?)

 ## PREDPOSTAVKE
  # Napovedovanje je verodostojno samo takrat, kadar veljajo predpostavke linearnih modelov:
  # 1. neodvisnost opazovanj (ne velja npr. pri veČstopenjskem vzorČenju - gledamo, da osebe ne vplivajo eno na drugo)
  # 2. linearnost (napovedane vrednsoti morajo biti v linearnem odnosu z dejanskimi vrednostmi)
  # 3. noramlnost porazdelitve ostankov
  # 4. homoscedastiČnost ali homogenost varianc


### 2.2 REGRESIJSKA DIAGNOSTIKA (Multipla regresija: diagnostika)

 ## DIAGNOSTIčNE SLIKE
plot(model2)
  # Funkcija izdela 4 diagnostiČne slike, za vsako sliko, ki jo želimo videti, moramo posebej pritisniti enter.
  # Dobimo graf ostankov ali rezidualni graf, ...

  # GRAF OSTANKOV (rezidualni graf)
  # Na abscisi imamo napovedane vrednosti in na ordinati imamo reziduale. Tisti, ki so zgoraj, imajo visoke ostanke,
  # so tisti, ki so bolj uspešni, kot smo jim napovedali; tisti, ki so spodaj pa so manj uspešni, kot smo jim
  # napovedali.
  # RdeČa Črta je lowless Črta - zglajena regresijska Črta (Če predpostavka o linearnosti drži). V idealnem primeru
  # bi morala biti ves Čas niČ, morala bi biti identiČna Črtkani Črti - povpreČna napaka bi bila niČ.  če opazimo
  # kakšen vzorec (zlasti kakšen izrazit nelinearen vzorec), gre za odstopanje od linearnosti, pomeni torej kršenje
  # linearnosti.
  # Na sredini je krivulja zelo blizu niČ ... na obeh straneh pa zavije se dvigne, kar bi lahko sumili kot odstopanje
  # od linearnosti. če bolje pogledamo, vidimo, da na desni samo tista ena oseba potegne Črto navzgor - imamo eno samo
  # osebo, ki zelo iztopa, napovedali smo ji visoko uspešnost, vendar je bila še bolj uspešna. Le ta oseba potegne
  # Črto navzgor. Enako je na drugi strani, imamo skupinico oseb, ki smo jim napovedali nizko uspešnost, pa niso bili
  # tako zelo neuspešni, so bili bolj uspešni, kot smo jim napovedali.
  # Navidezna nelinearnost je posledica le majhnega števila oseb (na repih). Tam, kjer je veliko oseb, Črta kaže na
  # lineranost. 
  # Iz tega lahko zakljuČimo, da predpostavka linearnosti ni kršena. Ni sistematiČnega odnosa med napovedanimi
  # vrednostmi in reziduali.

  # Poleg linearnosti lahko vidimo osebe, ki izrazito odstopajo - tiste s številko zraven.
  # (osebe z veliko napako napovedi - lahko z osebo nekaj narobe ali pa oseba ne sodi v naš vzorec)
  # Pregled ostankov je koristen. VČasih je dobro pogledati, kdo so ti ljudje, ki tako izstopajo, pri katerih smo
  # najbolj zgrešili z napovedjo (torej tisti, poleg katerih izpiše cifro).

  
  # Ali je razpršenost enaka vzdolž celotenga razpona napovedanih vrednosti? Ali je razpršenost napak enaka skozi
  # celotno širino tega oblaka?
  # Približno je. Zdi se, da toČke povsod približno enako odstopajo, ne izgleda, da bi imeli odstopanje od
  # predpostavke homogenosti varianc oz. homoscedastiČnosti. VeČja koncentracija oseb je blizu niČle kot pa od nje.

  # O normalnosti težko kaj reČemo ... 

  # NORMALNI KVANTILNI GRAF OSTANKOV (QQ-plot)
  # Tam, kjer je toČk najveČ, je odstopanje od normalnosti najmanjše ... je pa veČje odstopanje od normalnosti na
  # repih. Ni tako veliko, da bi morali uporabiti drugaČen test.
  # če smo v dvomu, ali je odstopanje od normalnosti veliko ali ne, je morda sliselno, da primerjamo IZ, ki jih dobimo
  # s parametriČnimi postopki, s tistimi IZ, ki jih dobimo z zankanjem (to smo že naredili, ni veČjih razlik).


### 5. vaja (4. 11. 2019)

  # GRAF ODNOSA MED NAPOVEDANO VREDNOSTJO in VARIABILNOSTJO (scale location plot)
  # Slika je grafiČni prikaz homogenosti varianc.
  # Na ordinati so ostanki (ostanke standardiziramo in nato korenimo, da dobimo absolutne vrednosti - zanima nas
  # samo velikost razlike, zanima nas samo, kako velika je bila razlika).
   
  # če je povpreČni absolutni ostanek povsod približno enak, predpostavka o homogenosti varianc velja.
  # Odstopanje je zelo majhno, ni izrazite razlike. Slika nam ne kaže izrazitega odstopanje od predpostavke.

  # GRAF VPLIVNOSTI OSEB
  # Ta slika služi diagnostiki vplivnosti. Lahko poskušamo najti osebe, ki imajo rezmeroma veliko vplivnost.
  # Na abscisi imamo roČico - oddaljenost od povpreČja vseh napovedi, na ordinati pa standardizirane ostanke (ne absolutne
  # vrednosti!).
  # VeČja kot je (roČica?) veČji bo vpliv neke osebe na regresijske parametre.
  # Vzemimo/imamo dve osebi, ena je Čisto na vrhu, ena pa Čisto desno spodaj. Glejmo povpreČje napovednika,
  # osebi sta enako oddaljeni od povpreČja napovednika. Imata enako roČico, vendar razliČno napakako napovedi. Spodanja
  # oseba ima visoko napako napovedi - napovdali smo višji rezultat, kot ga dejasnko ima. Rgeresijsko premico bo povlekla
  # navzdol. Je daleČ od povpreČja napovenika in ima veliko napako napovedi, zato je njena vplivnost velika. 
  # S Črtkano Črto je oznaČeno, od kje naprej je Cookova razdalja razmeroma velika. Glede na to, da na sliki ni oznaČene
  # Cookove razdalje, ni nobena oseba tako izsotpajoČa.
  # ProblematiČne so osebe, ki ležijo desno zgoraj in spodaj, ker so najbolj oddaljene in imajo najveČjo napako napovedi.
  # K vplivnosti se bomo vrnili kasneje.

  # Da jih vse narišemo v eno samo panelno sliko.
par(mfrow = c(2,2))
plot(model2)
  # Potem pa ne smemo pozabiti naštimat nazaj tako, da nam pokaže samo eno sliko.
par(mfrow = c(1, 1))

 ## Preverjanje postavk preko pogleda slike je subjektivno. Lahko si pomagamo s testi za preverjanje nosrmalnosti ostankov
  # (shapiro wilkov test, koeficienti splošČenosti in simetrije), za preverjanje homogenosti varianc pa lahko uporabimo
  # Brusch-Paganov test.

 ## BREUSCH-PAGANOV PREIZKUS
  # Testira veljavnost predpostavke enakosti varianc (homoscedastiČnosti).
  # Ker ta preizkus predpostavlja, da se varianca ostankov enakomerno poveČuje ali zmanjšuje vzdolž napovedanih vrednosti,
  # ne more povsem nadomestiti pregleda grafa ostankov (veČ razlage spodaj ...).
car:: ncvTest(model2)
  # hi-kvadrat (1) = 0,165; p > 0,05
  # Test ni statistiČno znaČilen. Stopnja heteroscedastiČnosti ni niČ veČja, kot bi jo priČakovali.

  # Test ima omejitev: zazna le tiste tipe heteroscedastiČnosti, kjer varianca napak narašČa le v eni smeri.
  # Metuljasta heteroscedastiČnost (pri srednjih vrednostih napovedovanje bolj natanČno) - tega ne bo zazanal. Ali pa Če
  # je najveČja varianca na sredi, tega ta test ne bi zaznal.
  # Nujno je treba pogledati sliko!!! Test torej ne bo zaznal, Če je porazdelitev metuljasta (to je tisto, ko imaš 
  # "2 skupini").

  #  |\_/|      ta je
  #  |/ \|     metuljasta

  #  |\      tako trikotno
  #  |/         zazna


 ## Napake napovedi oz. ostanki
  # Ali so približno normalno porazdeljeni in ali imamo osebe z zelo velikmi napakami napovedi?
  # velike napake napovedi lahko kažejo na težave v analizi - ali linerani model v osnovi ni ustrezen, lahko smo izpustili
  # kakšen pomebne napovednik, ali nismo ustrezno vzorČili, lahko pa tudi kaže na motnje v procesu zbiranja podatkov.
  # Lahko kaže na (na kratko):
  #   - lin. model neustrezen
  #   - izpušČen napovednik
  #   - vzorČenje
  #   - motnje pri zbiranju podatkov

  # IzraČunamo in prikažemo lahko tudi STUDENTIZIRANE OSTANKE, ki nam povedo, pri katerih osebah so bile napake napovedi
  # najveČje.
  # Lahko izraČunamo razlike med dejanskimi in napovedanimi vrednostmi. Vzamemo studentizirane ostanke -> odklone delimo z
  # njihovimi standarnimi napakami - porazdeljuje po t porazdelitvi.
so <- rstudent(model2)
  # studentizirani ostanki
  # Objekt "so" je kot nek vektor, za vsako osebo imamo vrednost studentiziranega ostanka.
hist(so)
  # Histogram bi izgledal takole.
psych:: describe(so)
  # PovpreČje ostankov je vedno niČ (0).
  # Zanimiva sta koeficienta simetriČnosti (praktiČno niČ, zelo simetriČna porazdelitev) in splošČenosti (rahlo negativen, 
  # malo splošČena porazdelitev - manj je velikih napak, napake napovedi so v nekih piČakovanih mejah, nimamo nobenih
  # ekstremnih napak, oseb, pri katerih smo zelo zgrešili, je manj, kot smo priČakovali; Če bi bile prisotne tudi ekstremne
  # napake, bi bila porazdelitev bolj splošČena in imela debelejše repe).
  # Odstopanje od normalnosti gre na raČun negativne splošČenosti.
  # Pomembna sta tudi min in max - najvišja in najnižja vrednost studentiziranih napak sta v okviru priČakovanih (kriterij, 
  # katere napake so nenavadno visoke, je +/- 3 -> mi smo precej oddaljeni od tega).
  # če bi nas zanimalo, kdo ima najnižjo/najvišjo vrednost:
which.min(so)
  # 9. oseba, pri njej smo najveČ zgrešili, napaka še vedno ni zelo velika.

 ## VPLIVNOST
  # Iskanje vplivnih oseb - oseb, ki imajo nesorazmeren vpliv na regresijske parametre.

  # VeČ mer vplivnosti dobimo s funkcijo im().
im <- influence.measures(model2)
  # Mere vplivnossti za potencialno vplivne osebe lahko prikažemo v tabeli:
summary(im)
  # Izpiše osebe, ki so potencialno najbolj vplivne.
  # Kaj vse R izpiše, kaj so vse te mere?
  # Prvi 4 stolpci (dfbetas) so standardizirane spremembe regresijskih nagibov. To niso dejanske spremembe, ampak
  # standardizirane tiste, ki so nad 2 oz. pod -2, so vplivne, pomenijo problematiČne spremembe.
  # Predzadnji stolpec je Cookova razdalja.
  # dffit

  # Kaj bi se zgodilo, Če bi osebo izloČili?
model2_brez9 <- lm(DU ~ TN + PRI + VZT, data = x[-9])
coefficients(model2_brez9)
coefficients(model2)

  # //Razlika pomeni, kakšen je vpliv te devete osebe.
  # Regresijska napaka bi se zmanjšala, tudi nagibi, nagib za prijaznosti bi se zmanjšal, za vztrajnost pa povišal.
  # za osebo ne moremo reČi, da je njen vpliv zanemarljiv, ni pa tak, da bi poruČil razmerja med napovedniki.// Manca

  # DramatiČnih sprememb izkljuČitev te osebe ne bi prinesla.
  # Od Česa je še odvisna vplivnost neke osebe (poleg velikosti napake napovedi)? Velikost vzorca.
  # Če imamo milijon ljudi, imamo lahko eno zelo ekstremno osebo, pa ne vpliva ravno na parametre, pri nekaj deset osebah
  # pa ena oseba z ekstremnimi vrednostmi vpliva na vrednosti parametrov.
  # veČinoma imamo manjhne vzorce pri kliniČnih populacijah, prav tako so taki vzorci zelo heterogeni.

  # Da identificiramo vplivne osebe, lahko narišemo okvir z roČaji (boxplot).
  # Matrika im$infmat vsebuje mere vplivnosti. če jih želimo prikazati, matriko spremenimo v podatkovni okvir.
mere_vplivnosti <- as.data.frame(im$infmat)
  # Zanimivo je narisati npr. Cookove razdalje.
Boxplot(mere_vplivnosti$cook.d)
  # Vidimo, da deveta oseba zelo iztopa, vendar njena izkljuČitev ne bi veliko spremenila rezultatov.
  # če bi bistveno spremenila interperaticijo rezultatov, potem imamo težavo. PoroČamo o vplivni toČki/osebi - Če jo
  # vkljuČimo, potem bi bili rezultati takšni in takšni. -> Lahko uporabimo robustne metode.

  # GrafiČen prikaz vplivnosti in njenih komponent dobimo s:
car:: influencePlot(model2)
  # Na abscisi so prikazane ÃÂ»roČiceÃÂ« (oddaljenost osebe od povpreČja napovednikov), na ordinati pa studentizirani ostanki.
  # Velikost mehurČka ustreza Cookovi razdalji. Potencialno vplivne osebe so oznaČene s številkami.

  # VeČji kot je krog, veČja je vplivnost neke osebe.
  # Levo so tisti, ki imajo vrednosti bližje napovedanim. ObmoČje, kjer je vplivnost zelo majhna (majhni krogci) - smo jim
  # precej toČno napovedali vrednost (osebe niso vplivne).
  # velika napaka napovedi, so zelo daleČ in imajo iztopajoČe vrednosti napovednika (vplivnost take osebe je visoka).

  # V realnosti regresijsko diagnostiko izvedemo Čim prej, pred interpretacijo in napovedovanjem.


### 2.3 REGRESIJA S KATEGORIALNIMI NAPOVEDNIKI

k <- read.table("mr2.txt", header = T)

  # Kaj nam povedo podatki?

  # Za skupino otrok in njihovih mater imamo naslednje podatke:
  #   - govor: govorni kod (višje vrednosti -> prevladujoČ razširjeni kod; nižje vrednosti -> prevladujoČ omejeni kod);
  #            to je dimanzionalna spremenljivka (imamo dve skrajnosti)
  #   - otrokova starost v mesecih
  #   - slog: vzgojni slog staršev (1 = avtoritativni, 2 = permisivni, 3 = avtoritarni); nominalna spremenljivka
  #   - pismen: materin dosežek na testu funkcionalne pismenosti (preverjal je, kako dobro oseba razume besedila in
  #             razumevanja navodil, obrazcev ...)

  # Zanima nas, v kolikšni meri lahko govorni kod pojasnimo z vzgojnim slogom in s pismenostjo. Regresijsko analizo
  # torej uporabimo kot pojasnjevalno metodo. Vrstni red vkljuČevanja napovednikov izhaja iz raziskovalnih hipotez!

  # Zanima nas torej, koliko variance govornega koda lahko pojasnimo z materinim vzgojnim slogom ter koliko lahko
  # napovedno moČ poveČamo z vkljuČitvijo pismenosti - koliko variance lahko dodatno pojsnimo z materino pismenostjo. 
  # Napovednike vkljuČujemo postopoma.
  # Ne zanimajo nas napovedane vrednosti porameznih mater - modela ne bomo uporabili za napovedovanje. 

  # Dajmo najprej pogledati opisne statistike.
describe(k[,-3])
  # Izpustimo 3. stolpec, ker je spremenljivka nominalna.
  # PovpreČen dosežek na testu pismenosti je okoli 30 s SD = 5.
table(k$slog)
  # Koliko mater spada v kateri vzgojni slog.

lm(govor ~ slog, data = k)
  # Iz tega bi dejansko nekaj dobili, a NI ok!!! Ker je slog nominalna spremenljivka - kateri slog ima katero številko
  # je povsem arbitrarno. 
  # Slog je nominalna spremenljivka, tako da to ne bi imelo smisla. Rezultati bi bili brez vrednosti.

  # Ali lahko v napovednik vkljuČimo spol? Imamo kakšen zadržek? V bistvu ga ni. Dihotomno spremenljivko lahko vedno
  # vljuČimo kot napovednik. Lažje je interpretirati regresijski nagib.
  # Recimo, da imamo spol kot napovednik in ga kodiramo z 0 in 1. Kaj nam bo povedal regresijski nagib? Povedal nam bo
  # napovedano razliko (razlika v odvisni spremenljivki) za moškega in žensko, Če imata ostale rezultate/dosežke enake
  # Alternativa za nedovisne t vzorce.

  # V reg. modelu imamo lahko dihotomno spremenljivko in nagib je razlika med povpreČjema obeh skupin ... Če imamo še
  # druge spremenljivke, pa je malo drugaČe.

  # Ena od rešitev je lahko, da nominalno/ordinalno spremenljivko predelamo v skupino dvojiških spremenljivk.
  # Potrebujemo dve spremeljivki, ki sta dihotomni - reČemo jim indikatorske ali slepe spremeljivke (ang. indicator ali
  # dummy variable). 
  # Imamo slog, ki je kodiran z 1, 2 in 3. Prvo indikatorsko spr. naredimo tako, da bo loČevala premisivne matere od
  # vseh ostalih - permisivne bi dobile 1. Drugo pa, da bo loČevala avtoritarne od ostalih. Za vsako matero sedaj vemo,
  # kateri slog ima. Z dvema indikatroskima spremeljivkama popolnoma nadomestimo nominalno spr. Vedno potrebujemo eno
  # indikatorsko spremenljivko (2) manj kot nominalno (3).
  # če bi imeli še eno indikatorsko spremenljivko, bi bila popolnoma korelirana z ostalima napovednikoma, in ne bi mogli
  # izraČunati parametrov.** 

  # tabelca

  #     slog         |  i1   i2   (i3 - ni tega stolpca**)
  # avtoritativna  1 |  0    1
  # permisivna     2 |  0    0
  # avtoritarna    3 |  1    0

  # R indikatorkse spremeljivke naredi sam. Samo napisati moramo, da je spremenljivka nominalna.
k$slog <- as.factor(k$slog)
levels(k$slog) <- c("avtoritativen", "permisiven", "avtoritaren")
  # Sedaj lahko izvedemo analizo.

model1_kat <- lm(govor ~ slog, data = k)
summary(model1_kat)
  # N = 99
  # Test statistiČne znaČilnost R^2.
  # F (2, 96) = 5,93; p = 0,0004
  # NiČelno hipotezo, da materin slog vzgoje ni povezan z govornim kodom, zavrnemo.
  # Ta F test je enak enosmerni analizi variance. 
  # V vzorcu pojasnimo 11 % variance.
  # Nepristranska ocena koeficienta determinacije je 10 %. Ne smemo sklepati o vzorČnosti!!!

  # Interpretacija koeficientov
  # Konstanta je 7 in še malo, kaj nam to pove? (da je y = 7, ko je x = 0... kaj pa Če smo malo bolj konkretni?)
  # Avtoritativnim materam bomo napovedali vrednost 7, 7 toČk, ker imajo vsoto niČ na vseh indikatorskih
  # spremenljivkah ... povpreČje tiste skupine, ki ima vrednost niČ na vseh napovednikih (naša referenČna skupina).
  # Avtoritativne matere imajo v povpreČju dosežek 7 toČk.
  # V kakšnem primeru imamo matere, ki se na prvi indikatorski spr. spremenijo za eno toČko, na drugi pa imajo enaka
  # rezultata - kadar priemrjamo avtoritativno in permisivno matero.
  # Permisivne mame imajo v povpreČju za 1,16 toČke nižji rezultat (govorni kod) v primerjavi z avtoritativnimi materami.
  # drugi nagib: na i1 obe niČ, na i 2 se razlikujet za eno toČko. avoritarne matere imajo za 0,7 toČke nižji govorni kod. 

  # Nagibi so razlike med napovedanimi vrednostmi med posamezno in referenČno skupino.
  # če imamo skupino, s katero primerjamo, ji reČemo referenČna skupina.
  # R prvo kategorijo vzame kot referenČno. vredost faktorja potem kodiramo kot vrednosti 1. 
  # Če bi slog kodirali drugaČe, bi SZ in moČ modela ostala enaka. nekateri naČini kodrianja so bolj prikladni kot drugi. 

  # Kaj Če slogu dodamo še pismenost?
model2_kat <- lm(govor ~ slog + pismen, data = k)
summary(model2_kat)
summary(model2_kat)$r.squared - summary(model1_kat)$r.squared
  # ali je razlika stat. znaČilna je treba posebej preveriti
anova(model1_kat, model2_kat)
  # Testiramo hipotezo, da oba modela pojasnita enako koliČino variance.
  # F(1, 95) = 7,08; p = 0,009
  # NiČelno hipotezo zavrnemo, vkljuČitev pismenosti statistiČno znaČilno poveČa pojasnjeno varianco.
  # Pojasnimo del govornega koda, ki ga ne moremo pojasniti z vzgonjim slogom - vkljuČitev pismenost je smiselna. 

  # Kaj nam pove nagib za pismenost? Imamo razliko v napovedname govornem kodu za 2 materu ku se na _???__ razlikujeta na
  # in imata pri tem enak dosežek na 
  # Dve materi, ki se ne razlikujemet NA PISMENOsti je pa ena permisivna in ena avtoritativna, bomo permisivni
  # napovedali za 0,935 toČke nižji govorni kod'???

  # Kaj pa konstanta?
  # Mesmiselno se je spraševati, kakšem rezultat bi napovedali materi, ki ima na pismenosti rezultat niČ (sploh
  # ker take osebe nimamo v našem vzorcu). Lahko bi pa vrednosti prej centrirali, da bi bilo bolj interpretabilno ...
  # od vseh vrednosti bi odšteli povpreČno vrednost ....??????......

  # Manca
  # nagib za pismenost nam pove, da razlika med napovedanem govornem kodu za materi, ki se na testu sloga razlikueta za eno
  # toČko.
  # poveČala za 0,08
  # konstanta: za mam, ki je avtoritatvina ima na testu pismenosti 0. Nesmislno napovedati nakaj materi, ki je dosegla niČ
  # toČk.
  # da bi bilo preseČišČe malo bolj interpetabilno: preseČišČe centriramo, odštejemo M in bi dobili pismenost 0.
  # indikatorske spr. nastopajo v kompletu. vse vkljuČimo ali vse izkljuČimo. vkljČitev vseh hkrati pomembno zviša pjasnjeno
  # variacno? to gledamo.

# ----------------------------------------------------------------------------------------------------------------------
  # DN : najprej poglej, kaj se zgodi, Če centriramo pismenost, kako se rezultati spremeijo, kaj
  # se spremeni in kako zdaj to kar se spremeni interpretiramo, 2. stvar pa je kaj bi se in kaj sene bi spremenilo,
  # Če bi v prvem koraku vkljuČili pismenost in v drugem koraku vzgojni slog.


### DomaČa naloga do 11. novembra:
  # Za primer z vzgojnim slogom in pismenostjo preverite, kako se spremenijo rezultati, Če:
  #   a) dosežek na testu pismenosti pred analizo centrirate;
  #   b) v model najprej vkljuČite pismenost in šele nato vzgojni slog.

  #   a) dosežek na testu pismenosti pred analizo centrirate



  #   b) v model najprej vkljuČite pismenost in šele nato vzgojni slog.

model1 <- lm(govor ~ pismen, data = k)
summary(model1)

k$slog <- as.factor(k$slog)
levels(k$slog) <- c("avtoritativen", "permisiven", "avtoritaren")

model2 <- lm(govor ~ pismen + slog, data = k)
summary(model2)
summary(model2)$r.squared - summary(model1)$r.squared
  # ali je razlika stat. znaČilna je treba posebej preveriti
anova(model1, model2)


### 6. Vaja (11. 11. 2019)

  # DN

model1a <- lm(govor ~ slog, data = k)
model1b <- lm(govor ~ pismen, data = k)
model2 <- lm(govor ~ slog + pismen, data = k)

summary(model2)
  # še manjka....

  # Kaj se spremeni? Konstanta. Kakšna je razlika? PrejšnjiČ smo konstanto interpretirali, kot dosežek neke matere,
  # ki ima na pismenosti dosežek niČ. Ni kaj dosti uporabno. Sedaj pa imamo konstanto, ki jo je lažje interpretirati.
  # Konstanto interpretiramokot dosežek neke matere, ki ima na pismenosti povpreČen dosežek.

  # Kaj pa 2. naloga? Kaj se spremeni? Bistveno je, da nam je hotel pokazati, zakaj je vrstni red pomemben.
  # manjša statistiČna znaČilnost.
  # razlika med pojasnjeni varianci bi bila drugaČna. za koliko poveČamo pojansnjeno varianco bi bila drugaČna. 
# ----------------------------------------------------------------------------------------------------------------------


### pa nadaljujmo

### 2.4 NELINEARNI ODNOSI

  # //ali je materin govor odvisen tudi od znaČilnosti otroka, od starosti otroka
  # vprašanje ali matera na enak naČn govorijo z otroki enake starosti ali ne//

describe(k[,1:2])
  # najstarejši so bili stari 10 let. pomembno je vedeti kakšen je razpon podatkov.
cor(k$govor, k$starost)
  # //korelacija je skoraj niČ. ni statistiČno znaČilna
  # naredili bi lahko lienaren model, ki bi namesto premice modeliral odnos med govorom matere in otrokovo strastjo//

model_1 <- lm(govor ~ starost, data = k)
summary(model_1)
  # //regresijski nagib za starost je negativen, vendar niso SZ od niČ. na osnovi modela ne moremo zakljuČiti, da je
  # starost linerno povezana z materinim govorom.//
  # Regresijske slike:
plot(model_1)
  # 1. slika: Zelo oČitno je, da odnos ni linearen (cel korelacijski oblak ima tako malo klobasasto obliko), zdi se,
  # da je res
  # ena taka nelinearna krvulja. Linearna povezanost je skoraj niČelna, nelinearna pa ne.
  # Naslednja slika: ... do enake slike bi prišli s funkcijo sp() iz paketa car.
car:: sp(k$govor ~ k$starost)
  # S tem smo pogledali odnos med govornim kodom in otrokovo starostjo.
  # nelinearen odnos
  # kako se lahko pri analizi podatki spospadamo z nelineanimi odnosi? NaČeloma imamo 3 možnosti:
  #   1. še vedno uporabimo linearni model (na zunaj izgleda, kot da imamo še vedno linearni model), ki pa bo uporabil
  #      nelinearno funkcijo napovednika. nelinearen v napovednikih -> najelegantnejša rešitev.
  #      ([y s strešico zgoraj]Y = a + b1X +b2 * f(X)) 
  #   2. posplošen linearni model (ko s pretvorbo napovednika ne moremo rešiti težave)
  #   3. intrinziČno nelinearen model (odnosa ne moremo spraviti v linearno obliko) -> takim modelom se izogibamo
  #      ([y s strešico zgoraj]Y = (aX) / (bZ))

 ## poskuili bomo s kvadratno funkcijo - zdi se, da bi odnos lahko odpisali s kvadratno krivuljo
  # linearni model: [y s strešico zgoraj]Y = a + b1X +b2 * X^2
  #        [govor s strešico zgoraj] govor = a + b1*S +b2*S^2 (S ... starost)

model_k <- lm(govor ~ starost + I(starost**2), data = k)
  # zdaj lahko ocenimo parametre.
  # Vedno kadar dodajamo nove napovednike v model je potrebno preveriti ali se je pojasnjena varianca pomembno zvišala

  # linearni in kvadratni model primerjamo s funkcijo anova
anova(model_1, model_k)
  # F(1, 96) = 8,13; p = 0,005
  # Če bi bila oba modela v populaciji enako dobro, Če bi oba enako dobro pojasnjevala materin govor, bi samo v 
  # _% dobili _???_

  # Razlika med modeloma je statistiČno znaČilna. Nelinearni ima veČjo napovedno moČ od linearnega.
summary(model_k)
  # Z nelinearnim modelom smo pojasnili manj kot 1 % variance, regresijski nagib j ebil pa praktiČno niČ.
  # V nelinearnem modelu pa je _???_ razlika je skoraj 8 %.

  # R^2 = 0,085; R^2(pribl.) = 0,066
  # F(2, 96) = 4,96; p = 0,014

  # Pri interpretaciji moramo biti pazljivi. Upoštevati bi morali, da nismo imeli longitudinalnega pristopa, imeli smo
  # razliČno stare otroke ... morlai bi imeli longitudinalni naČrt. To zdaj moramo jemati bolj kot hipotezo, kaj se
  # dogaja skozi Čas.

  # da ima kvadrat starosti negativen predznak ... imamo obrnjeno u-krivuljo ... ta ima maksimum (lahko gledaš graf
  # funkcije sp())

  # Oba nagiba (starosti in I) sta bistveno veČja (ker sta kvadrirana), oba nagiba sta statistiČno znaČilna. Oba
  # moramo ohraniti (pomambno prispevata k pojasnjeni varianci)

  # Standardna interpretacija regresijskega nagiba tu ne pride v poštev.
  # Kako pa je z interpretacijo konstante? Na prvi pogled ima smiselno interpretacijo, konstanta pove, kakšna je
  # napovedana ocena govora za mamo z novorojenČkom. Ampak ... zakaj tega ne bi zapisali, na glas izgovorili ...
  # zakaj je to formalno sicer res, praktiČno gledano pa ne ... nimamo nobene mame z novorojenČkom v vzorcu, najmlajši
  # so stari eno leto. 
  # _???_ neupraviČeno bi bilo na osnovi predšolskih otrok sklepati, kako se matere pogovarjajo z novorojenČki

  # Če bi hoteli imeti interpretabilno konstanto, bi morali zopet centrirati starost ali pa _????_

  # Zavedati se moramo, da odnosi niso nujno vedno linearni, _????_ in paziti moramo pri intarpretaciji
  # in se vzdržati ekstrapolacije

plot(model_k)
  # //s kvadratnim modelom dobro opišemo odnos med govornim kodom matere in starostjo otroka.
  # zadnja slika  - Cookove razdalje. nekaj toČk, ki so bilj vplivne kot v prejšnjih primerih. 
  # stabilnost modela je malce slabša kot v prejšnjih dveh modelih. 
  # rešili smo problem nelineranosti. 
  # kvadratna funkcija vsebuje tako linearni kot kvadratni Člen. dobro je imeti v modelu oboje.//


############################################## Čez 2 tedna kolokvij #################################################



### Nova zadeva (nova skripta) 3_logistiČna regresija
