#**************************************************************************
# NRP80-Study1
# Corrections for Witzig
# August 2025
# Laurenz Meier
#***************************************************************************

#***********************************************************************
# 00 Pre - Mise en place ----
#***********************************************************************

# . Clean up & packages  ----
pacman::p_load(rstudioapi,readr,dplyr,tidyr,epiDisplay,psych)

rm(list=ls())
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(scipen=999,max.print = 9999)
cat("\014") 

# . Data import ----
setwd(dirname(getActiveDocumentContext()$path))
data <- read_csv("NRP80-Study1.csv") #load from SwitchDrive (01- Study 1 > 05 - Media Outlet > Analyses). Dataset is not available on GitHub (public repository)

data.wfh <- data %>% filter(WFHmog == 2)


#***********************************************************************
# 01  WITZIG booklet ----
#***********************************************************************

# . p 6 ----

# Text:
# Von den Arbeitnehmenden, die die Möglichkeit haben im Homeoffice zu arbeiten,
# äusserten viele der Befragten eine klare Präferenz für eine begrenzte Anzahl von Homeoffice-Tagen,
# welche entweder einem Tag (20 %) oder höchstens zwei Tagen (26 %) Homeoffice pro
# Woche entsprach. Interessanterweise gaben jedoch auch 13 % der Arbeitnehmenden an,
# überhaupt nicht im Homeoffice arbeiten zu wollen, während andere einen häufigeren
# Rhythmus – von drei Tagen (13 %) bis hin zu einer ganzen Woche (2 %) – bevorzugten.

# Variable: WFHwishr - Wie viele Tage pro Woche würden Sie im Homeoffice arbeiten wollen?
# Data: Participants who can work from home

# Original response format
tab1(data.wfh$WFHwishr)

# Recode WFHwishr into the text's categories
data.wfh$WFHwishr_cat <- dplyr::case_when(
  data.wfh$WFHwishr %in% c(0.5, 1) ~ "1",
  data.wfh$WFHwishr %in% c(1.5, 2) ~ "2",
  data.wfh$WFHwishr == 0           ~ "0",
  data.wfh$WFHwishr == 3           ~ "3",
  data.wfh$WFHwishr %in% c(5, 6, 7)~ "5+",
  !is.na(data.wfh$WFHwishr) ~ "Other"  # All other non-missing values
)
tab1(data.wfh$WFHwishr_cat)

# ..............................

# Text:
# Ein Grossteil der Befragten gab an, entweder gar nicht (21 %) oder nicht mehr als
# einmal pro Woche (21 %) im Homeoffice tätig zu sein – oft weniger als das, was ihre
# Arbeitgeber*innen erlauben.

# Variable: WFHday_rr	- An wie vielen Tagen pro Woche arbeiten Sie üblicherweise im Homeoffice?
# Data: Participants who can work from home

# Original response format
tab1(data.wfh$WFHday_rr)

# Recode WFHday into the text's categories
data.wfh$WFHday_cat <- dplyr::case_when(
  data.wfh$WFHday_rr %in% c(0.5, 1) ~ "1",
  data.wfh$WFHday_rr == 0           ~ "0",
  !is.na(data.wfh$WFHday_rr) ~ "Other"  # All other non-missing values
)
tab1(data.wfh$WFHday_cat)

# ..............................

# Text:
# Die Entscheidungsfindung zu Homeoffice-Vereinbarungen erfolgt dabei überwiegend kollaborativ,
# wobei viele Arbeitnehmende (29 %) von gemeinsamen Entscheidungen mit ihren Teams oder Vorgesetzten
# berichten. Zentrale Vorgaben durch die Organisation sind her weniger verbreitet (27 %),
# während 24 % der Arbeitnehmenden ihre Homeoffice-Regelungen selber festlegen können und nur
# 18 % den Vorgaben ihrer Vorgesetzten Folge leisten müssen.

# Variable: WFHdec	Wer trifft die Entscheidungen über Ihre Homeoffice-Vereinbarung (z.B. ob, wie häufig, oder an welchen Tagen Sie die Möglichkeit haben im Homeoffice zu arbeiten)?
# Data: All participants

data$WFHdec <- if_else(data$WFHdec == 0, NA_real_, data$WFHdec)

tab1(data$WFHdec)


# ..............................

# Text:
# Was den gewählten Arbeitsort betrifft, wenn man ausserhalb des traditionellen Büros
# arbeitet, so entscheidet sich die überwiegende Mehrheit (79 %) tatsächlich dafür, von zu Hause aus zu arbeiten.
# Dennoch ist das Arbeiten im Homeoffice nicht auf das eigene Zuhause beschränkt. Eine kleine, aber nennenswerte
# Gruppe von Arbeitnehmenden zieht öffentlichere oder dynamische Umgebungen vor:
# Drei Prozent (3 %) entscheiden sich für Cafés, während 2 % gemeinsame Co-Working-Spaces nutzen.
# Darüber hinaus gaben viele der Befragten an,
                                                                  

# Variables:  WFHloc1 - WFHloc4
# Data: Participants who can work from home

tab1(data.wfh$WFHloc1) #home office
tab1(data.wfh$WFHloc2) #café
tab1(data.wfh$WFHloc3) #shared
tab1(data.wfh$WFHloc4) #Varia


# . p 7 ----

# Text:
# Für viele sind Produktivität und Flexibilität grosse Anreize für die Arbeit im Homeoffice:
# Achtzig Prozent (80 %) der Befragten nannten die Abwesenheit von Pendelzeiten als einen wichtigen Vorteil,
# während 56 % die Flexibilität bei der Arbeitszeitgestaltung schätzten. Zudem geben 59 % der befragten Arbeitnehmenden an,
# im Homeoffice eine ruhigere Umgebung zu haben, welche ihnen wiederum beispielsweise bessere Konzentration und
# gesteigerte Produktivität ermöglicht (45 %).

# Variable: WFHadv1-8 - Bitte geben Sie an, welche der folgenden Aspekte Sie persönlich als Vorteile des Arbeitens im Homeoffice erachten.
# Data:  Participants who can work from home

tab1(data.wfh$WFHadv1) #Pendeln
tab1(data.wfh$WFHadv2) #Arbeitszeit
tab1(data.wfh$WFHadv5) #Ruhe
tab1(data.wfh$WFHadv8) #Konzentration


# Text:
# Doch das Arbeiten im Homeoffice ist nicht gänzlich ohne Herausforderungen:
# Zweiunddreissig Prozent (32 %) der Befragten äusserten den Wunsch nach besseren Arbeitsbedingungen
# im Homeoffice und nannten eine unzureichende Ausstattung als Nachteil.

# Variable: WFHauss - Ich wünsche mir, dass mein Arbeitgeber meinen Heimarbeitsplatz besser ausstattet (z.B. durch einen Bürostuhl oder eine IT Ausstattung).
# Data:  Participants who can work from home
tab1(data.wfh$WFHauss)

data.wfh$WFHauss_bin <- dplyr::case_when(
  data.wfh$WFHauss %in% c(1, 2, 3) ~ 0,
  data.wfh$WFHauss %in% c(4, 5)    ~ 1,
  TRUE                         ~ NA_real_  # Preserve missing values
)

tab1(data.wfh$WFHauss_bin)

# Text:
# Während 37 % ihre Ergonomie im Homeoffice als „gut“ und 33 % als „sehr gut“ bewerteten,
# empfanden erwähnenswerte 15 % ihre Setups dabei bestenfalls als unangenehm.

# Variable: WFerg1 - Ich wünsche mir, dass mein Arbeitgeber meinen Heimarbeitsplatz besser ausstattet (z.B. durch einen Bürostuhl oder eine IT Ausstattung).
# Data:  Participants who can work from home
tab1(data.wfh$WFHerg1)

data.wfh$WFHerg1_cat <- dplyr::case_when(
  data.wfh$WFHerg1 %in% c(1,2) ~ "unangenehm",
  data.wfh$WFHerg1 %in% c(3)  ~  ".",
  data.wfh$WFHerg1 %in% c(4)    ~ "gut",
  data.wfh$WFHerg1 %in% c(5)    ~ "sehr gut"
)
tab1(data.wfh$WFHerg1_cat)


# . p 9 ----

# Text: 
# Die meisten Befragten gaben an, die Entscheidungen ihrer Organisation zum Thema Homeoffice
# (d. h. Entscheidungen darüber, wer im Homeoffice arbeiten kann, wann und warum) als fair zu empfinden,
# wobei 32 % dieser Aussage zustimmten und 45 % sogar ihre volle Zustimmung gaben.
# Im Gegensatz dazu äusserten nur 11 % der Befragten eine gewisse Unzufriedenheit, und 13 % blieben neutral
# in Bezug auf die Fairness ihrer Homeoffice-Entscheidungen.

# Variable: JODfr - Ich erachte die Entscheidungen als gerecht (d.h. ob, wie oft, wann und warum ich im Homeoffice arbeite).
# Data: All participants, various NA (reason not clear as variable is not mentioned in the codebook)

tab1(data$JODfr)

data$JODfr_cat <- dplyr::case_when(
  data$JODfr %in% c(1,2) ~ "unzufrieden",
  data$JODfr %in% c(3)  ~  "neutral",
  data$JODfr %in% c(4)  ~ "Zustimmung",
  data$JODfr %in% c(5)  ~ "Volle Zustimmung"
)

tab1(data$JODfr_cat)

# ..............................

# Text:
# Die Mehrheit betrachtete die Entscheidungen dabei vorwiegend als fair, wenn sie auf individuellen Bedürfnissen basieren,
# wie etwa gesundheitlichen Aspekten oder pflegebezogenen Verpflichtungen (65 %), oder wenn sie gleichermassen für alle
# Arbeitnehmenden angewendet werden (30 %).

# Variable: Not clear how this values were calculated.


# . p 12 ----

# Text:
# Eine aktuelle Studie der Fachhochschule Nordwestschweiz und der Universität Neuenburg zu den Homeoffice-
# Gewohnheiten von über 2’300 Schweizer Arbeitnehmenden – darunter mehr als 800 Eltern 

# Variable : LIVING2 - Ich wohne ... mit meinem Kind/meinen Kindern
# Data: All participants
tab1(data$LIVING2)

# ..............................

# Text:
# Die Studienergebnisse zeigen, dass Eltern im Schnitt 1.5 Tage pro Woche im Homeoffice
# arbeiten, während Arbeitnehmende ohne Kinder auf 1.7 Tage kommen

# Variable: WFHday_rr	- An wie vielen Tagen pro Woche arbeiten Sie üblicherweise im Homeoffice?
# Data: Participants who can work from home

data.wfh %>%group_by(LIVING2) %>%
  summarise(
    n = n(),
    mean_WFH = mean(WFHday_rr, na.rm = TRUE),
    sd_WFH = sd(WFHday_rr, na.rm = TRUE))
t.test(WFHday_rr ~ LIVING2, data = data.wfh)

# ..............................

# Text:
# Dieser Unterschied bleibt bestehen, selbst wenn man die Anzahl der möglichen Homeoffice-Tage,
# das Arbeitspensum und verschiedene demografische Variablen berücksichtigt

# Variable: WFHday_rr	- An wie vielen Tagen pro Woche arbeiten Sie üblicherweise im Homeoffice?
# Data: Participants who can work from home

summary(lm(WFHday_rr ~ LIVING2 + Wpct + WFHday_or + gender + age, data = data.wfh))

# ..............................

# Text:
# Tatsächlich bewerten 20 % der Eltern ihre Arbeitsumgebung im Homeoffice hinsichtlich ungestörtem Arbeiten
# als eher schlecht bis durchschnittlich, während dies nur auf 13 % der „Nicht-Eltern“ zutrifft – trotz
# vergleichbarer Ausstattung und Ergonomie.


# Variables: WFHerg1 - hinsichtlich Ergonomie (Schreibtisch- und Stuhlhöhe, Bildschirmgrösse und –abstand etc.)? 
#            WFHerg2 - hinsichtlich Technik (Internet- und Computerleistung, elektronischer Zugriff auf Unterlagen/Informationen, Hardware wie Scanner, Drucker etc.)?
#            WFHerg3 - …hinsichtlich ungestörten Arbeitens (Lärm, nicht arbeitsbedingte Ablenkungen etc.)?
tab1(data.wfh$WFHerg3[data.wfh$LIVING2 == 1])
tab1(data.wfh$WFHerg3[data.wfh$LIVING2 == 0])

tab1(data.wfh$WFHerg1[data.wfh$LIVING2 == 1])
tab1(data.wfh$WFHerg1[data.wfh$LIVING2 == 0])

tab1(data.wfh$WFHerg2[data.wfh$LIVING2 == 1])
tab1(data.wfh$WFHerg2[data.wfh$LIVING2 == 0])

# ..............................

# Text:
# Interessanterweise äussern Eltern, trotz des Wunsches, weniger im Homeoffice zu arbeiten
# als „Nicht-Eltern“ (2.4 Tage), dennoch das Bedürfnis, einen bedeutenden Teil ihrer
# Arbeitszeit (2.1 Tage) im Homeoffice zu verbringen – mindestens zwei Tage pro Woche.

# Variable: WFHwishr
# Data: Participants who can work from home

describe(data.wfh$WFHwishr[data.wfh$LIVING2 == 1])
describe(data.wfh$WFHwishr[data.wfh$LIVING2 == 0])

tab1(data.wfh$WFHwishr[data.wfh$LIVING2 == 1])
tab1(data.wfh$WFHwishr[data.wfh$LIVING2 == 0])

# ..............................

# Text:
# Insgesamt gaben 18 % der Befragten dabei an,dass ihr Büroarbeitsplatz im Hinblick auf
# ungestörtes Arbeiten als schlecht bis sehr schlecht einzustufen sei.

# Variable: AGerg3 - …hinsichtlich ungestörten Arbeitens (Lärm, nicht arbeitsbedingte Ablenkungen etc.)?
# Data: All participants (side note: almost identical values for the WFH sub sample)
tab1(na.omit(data$AGerg3))


# Text:
# So berichten Eltern häufiger, dass die Möglichkeit, mehr Zeit mit Familie und Freunden zu verbringen,
# ein Vorteil des Homeoffice sei (36 % gegenüber 27 % bei „Nicht-Eltern“). Im Gegensatz schätzen
# „Nicht-Eltern“ häufiger die geringere Zeit, die sie morgens für das Zurechtmachen benötigen
# (46 % gegenüber 39 % bei Eltern). 

# Variables: WFHadv4 - Mehr Zeit für Freunde und Familie ; WFHadv3 - Weniger Aufwand, um mich für die Arbeit fertig zu machen
# Data: Participants who can work from home

tab1(data.wfh$WFHadv4[data.wfh$LIVING2 == 1])
tab1(data.wfh$WFHadv4[data.wfh$LIVING2 == 0])

tab1(data.wfh$WFHadv3[data.wfh$LIVING2 == 1])
tab1(data.wfh$WFHadv3[data.wfh$LIVING2 == 0])

# ..............................

# Text:
# Insgesamt gaben die Befragten an, dass die drei Hauptvorteile des Homeoffice das Wegfallen des Pendelns (63 %),
# die ruhigere Arbeitsumgebung (50 %) und die flexiblen Arbeitszeiten (44 %) sind. 

# Variable: WFHadv1-8 - Bitte geben Sie an, welche der folgenden Aspekte Sie persönlich als Vorteile des Arbeitens im Homeoffice erachten.
# Data: Participants who can work from home

adv_items <- dplyr::select(data.wfh, starts_with("WFHadv"))

adv_summary <- adv_items %>%
  summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(),
               names_to = c("Item", ".value"),
               names_sep = "_") %>% 
  arrange(desc(mean))

print(adv_summary)

rm(adv_items,adv_summary)

# . p 15 ----

# Text:
# Die Umfrage ergab, dass die durchschnittliche Pendelzeit von rund 30 Minuten pro Streck
# mit den historisch berichteten Werten des Bundesamtes für Statistik übereinstimmt. Differenziert betrachtet,
# gaben 27 % der Befragten dabei an, weniger als 30 Minuten pro Tag zu pendeln, 66 % pendelten zwischen
# 30 und 120 Minuten, und nur 7 % verbrachten mehr als 120 Minuten pro Tag mit ihrem Arbeitsweg.

# Variables: COMUhin - Wie viele Minuten pendeln Sie durchschnittlich pro Tag? -> Hinweg; COMUruc - Wie viele Minuten pendeln Sie durchschnittlich pro Tag? -> Hinweg
# Data: All participants
data$COMU <- (data$COMUhin + data$COMUruc)/2

data$COMU_tot <- data$COMUhin + data$COMUruc



data$COMU_cat <- dplyr::case_when(
  data$COMU_tot == 0                          ~ "0",
  data$COMU_tot > 0 & data$COMU_tot < 30      ~ "-30",
  data$COMU_tot >= 30 & data$COMU_tot <= 120  ~ "30-120",
  data$COMU_tot > 120                         ~ "120+",
  TRUE                                        ~ NA_character_  # optional: fallback für alles andere
)

tab1(data$COMU_cat)

# ..................

# Text:
# Arbeitnehmende mit der Möglichkeit, im Homeoffice zu arbeiten, berichteten über längere Pendelzeiten
# im Vergleich zu Arbeitnehmenden ohne diese Möglichkeit. Dieser Unterschied zeigte sich sowohl bei
# der Pendelzeit pro Weg als auch bei der Gesamtpendelzeit pro Woche.

# Variables: COMUhin - Wie viele Minuten pendeln Sie durchschnittlich pro Tag? -> Hinweg; COMUruc - Wie viele Minuten pendeln Sie durchschnittlich pro Tag? -> Hinweg
# Data: All participants

data$COMU_week <- data$COMU_tot * data$COMUdr

t.test(COMU ~ WFHmog, data = data) # Commute time per way
t.test(COMU_week ~ WFHmog, data = data) # Commute time per week (Time per day * Number of days)
                                        # ! I could not replicate the finding that the groups differ !

# ..................

# Text:
# Teilnehmende mit längeren Pendelwegen gaben an, Arbeitsstellen zu bevorzugen,
# welche ihnen die Möglichkeit zum Homeoffice bieten. Bei Arbeitnehmenden mit kürzeren Pendelwegen wurde
# dies als weniger wichtig bewertet

# Variables: crucial - Wie wichtig ist Ihnen bei der Suche nach einer neuen Arbeit die Möglichkeit im Homeoffice arbeiten zu können?
# Data: All participants

cor.test(data$crucial, data$COMU, use = "complete.obs", )

# ..................

# Text:
# Langstreckenpendler (Personen mit mehr als 120 Minuten Pendelzeit pro Tag) gaben an,
# etwa einen halben Tag mehr im Homeoffice zu arbeiten als Kurzstreckenpendler
# (Personen mit weniger als 30 Minuten Pendelzeit pro Tag). Darüber hinaus äusserten
# Langstreckenpendler einen stärkeren Wunsch, mehr im Homeoffice zu arbeiten:
# Im Durchschnitt gaben sie an, zweieinhalb Tage pro Woche im Homeoffice arbeiten zu wollen, verglichen mit etwa zwei
# Tagen bei Kurzstreckenpendlern.

# Variables: WFHday_rr - An wie vielen Tagen pro Woche arbeiten Sie üblicherweise im Homeoffice?
#            WFHwishr - Wie viele Tage pro Woche würden Sie im Homeoffice arbeiten wollen?
# Data: Participants who can work from home

data.wfh$COMU <- (data.wfh$COMUhin + data.wfh$COMUruc)/2

data.wfh$COMU_tot <- data.wfh$COMUhin + data.wfh$COMUruc


data.wfh$COMU_cat2 <- dplyr::case_when(
  data.wfh$COMU_tot > 0 & data.wfh$COMU_tot < 30  ~ "-30",
  data.wfh$COMU_tot > 120                         ~ "120+",
  TRUE ~ NA_character_  
)
tab1(data.wfh$COMU_cat2)

t.test(WFHday_rr ~ COMU_cat2, data = data.wfh, var.equal = FALSE)
t.test(WFHwishr  ~ COMU_cat2, data = data.wfh, var.equal = FALSE)

#***********************************************************************
# 02  WITZIG article ----
#***********************************************************************

# Text:
# Laut Homeoffice Lab arbeiten Mitarbeitende in der Schweiz aktuell im Schnitt 1,6 Tage pro Woche von zuhause,
# obwohl 2,4 Tage erlaubt wären. Gleichzeitig wünschen sie sich durchschnittlich 2,1 Tage im Homeoffice.

# Variables: WFHwishr, WFHday_or, WFHday_rr
# Data: Participants who can work from home
describe(data.wfh %>% dplyr::select(WFHwishr, WFHday_or, WFHday_rr))

# ..................

# Text:
# Obwohl Homeoffice zahlreiche Vorteile bietet – geringere Erschöpfung und höhere Zufriedenheit etwa -,
# gibt es auch Schattenseiten: 33 Prozent der Mitarbeitenden fühlen sich laut Studie zuhause einsamer. 

# Variable: LONE
# Data: Participants who can work from home
tab1(data.wfh$LONE)

