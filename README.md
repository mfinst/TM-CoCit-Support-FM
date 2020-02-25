# Guide on How to use the Library

* Deutsch
* English

## Deutsch

### Abhängigkeiten

R Statistics
* R Version 3.6.2+
* Empfohlen: RStudio Free Version
R Packages
* tm
* igraph
* devtools
* RWeka
* topicmodels (LDA Modell)
* Einen Internetzugang zu Web of Knowledge (www.webofknowledge.com)

### Export und Extraktion der Zitationen

Um die Funktionsweise des Pakets darzustellen wird anhand eine erweiterten Suchanfrage eine Extraktion durchgeführt.
Zunächst lädt man einen Datensatz aus dem Web of Knowledge herunter. In diesem Fall laden wir mithilfe einer erweiterten Suchanfrage eine Sammlung von Publikationen zum Thema Business Intelligence.

![](https://github.com/mfinst/TM-CoCit-Support-FM/images/Export.PNG "Exported Data")

Da das Thema relativ umfangreich behandelt wurde werden wir die Suchanfrage bis zum Jahr 2012 eingrenzen.

![](https://github.com/mfinst/TM-CoCit-Support-FM/images/Query.PNG "Exported Data")


Web of Knowledge speichert die exportierten Datensätze immer als savedrecs.txt.
Der Datensatz enthält zum Zeitpunkt dieser Beschreibung 46 Merkmale, wovon hauptsächlich die CR Spalte von bedeutung für uns ist.

Anschließend werden die Daten mit der Funktion read_in_WebOfKnowledge_export geladen. Die Funktion legt zusätzlich noch eine Spalte mit laufenden Indizes für die Publikationen an.
```R
extracted_data <- read_in_WebOfKnowledge_export(fileName = "pfad")
```
Nun sind die Daten in die aktuelle R Session geladen. Mit der Funktion rebuild_data_source_cocit können die Zitationen extrahiert werden.
Je nach Größe des Datensates kann dies länger dauern. 
Das Ergebnis der Funktion ist ein Dataframe mit allen verfügbaren Informationen zu den Zitationen.
Nun fehlt nur noch die Zuordnung der Zitationen unter den Publikationen. Es muss sichergestellt werden, dass gleiche Zitationen auch als solche erkennbar sind.
Die Funktion sucht anhand der vorhandenen Merkmale gleiche Zitationen und speichert diese.


```R
cits <- rebuild_data_source_cocit(paperTable = extracted_data, ignoreCRs = TRUE)
citsWithCRs <- assign_citation_numbers(cits = cits)
```
Nachdem das Dataframe erstellt wurde ist es sinnvoll die Daten auf der HHD/SSD persistent zu speichern.

```R
write.csv2(x = cocits, file = "Zitationen mit Zitationsnummern.csv")
write.csv2(extracted_data, file = "papers.csv")
```

### Text Mining

An dieser Stelle ist es nötig mit den Zitationen ein Co-Zitationsnetzwerk zu berechnen Datensatz. Dies kann die hier dargestellte Bibliothek nicht, da dieser Prozess von dem eigenen Zeil abhängig ist.

Nachdem die Strukturen berechnet wurden ist es nötig, dass die DOIs zu jeder relevanten Zitation zugeordnet sind, da sonst keine adäquate Möglichkeit besteht die Zitationen zu Publikationen zuzuordnen.

Aus den relevanten Zitationen wird anschließend eine Suchabfrage für das Web of Knowledge erstellt die unter Erweiterter Suche eingegeben werden kann.
* BILD

Die Daten dieses Exports enthalten für die Selektion alle relevanten Publikationen.
Wichtig ist nun eine Markierung der Abstracts zu den Clustergruppen. Wenn der Graph mit dem igraph Package erstellt wurde dann können  die Funktionen des Pakets verwendet werden.
* CODE

Anschließend werden die Zuweisungen und Text Mining Strukturen erstellt.
* CODE

Nun sind alle relevanten Informationen in einer Text Mining Struktur eingetragen. Die Daten können nun beliebig verknüpft werden.

### Text Mining Verküpfung mit Co-Zitationsnetzwerkgraph von igraph

Wurden der Graph mit igraph erstellt können mit den folgenden Schritten die Daten kombiniert werden.
* Code


## English

