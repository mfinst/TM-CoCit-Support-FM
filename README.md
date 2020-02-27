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

![](https://github.com/mfinst/TM-CoCit-Support-FM/blob/master/images/Query.PNG "Exported Data")

Da das Thema relativ umfangreich behandelt wurde werden wir die Suchanfrage bis zum Jahr 2012 eingrenzen.

![](https://github.com/mfinst/TM-CoCit-Support-FM/blob/master/images/Export.PNG "Exported Data")


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

An dieser Stelle ist es nötig mit den Zitationen ein Co-Zitationsnetzwerk zu berechnen. Dies kann die hier dargestellte Bibliothek nicht, da dieser Prozess von dem eigenen Ziel abhängig ist.

**Die Co-Zitationen lassen sich für den vorliegenden Datensatz mit einer Mindestanzahl der Zitationen von 4 berechnen.**

Dabei wird folgende Struktur für die weitere Verarbeitung vorrausgesetzt:


|A|B|A_DOI|B_DOI|ANZ|ANZC1|ANZC2|CCV|
|---|---|---|---|---|---|---|---|
|Autor, Jahr Kombinationen der Zitationen|Autor, Jahr Kombinationen der Zitationen|DOI von A|DOI von B|Anzahl der Co Zitationen|Anzahl der Co Zitationen von A|Anzahl der Co Zitationen von B|Co Zitationswert|

Mit einer solchen Tabelle lässt sich ein Graph mit dem *igraph* Paket berechnen.
```R
cocit_vertices <- unique(data.frame(c(table$A , table$B),c(table$ANZC1 , table$ANZC2)))
cocit_graph <- graph.data.frame(table, directed=FALSE, vertices=cocit_vertices)
wckarate <- walktrap.community(graph)

sapply(unique(membership(wckarate)), function(gg) {
  subg1<-induced.subgraph(graph, which(membership(wckarate)==gg)) 
  ecount(subg1)/ecount(graph)
})
```
Mit diesem Codefragment lassen sich die benötigten Strukturen bauen anhand denen das Text Mining durchgeführt werden kann.

Nachdem die benötigten Abstracts über die erweiterte Suchabfrage aus dem Web of Knowledge geladen wurden können mit ein paar Funktionsaufrufen entsprechende Strukturen gebaut werden.

```R
colors <- c("red", "blue", "green")

biggestGroupsSorted <- sort(table(mapAuthorToDOI(wckarate)[,1]), decreasing = TRUE)
colb <- arrangeColors(colorVector=colors, groupSizes=biggestGroupsSorted)
cocit_TF <- build_structure_for_list(structKey = "tf", 
                                     gen_cocit_corpuslist(mapAuthorToDOI(wckarate)), tokenizer = phraseTokenizer)
``` 
* **colors** werden benötigt um im späteren Plot die Knoten der größten Netzwerke einzufärben.
* **biggestGroupsSorted** ermittelt die größten Gruppen und sortiert diese
* **cocit_TF** enthält letzendlich die erstellten Text Mining Strukturen als sortierte Term Frequenz Matrix

Anschließend lassen sich die Informationen in einem Graphen darstellen.

```R
plot(
  wckarate,
  col=colb[membership(wckarate)], 
  vertex.size=4+V(graph)$ANZ %/% 2.5*1,
  vertex.label.dist=0.6, 
  vertex.label.cex=0.7, 
  vertex.label.degree=0, 
  layout=layout.fruchterman.reingold, graph, 
  edge.width=E(graph)$CCV*20, 
)
```
Wenn das *igraph* Paket geladen ist, wird die plot Funktion für das ipgrah Objekt angewandt und ein Plot erstellt.
Der Graph sieht anschließend ungefähr so aus:

![](https://github.com/mfinst/TM-CoCit-Support-FM/blob/master/images/graph_no_text_mining.png "Co Zitationsgraph")

Die tatsächlichen Gruppen können je nach Berechnung und Selektion des Cozitationswerts abweichen.


### Text Mining Verküpfung mit Co-Zitationsnetzwerkgraph von igraph

Die Daten können als Legende im gleichen Plot angezeigt werden.

```R
x_interspace <- 0.3
y_interspace <- 0.3
tf_length <- 20
text_width <- 0.25
legend(x = 1.15, y = 1.25, 
       legend = unlist(type.convert(head(cocit_TF[[as.numeric(names(biggestGroupsSorted)[1])]], tf_length), as.is = TRUE)),
       x.intersp = x_interspace,
       y.intersp = y_interspace,
       ncol = 2,
       box.col = colb[as.numeric(names(biggestGroupsSorted)[1])],
       box.lty = 2,
       box.lwd = 2,
       text.width = text_width
)
```
Dies sollte anschließend wiefolgt aussehen:

![](https://github.com/mfinst/TM-CoCit-Support-FM/blob/master/images/graph.png "Graph with Text Mining")

Es lohnt sich die intersp Werte auszulagern, da für jede weiteres Netzwerk eine weitere Legende dargestellt werden muss.

Zudem müssen jenachdem wieviele Werte dargestellt werden sollen die einzelnen Parameter angepasst werden.

Zusätzlich muss die x und y position individuell angepasst werden, jenachdem wie der Graph aussieht.

Es kann auch nötig sein die Informationen über eine Clustergruppe in einem einzelnen Plot darzustellen. Für diesen Fall lässt sich die **cocit_TF** mit index zur jeweiligen Clustergruppe in einem Plot der Wahl darstellen.
## English