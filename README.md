# LoremIpsum
Wir haben einen Lorem Ipsum (https://de.wikipedia.org/wiki/Lorem_ipsum) Generator erstellt, der auf Zufallszahlen passiert und somit immer einen anderen Text erzeugt.

Zum starten muss nur das Projekt mit 'stack build' gebaut werden. Anschleßend kann man die Anwendung wie gewohnt mit 'stack repl'
starten.
Um sich einen Text generieren zu lassen muss man nur die main ausführen.
Es wird immer die Datei 'LoremIpsum.txt' eingelesen. Der Inhalt dieser Datei kann vor der Programmausführung beliebig geändert werden.

-----------------------------------------------------------------------------------------------------------------------------------

Wir haben dieses Programm komplett zusammen (also auch räumlich und daher immer im Dialog) erarbeitet.
Wir gehen von einer 50/50 Beteiligung in allen Teilen der Anwendung aus.

Doch als grober Überblick bei der Ideengebung/Federführung

Seeholzer:
  Datei öffnen
  Datei analysieren
  Ur-Listenstruktur aufbauen
  Konfiguration des Projekts

Hübner:
  Ur-Listenstruktur zur statistischen Aufbereitung ummappen
  statistisches aufbereiten
  zufallsbasierte Ermittlung des nächsten Buchstaben
  Tests

zusammen:
  Zufallsgenerator (jeweils mehrfach)
  Ermittlung des endgültigen LoremIpsum-Outputs
  Dokumentation
