# LoremIpsum
Wir haben einen Lorem Ipsum (https://de.wikipedia.org/wiki/Lorem_ipsum) Generator erstellt, der auf Zufallszahlen passiert und somit immer einen anderen Text erzeugt.

Zum starten muss nur das Projekt mit 'stack build' gebaut werden. Anschleßend kann man die Anwendung wie gewohnt mit 'stack repl'
starten.
Um sich einen Text generieren zu lassen muss man nur die main ausführen.

Man wird gefragt, mit welcher Textdatei gearbeitet werden soll. Gibt man nichts ein, sondern drückt einfach Enter, so eird der Original-LoremIpsum-Text verwendet. Möglich ist allerdings auch >>LoremIpsum_deutsch.txt<<, >>LoremIpsum_englisch.txt<< und  >>LoremIpsum_tuerkisch.txt<<, welche nicht LoremIpsum selbst, sondern den Wikipedia Atikel dazu in der jeweiligen Sprache enthalten.

-----------------------------------------------------------------------------------------------------------------------------------

Wir haben dieses Programm komplett zusammen (also auch räumlichh und daher immer im Dialog) erarbeitet.
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
