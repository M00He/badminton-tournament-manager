# Badminton Turnier Manager

Eine R Shiny App zur Verwaltung eines Badminton-Doppel-Turniers mit automatischer Auslosung nach Swiss-System-Prinzipien.

## Features

- **Spielerverwaltung**: Bis zu 20 Spieler hinzufügen/entfernen
- **Flexible Turniereinstellungen**: Variable Rundenanzahl und Felderzahl
- **Manuelle Eingabe Runde 1**: Erste Runde wird vor Ort ausgespielt und manuell ins Programm eingetragen
- **Automatische Auslosung ab Runde 2**:
  - Bessere Spieler werden mit schlechteren gepaart
  - Keine doppelten Partnerschaften
  - Keine doppelten Gegner-Paarungen
  - Vermeidung von Wiederholungen aus der vorherigen Runde
- **Live-Rangliste**:
  - Sortierung nach: Siege → Satzdifferenz → direkter Vergleich
  - Übersichtliche Darstellung aller Statistiken
- **Ergebnisverwaltung**: Einfache Eingabe von Punktzahlen (höhere = Gewinner)
- **Spieler-Ausfall-Management**: Spieler können vorzeitig das Turnier verlassen
- **Spielhistorie**: Alle Spiele und Ergebnisse werden angezeigt

## Installation

### Voraussetzungen

R (Version 4.0 oder höher) muss installiert sein.

### Benötigte Pakete installieren

Öffne R oder RStudio und führe folgenden Befehl aus:

```r
install.packages(c("shiny", "bslib"))
```

## App starten

### Variante 1: In RStudio

1. Öffne die Datei `app.R` in RStudio
2. Klicke auf den "Run App" Button oben rechts im Editor

### Variante 2: Über die R-Konsole

```r
# Setze das Arbeitsverzeichnis auf den App-Ordner
setwd("c:/Users/MoritzHemmann/Documents/test_claude/test_claude")

# Starte die App
shiny::runApp()
```

### Variante 3: Direkter Start aus R

```r
shiny::runApp("c:/Users/MoritzHemmann/Documents/test_claude/test_claude")
```

## Bedienung

### 1. Setup

1. Gehe zum Tab "Setup"
2. Füge Spieler hinzu (mindestens 4, maximal 20)
3. Stelle die Anzahl der Runden ein (empfohlen: 5-7 Runden)
4. Stelle die Anzahl der verfügbaren Felder ein (Standard: 4)
5. Klicke auf "Turnier starten"

### 2. Runde 1 (Manuelle Eingabe)

1. Gehe zum Tab "Aktuelle Runde"
2. Klicke auf "Auslosung generieren" um leere Felder zu erstellen
3. Wähle für jedes Feld die 4 Spieler aus (2 pro Team)
4. Nach dem Spiel: Gib die Punktzahlen ein
5. Klicke auf "Spiel speichern"
6. Wenn alle Spiele der Runde abgeschlossen sind: "Nächste Runde"

### 3. Ab Runde 2 (Automatische Auslosung)

1. Klicke auf "Auslosung generieren"
2. Die App erstellt automatisch die Paarungen basierend auf der aktuellen Rangliste
3. Gib nach jedem Spiel die Punktzahlen ein
4. Klicke auf "Spiel speichern"
5. Wenn alle Spiele abgeschlossen sind: "Nächste Runde"

### 4. Rangliste ansehen

Gehe zum Tab "Rangliste" um:
- Die aktuelle Platzierung aller Spieler zu sehen
- Detaillierte Statistiken anzuzeigen
- Alle bisherigen Spiele einzusehen

### 5. Spieler entfernen (optional)

Falls ein Spieler vorzeitig das Turnier verlassen muss:
1. Gehe zum Tab "Spieler-Verwaltung"
2. Wähle den Spieler aus
3. Klicke auf "Spieler aus Turnier entfernen"
4. Bereits gespielte Spiele bleiben bestehen
5. Ab der nächsten Runde wird der Spieler nicht mehr berücksichtigt

## Projektstruktur

```
.
├── app.R                           # Haupt-App-Datei
├── functions/
│   ├── ranking_calculation.R      # Ranglisten-Berechnung
│   └── tournament_logic.R         # Auslosungs-Algorithmus
└── modules/
    ├── module_setup.R             # Setup-Modul
    ├── module_round.R             # Runden-Modul
    └── module_ranking.R           # Ranglisten-Modul
```

## Technische Details

### Ranglisten-Berechnung

Die Rangliste wird nach folgenden Kriterien sortiert:

1. **Anzahl Siege** (höher = besser)
2. **Satzdifferenz** (Punkte für - Punkte gegen)
3. **Direkter Vergleich** (falls vorhanden)

### Auslosungs-Algorithmus

Ab Runde 2 verwendet die App einen Swiss-System-artigen Ansatz:

1. Spieler werden nach aktueller Rangliste sortiert
2. Bessere Hälfte und schlechtere Hälfte werden gebildet
3. Teams werden gemischt: 1 besserer + 1 schlechterer Spieler
4. Gegner werden ebenfalls gemischt aus beiden Hälften
5. Validierung: Keine Wiederholungen von Partnern oder Gegnern

###Constrains

- Keine doppelten Partnerschaften
- Keine doppelten Gegner über das gesamte Turnier
- Keine Wiederholung von Gegnern aus der vorherigen Runde
- Faire Verteilung: Bessere mit schlechteren Spielern

## Troubleshooting

**Problem**: App startet nicht
- Stelle sicher, dass alle Pakete installiert sind: `install.packages(c("shiny", "bslib"))`
- Überprüfe, ob du im richtigen Arbeitsverzeichnis bist

**Problem**: Auslosung schlägt fehl
- Dies kann bei wenigen Spielern und vielen Runden auftreten
- Versuche die Auslosung erneut zu generieren (zufälliger Algorithmus)
- Reduziere die Rundenanzahl

**Problem**: Spieler erscheint mehrfach in einer Runde
- Die App validiert dies automatisch - bitte Fehlermeldung beachten
- In Runde 1: Wähle jeden Spieler nur einmal aus

## Lizenz

Dieses Projekt wurde für private/interne Nutzung erstellt.

## Support

Bei Fragen oder Problemen kannst du die App anpassen oder erweitern. Der Code ist modular aufgebaut und gut dokumentiert.
