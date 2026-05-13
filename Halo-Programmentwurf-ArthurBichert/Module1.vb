Module Module1

    ' =====================================================================
    '  HALO - Erweitertes Konsolespiel
    '  Basiert auf der Schul-Basislösung, erweitert um:
    '  - Punktesystem mit Bonuspunkten
    '  - Verschiedene Hindernissymbole (x, X, #)
    '  - Startmenü mit ASCII-Logo
    '  - Pausefunktion (Taste P)
    '  - Soundeffekte
    '  - Power-Ups (♥ O T S)
    '  - Schwierigkeitsstufen
    '  - Highscore-Speicherung
    '  - Explosionseffekt & Levelfarben
    ' =====================================================================

    ' --- Tastatur-Konstanten ---
    Const NO_KEY = 0
    Const CURSOR_LEFT = 1
    Const CURSOR_RIGHT = 2
    Const KEY_PAUSE = 3
    Const UNKNOWN_KEY = 99

    ' --- Spielfeld-Größe ---
    Const SPALTE_MAX = 79
    Const ZEILE_MAX = 24

    ' --- Hindernisse: Anzahl ---
    Const A_MIN = 1
    Const A_MAX_START = 2

    ' --- Hindernisse: Größe ---
    Const G_MIN = 1
    Const G_MAX = 9

    ' --- Hindernisse: Position ---
    Const P_MIN = 0
    Const P_MAX = SPALTE_MAX

    ' --- Bewegungsschritte pro Spielfeldrunde ---
    Const BEWEGUNG_SPIELFIGUR = 10

    ' --- Hindernissymbole ---
    Const HINDERNIS_KLEIN = "x"c    ' kleines Hindernis  → 1 Schadenspunkt
    Const HINDERNIS_GROSS = "X"c    ' großes Hindernis   → 1 Schadenspunkt
    Const HINDERNIS_SELTEN = "#"c   ' seltenes Hindernis → 2 Schadenspunkte (Doppelschaden)

    ' --- Power-Up-Symbole ---
    Const POWERUP_HERZ = "H"c       ' ♥ → +1 Leben   (als "H" im Array)
    Const POWERUP_SCHILD = "O"c     ' O → schützt 1x vor Schaden
    Const POWERUP_TURBO = "T"c      ' T → Spieler schneller
    Const POWERUP_SLOW = "S"c       ' S → Hindernisse langsamer

    ' --- Highscore-Datei ---
    Const HIGHSCORE_DATEI = "highscore.txt"

    ' --- Globale Spielzustandsvariablen ---
    Dim paused As Boolean = False       ' Ist das Spiel gerade pausiert?
    Dim schildAktiv As Boolean = False  ' Ist der Schild-Power-Up aktiv?
    Dim turboAktiv As Boolean = False   ' Ist der Turbo-Power-Up aktiv?
    Dim slowAktiv As Boolean = False    ' Ist Slow-Motion aktiv?
    Dim turboTimer As Integer = 0       ' Zähler für Turbo-Dauer
    Dim slowTimer As Integer = 0        ' Zähler für Slow-Dauer

    ' =====================================================================
    '  TASTATURABFRAGE
    ' =====================================================================
    Function Tastatur_Abfrage() As Integer
        Dim cki As New ConsoleKeyInfo()
        If Console.KeyAvailable = False Then
            Return NO_KEY
        Else
            cki = Console.ReadKey(True)
            If cki.Key = ConsoleKey.LeftArrow Then
                Return CURSOR_LEFT
            ElseIf cki.Key = ConsoleKey.RightArrow Then
                Return CURSOR_RIGHT
            ElseIf cki.Key = ConsoleKey.P Then
                Return KEY_PAUSE
            Else
                Return UNKNOWN_KEY
            End If
        End If
    End Function

    ' =====================================================================
    '  ZUFALLSZEILE ERZEUGEN (mit verschiedenen Hindernissymbolen + Power-Ups)
    ' =====================================================================
    Sub Erzeuge_Zeile(ByRef Zeile() As Char, ByVal a_max As Integer)

        Dim a As Integer    ' Anzahl Hindernisblöcke
        Dim x As Single
        Dim i, j As Integer
        Dim g As Integer    ' Größe eines Hindernisblocks
        Dim p As Integer    ' Startposition eines Hindernisblocks
        Dim symbol As Char  ' Welches Symbol wird verwendet?

        ' Zeile mit Leerzeichen füllen (leere Zeile)
        For i = 0 To SPALTE_MAX
            Zeile(i) = " "c
        Next

        ' Anzahl A der Hindernisblöcke zufällig ermitteln
        Randomize()
        x = VBMath.Rnd()
        a = CInt((a_max - A_MIN) * x + A_MIN)

        ' Für jeden der A Hindernisblöcke:
        For i = 1 To a

            ' --- Symbolwahl: zufällig gewichtet ---
            ' 60% kleines Hindernis, 30% großes, 10% seltenes (doppelter Schaden)
            Randomize()
            Dim symbolWurf As Single = VBMath.Rnd()
            If symbolWurf < 0.6 Then
                symbol = HINDERNIS_KLEIN
            ElseIf symbolWurf < 0.9 Then
                symbol = HINDERNIS_GROSS
            Else
                symbol = HINDERNIS_SELTEN
            End If

            ' Größe G des Hindernisblocks zufällig ermitteln
            Randomize()
            x = VBMath.Rnd()
            g = CInt((G_MAX - G_MIN) * x + G_MIN)

            ' Startposition P zufällig ermitteln
            Randomize()
            x = VBMath.Rnd()
            p = CInt((P_MAX - P_MIN) * x + P_MIN)

            ' Für jedes der G Einzelhindernisse im Block:
            For j = 1 To g
                If p + j - 1 <= SPALTE_MAX Then
                    Zeile(p + j - 1) = symbol
                End If
            Next

        Next

        ' --- Power-Up mit kleiner Wahrscheinlichkeit einfügen ---
        Randomize()
        If VBMath.Rnd() < 0.08 Then   ' 8% Chance auf ein Power-Up pro Zeile
            Randomize()
            Dim puPos As Integer = CInt(VBMath.Rnd() * SPALTE_MAX)

            ' Nur einfügen, wenn die Stelle frei ist
            If Zeile(puPos) = " "c Then
                Randomize()
                Dim puWurf As Single = VBMath.Rnd()
                If puWurf < 0.25 Then
                    Zeile(puPos) = POWERUP_HERZ    ' Herz
                ElseIf puWurf < 0.5 Then
                    Zeile(puPos) = POWERUP_SCHILD  ' Schild
                ElseIf puWurf < 0.75 Then
                    Zeile(puPos) = POWERUP_TURBO   ' Turbo
                Else
                    Zeile(puPos) = POWERUP_SLOW    ' Slow-Motion
                End If
            End If
        End If

    End Sub

    ' =====================================================================
    '  EIN ZEICHEN MIT FARBE AUSGEBEN (Hilfsfunktion)
    ' =====================================================================
    Sub SchreibeZeichen(ByVal zeichen As Char, ByVal fg As ConsoleColor, ByVal bg As ConsoleColor)
        Console.ForegroundColor = fg
        Console.BackgroundColor = bg
        Console.Write(zeichen)
        Console.ResetColor()
    End Sub

    ' =====================================================================
    '  SPIELFELD AUSGEBEN (farbige Hindernisse + Power-Ups)
    ' =====================================================================
    Sub Spielfeld_Ausgeben(ByRef spielfeld(,) As Char, ByVal level As Integer)

        ' Hintergrundfarbe je nach Level wechseln
        Dim bgFarbe As ConsoleColor
        Select Case (level Mod 5)
            Case 0 : bgFarbe = ConsoleColor.Black
            Case 1 : bgFarbe = ConsoleColor.DarkBlue
            Case 2 : bgFarbe = ConsoleColor.DarkGreen
            Case 3 : bgFarbe = ConsoleColor.DarkMagenta
            Case 4 : bgFarbe = ConsoleColor.DarkRed
            Case Else : bgFarbe = ConsoleColor.Black
        End Select

        Console.SetCursorPosition(0, 0)

        For z As Integer = 0 To ZEILE_MAX - 2
            For s As Integer = 0 To SPALTE_MAX
                Dim c As Char = spielfeld(z, s)
                Select Case c
                    Case HINDERNIS_KLEIN
                        SchreibeZeichen(c, ConsoleColor.Yellow, bgFarbe)
                    Case HINDERNIS_GROSS
                        SchreibeZeichen(c, ConsoleColor.Red, bgFarbe)
                    Case HINDERNIS_SELTEN
                        SchreibeZeichen(c, ConsoleColor.Magenta, bgFarbe)
                    Case POWERUP_HERZ
                        SchreibeZeichen("H"c, ConsoleColor.Red, bgFarbe)
                    Case POWERUP_SCHILD
                        SchreibeZeichen("O"c, ConsoleColor.Cyan, bgFarbe)
                    Case POWERUP_TURBO
                        SchreibeZeichen("T"c, ConsoleColor.Green, bgFarbe)
                    Case POWERUP_SLOW
                        SchreibeZeichen("S"c, ConsoleColor.Blue, bgFarbe)
                    Case Else
                        Console.BackgroundColor = bgFarbe
                        Console.ForegroundColor = ConsoleColor.White
                        Console.Write(" "c)
                        Console.ResetColor()
                End Select
            Next
            Console.WriteLine()
        Next

    End Sub

    ' =====================================================================
    '  EXPLOSIONSEFFEKT BEI KOLLISION
    ' =====================================================================
    Sub Explosion_Anzeigen(ByVal spalte As Integer, ByVal zeile As Integer)
        Dim explosionsFrames() As String = {"*", "+", ".", " "}

        For Each frame In explosionsFrames
            Console.SetCursorPosition(spalte, zeile)
            Console.ForegroundColor = ConsoleColor.Red
            Console.Write(frame)
            Console.ResetColor()
            Threading.Thread.Sleep(60)
        Next
    End Sub

    ' =====================================================================
    '  STATUSZEILE AUSGEBEN (Leben, Punkte, Level, Power-Ups)
    ' =====================================================================
    Sub Status_Ausgeben(ByVal leben As Integer, ByVal punkte As Integer,
                        ByVal level As Integer, ByVal schildAktivLokal As Boolean,
                        ByVal turboAktivLokal As Boolean, ByVal slowAktivLokal As Boolean)

        Console.SetCursorPosition(0, ZEILE_MAX)
        Console.ForegroundColor = ConsoleColor.White
        Console.BackgroundColor = ConsoleColor.DarkGray

        ' Leben rot anzeigen wenn wenig übrig
        Dim lebenFarbe As ConsoleColor = ConsoleColor.Green
        If leben <= 2 Then lebenFarbe = ConsoleColor.Red

        Console.Write(" Leben: ")
        Console.ForegroundColor = lebenFarbe
        Console.Write(leben)
        Console.ForegroundColor = ConsoleColor.White
        Console.Write("   Punkte: ")
        Console.ForegroundColor = ConsoleColor.Yellow
        Console.Write(punkte)
        Console.ForegroundColor = ConsoleColor.White
        Console.Write("   Level: ")
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.Write(level)
        Console.ForegroundColor = ConsoleColor.White

        ' Aktive Power-Ups anzeigen
        If schildAktivLokal Then
            Console.ForegroundColor = ConsoleColor.Cyan
            Console.Write("  [SCHILD]")
        End If
        If turboAktivLokal Then
            Console.ForegroundColor = ConsoleColor.Green
            Console.Write("  [TURBO]")
        End If
        If slowAktivLokal Then
            Console.ForegroundColor = ConsoleColor.Blue
            Console.Write("  [SLOW] ")
        End If

        Console.ResetColor()
        ' Zeile bis zum Ende auffüllen
        Dim posX As Integer = Console.CursorLeft
        For k As Integer = posX To SPALTE_MAX
            Console.Write(" ")
        Next

    End Sub

    ' =====================================================================
    '  PAUSEBILDSCHIRM
    ' =====================================================================
    Sub Pause_Anzeigen()
        Console.SetCursorPosition(30, ZEILE_MAX \ 2)
        Console.ForegroundColor = ConsoleColor.Yellow
        Console.BackgroundColor = ConsoleColor.DarkBlue
        Console.Write("  *** PAUSE - P zum Weiterspielen ***  ")
        Console.ResetColor()
    End Sub

    ' =====================================================================
    '  GAME OVER BILDSCHIRM
    ' =====================================================================
    Sub Game_Over(ByVal punkte As Integer)
        Console.BackgroundColor = ConsoleColor.DarkRed
        Console.ForegroundColor = ConsoleColor.White
        Console.Clear()

        Console.SetCursorPosition(0, 8)
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("  ________                        ________                      ")
        Console.WriteLine(" /  _____/_____    _____   ____   \_____  \___  __ ___________  ")
        Console.WriteLine("/   \  ___\__  \  /     \_/ __ \   /   |   \  \/ // __ \_  __ \ ")
        Console.WriteLine("\    \_\  \/ __ \|  Y Y  \  ___/  /    |    \   /\  ___/|  | \/ ")
        Console.WriteLine(" \______  (____  /__|_|  /\___  > \_______  /\_/  \___  >__|    ")

        Console.ForegroundColor = ConsoleColor.Yellow
        Console.SetCursorPosition(28, 15)
        Console.Write("Deine Punkte: " & punkte)

        ' Game-Over-Sound: absteigender Ton
        Console.Beep(600, 300)
        Threading.Thread.Sleep(100)
        Console.Beep(450, 300)
        Threading.Thread.Sleep(100)
        Console.Beep(300, 600)

        Console.ForegroundColor = ConsoleColor.White
        Console.SetCursorPosition(25, 18)
        Console.Write("Drücke ENTER zum Beenden...")
        Console.ResetColor()
        Console.ReadLine()
    End Sub

    ' =====================================================================
    '  HIGHSCORE LESEN
    ' =====================================================================
    Function Highscores_Lesen() As List(Of Integer)
        Dim liste As New List(Of Integer)
        Try
            If System.IO.File.Exists(HIGHSCORE_DATEI) Then
                Dim zeilen() As String = System.IO.File.ReadAllLines(HIGHSCORE_DATEI)
                For Each zeile As String In zeilen
                    Dim wert As Integer
                    If Integer.TryParse(zeile.Trim(), wert) Then
                        liste.Add(wert)
                    End If
                Next
            End If
        Catch
            ' Fehler ignorieren (Datei nicht vorhanden etc.)
        End Try
        ' Sortieren: höchster Wert zuerst
        liste.Sort()
        liste.Reverse()
        Return liste
    End Function

    ' =====================================================================
    '  HIGHSCORE SPEICHERN
    ' =====================================================================
    Sub Highscore_Speichern(ByVal punkte As Integer)
        Dim liste As List(Of Integer) = Highscores_Lesen()
        liste.Add(punkte)
        liste.Sort()
        liste.Reverse()
        ' Maximal 10 Einträge speichern
        Dim maxEintraege As Integer = Math.Min(liste.Count, 10)
        Dim zeilen(maxEintraege - 1) As String
        For i As Integer = 0 To maxEintraege - 1
            zeilen(i) = liste(i).ToString()
        Next
        Try
            System.IO.File.WriteAllLines(HIGHSCORE_DATEI, zeilen)
        Catch
            ' Fehler ignorieren
        End Try
    End Sub

    ' =====================================================================
    '  HIGHSCORE ANZEIGEN
    ' =====================================================================
    Sub Highscores_Anzeigen()
        Dim liste As List(Of Integer) = Highscores_Lesen()

        Console.Clear()
        Console.ForegroundColor = ConsoleColor.Yellow
        Console.SetCursorPosition(28, 2)
        Console.WriteLine("=== HIGHSCORES ===")
        Console.ForegroundColor = ConsoleColor.White

        If liste.Count = 0 Then
            Console.SetCursorPosition(28, 5)
            Console.WriteLine("Noch keine Einträge.")
        Else
            For i As Integer = 0 To Math.Min(liste.Count - 1, 9)
                Console.SetCursorPosition(30, 5 + i)
                Console.ForegroundColor = If(i = 0, ConsoleColor.Yellow, ConsoleColor.White)
                Console.Write((i + 1) & ". Platz:  " & liste(i) & " Punkte")
                Console.ResetColor()
            Next
        End If

        Console.ForegroundColor = ConsoleColor.Gray
        Console.SetCursorPosition(25, 18)
        Console.Write("Drücke ENTER um zurückzugehen...")
        Console.ResetColor()
        Console.ReadLine()
    End Sub

    ' =====================================================================
    '  ASCII HALO LOGO (für Startmenü)
    ' =====================================================================
    Sub Halo_Logo_Anzeigen()
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.SetCursorPosition(10, 1)
        Console.WriteLine("  _   _    _    _     ___  ")
        Console.SetCursorPosition(10, 2)
        Console.WriteLine(" | | | |  / \  | |   / _ \ ")
        Console.SetCursorPosition(10, 3)
        Console.WriteLine(" | |_| | / _ \ | |  | | | |")
        Console.SetCursorPosition(10, 4)
        Console.WriteLine(" |  _  |/ ___ \| |__| |_| |")
        Console.SetCursorPosition(10, 5)
        Console.WriteLine(" |_| |_/_/   \_\_____\___/ ")
        Console.ForegroundColor = ConsoleColor.DarkCyan
        Console.SetCursorPosition(15, 6)
        Console.WriteLine("~ Das Hindernisrennen ~")
        Console.ResetColor()
    End Sub

    ' =====================================================================
    '  ANLEITUNG ANZEIGEN
    ' =====================================================================
    Sub Anleitung_Anzeigen()
        Console.Clear()
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.SetCursorPosition(28, 1)
        Console.WriteLine("=== ANLEITUNG ===")
        Console.ResetColor()

        Dim zeile As Integer = 3
        Console.SetCursorPosition(5, zeile) : Console.ForegroundColor = ConsoleColor.White
        Console.Write("STEUERUNG:")
        zeile += 1
        Console.SetCursorPosition(5, zeile) : Console.Write("  Pfeiltaste Links / Rechts  →  Spielfigur bewegen")
        zeile += 1
        Console.SetCursorPosition(5, zeile) : Console.Write("  Taste P                   →  Pause / Weiter")
        zeile += 2

        Console.SetCursorPosition(5, zeile) : Console.ForegroundColor = ConsoleColor.Yellow
        Console.Write("HINDERNISSE:")
        zeile += 1
        Console.SetCursorPosition(5, zeile) : Console.ForegroundColor = ConsoleColor.Yellow
        Console.Write("  x  →  Kleines Hindernis  (1 Leben Schaden)")
        zeile += 1
        Console.SetCursorPosition(5, zeile) : Console.ForegroundColor = ConsoleColor.Red
        Console.Write("  X  →  Großes Hindernis   (1 Leben Schaden)")
        zeile += 1
        Console.SetCursorPosition(5, zeile) : Console.ForegroundColor = ConsoleColor.Magenta
        Console.Write("  #  →  Seltenes Hindernis (2 Leben Schaden!)")
        zeile += 2

        Console.SetCursorPosition(5, zeile) : Console.ForegroundColor = ConsoleColor.Green
        Console.Write("POWER-UPS:")
        zeile += 1
        Console.SetCursorPosition(5, zeile) : Console.ForegroundColor = ConsoleColor.Red
        Console.Write("  H  →  Herz:      +1 Leben")
        zeile += 1
        Console.SetCursorPosition(5, zeile) : Console.ForegroundColor = ConsoleColor.Cyan
        Console.Write("  O  →  Schild:    schützt 1x vor Schaden")
        zeile += 1
        Console.SetCursorPosition(5, zeile) : Console.ForegroundColor = ConsoleColor.Green
        Console.Write("  T  →  Turbo:     Spieler bewegt sich schneller")
        zeile += 1
        Console.SetCursorPosition(5, zeile) : Console.ForegroundColor = ConsoleColor.Blue
        Console.Write("  S  →  Slow:      Hindernisse verlangsamen sich")
        zeile += 2

        Console.SetCursorPosition(5, zeile) : Console.ForegroundColor = ConsoleColor.White
        Console.Write("PUNKTE:")
        zeile += 1
        Console.SetCursorPosition(5, zeile) : Console.Write("  +10 Punkte pro überstandener Runde")
        zeile += 1
        Console.SetCursorPosition(5, zeile) : Console.Write("  +50 Bonuspunkte wenn Leben ≤ 2 (knappe Situation)")
        zeile += 2

        Console.ForegroundColor = ConsoleColor.Gray
        Console.SetCursorPosition(25, zeile + 1)
        Console.Write("Drücke ENTER um zurückzugehen...")
        Console.ResetColor()
        Console.ReadLine()
    End Sub

    ' =====================================================================
    '  STARTMENÜ (mit Schwierigkeitswahl)
    ' =====================================================================
    Function Startmenue_Anzeigen() As Integer
        ' Rückgabe: 0 = Spiel beenden, 1 = Leicht, 2 = Mittel, 3 = Schwer

        Do
            Console.Clear()
            Halo_Logo_Anzeigen()

            ' Highscore oben rechts anzeigen
            Dim hsListe As List(Of Integer) = Highscores_Lesen()
            Console.SetCursorPosition(55, 1)
            Console.ForegroundColor = ConsoleColor.Yellow
            If hsListe.Count > 0 Then
                Console.Write("Highscore: " & hsListe(0))
            Else
                Console.Write("Highscore: ---")
            End If
            Console.ResetColor()

            ' Menüpunkte
            Console.ForegroundColor = ConsoleColor.White
            Console.SetCursorPosition(30, 9)
            Console.Write("[1]  Spiel starten")
            Console.SetCursorPosition(30, 11)
            Console.Write("[2]  Highscores anzeigen")
            Console.SetCursorPosition(30, 13)
            Console.Write("[3]  Anleitung")
            Console.SetCursorPosition(30, 15)
            Console.Write("[4]  Beenden")

            Console.ForegroundColor = ConsoleColor.Gray
            Console.SetCursorPosition(25, 18)
            Console.Write("Deine Wahl: ")
            Console.ResetColor()

            Dim eingabe As String = Console.ReadLine()

            Select Case eingabe.Trim()
                Case "1"
                    ' Schwierigkeitsgrad wählen
                    Return Schwierigkeitsgrad_Waehlen()
                Case "2"
                    Highscores_Anzeigen()
                Case "3"
                    Anleitung_Anzeigen()
                Case "4"
                    Return 0
            End Select
        Loop
    End Function

    ' =====================================================================
    '  SCHWIERIGKEITSGRAD WÄHLEN
    ' =====================================================================
    Function Schwierigkeitsgrad_Waehlen() As Integer
        Console.Clear()
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.SetCursorPosition(25, 3)
        Console.WriteLine("=== SCHWIERIGKEITSGRAD ===")
        Console.ResetColor()

        Console.SetCursorPosition(28, 6) : Console.ForegroundColor = ConsoleColor.Green
        Console.Write("[1]  Leicht  (weniger Hindernisse, langsamer)")
        Console.SetCursorPosition(28, 8) : Console.ForegroundColor = ConsoleColor.Yellow
        Console.Write("[2]  Mittel  (Standard)")
        Console.SetCursorPosition(28, 10) : Console.ForegroundColor = ConsoleColor.Red
        Console.Write("[3]  Schwer  (mehr Hindernisse, schneller)")
        Console.ResetColor()

        Console.SetCursorPosition(28, 13) : Console.ForegroundColor = ConsoleColor.Gray
        Console.Write("Deine Wahl: ")
        Console.ResetColor()

        Dim eingabe As String = Console.ReadLine()

        Select Case eingabe.Trim()
            Case "1" : Return 1
            Case "2" : Return 2
            Case "3" : Return 3
            Case Else : Return 2  ' Standard: Mittel
        End Select
    End Function

    ' =====================================================================
    '  HAUPTSPIELABLAUF
    ' =====================================================================
    Sub Spielablauf(ByVal schwierigkeit As Integer)

        ' --- Lokale Variablen ---
        Dim leben As Integer
        Dim spielfeld(ZEILE_MAX, SPALTE_MAX) As Char
        Dim zeile(SPALTE_MAX) As Char
        Dim z, s As Integer
        Dim taste As Integer
        Dim spielfigurSpalte As Integer
        Dim i As Integer
        Dim wartezeit As Single
        Dim a_max As Single
        Dim punkte As Integer
        Dim runde As Integer
        Dim level As Integer

        ' --- Startwerte je nach Schwierigkeitsgrad ---
        Select Case schwierigkeit
            Case 1  ' Leicht
                leben = 7
                wartezeit = 280
                a_max = 1.5
            Case 3  ' Schwer
                leben = 3
                wartezeit = 140
                a_max = 3
            Case Else  ' Mittel (Standard)
                leben = 5
                wartezeit = 200
                a_max = A_MAX_START
        End Select

        spielfigurSpalte = SPALTE_MAX \ 2
        punkte = 0
        runde = 0
        level = 1

        ' Power-Up-Zustände zurücksetzen
        schildAktiv = False
        turboAktiv = False
        slowAktiv = False
        turboTimer = 0
        slowTimer = 0

        ' Konsole vorbereiten
        Console.CursorVisible = False
        Console.Clear()

        ' ================================================================
        '  HAUPTSPIELSCHLEIFE
        ' ================================================================
        Do
            runde += 1

            ' Level berechnen (alle 20 Runden ein neues Level)
            level = (runde \ 20) + 1

            ' Neue Zeile erzeugen
            Dim effektivesAMax As Single = a_max
            If slowAktiv Then effektivesAMax = a_max * 0.6  ' Slow: weniger Hindernisse

            Erzeuge_Zeile(zeile, effektivesAMax)

            ' Alle Zeilen des Spielfelds um eine nach unten verschieben
            For z = ZEILE_MAX To 1 Step -1
                For s = 0 To SPALTE_MAX
                    spielfeld(z, s) = spielfeld(z - 1, s)
                Next
            Next

            ' Neue Zeile oben eintragen
            For s = 0 To SPALTE_MAX
                spielfeld(0, s) = zeile(s)
            Next

            ' Spielfeld ausgeben (mit Level-Farbe)
            Spielfeld_Ausgeben(spielfeld, level)

            ' ============================================================
            '  BEWEGUNGSSCHLEIFE (BEWEGUNG_SPIELFIGUR Schritte pro Runde)
            ' ============================================================
            Dim bewegungsSchritte As Integer = BEWEGUNG_SPIELFIGUR
            If turboAktiv Then bewegungsSchritte = BEWEGUNG_SPIELFIGUR * 2  ' Turbo verdoppelt Schritte

            For i = 1 To bewegungsSchritte

                ' Tastatur abfragen
                taste = Tastatur_Abfrage()

                ' Pause-Taste?
                If taste = KEY_PAUSE Then
                    paused = Not paused
                    If paused Then
                        Pause_Anzeigen()
                    Else
                        ' Spielfeld neu zeichnen um Pause-Text zu entfernen
                        Spielfeld_Ausgeben(spielfeld, level)
                        Status_Ausgeben(leben, punkte, level, schildAktiv, turboAktiv, slowAktiv)
                    End If
                End If

                ' Solange pausiert: warten
                If paused Then
                    Threading.Thread.Sleep(50)
                    Continue For
                End If

                ' Alte Spielfigur löschen
                Console.SetCursorPosition(spielfigurSpalte, ZEILE_MAX - 1)
                Console.Write(" "c)

                ' Position der Spielfigur berechnen
                If taste = CURSOR_LEFT Then
                    spielfigurSpalte -= 1
                    If spielfigurSpalte < 0 Then spielfigurSpalte = 0
                End If
                If taste = CURSOR_RIGHT Then
                    spielfigurSpalte += 1
                    If spielfigurSpalte > SPALTE_MAX Then spielfigurSpalte = SPALTE_MAX
                End If

                ' Spielfigur ausgeben
                Console.SetCursorPosition(spielfigurSpalte, ZEILE_MAX - 1)
                Console.ForegroundColor = ConsoleColor.White
                Console.Write("@"c)
                Console.ResetColor()

                ' --------------------------------------------------------
                '  KOLLISIONSPRÜFUNG
                ' --------------------------------------------------------
                Dim feldUnterSpieler As Char = spielfeld(ZEILE_MAX - 2, spielfigurSpalte)

                ' Hindernisse treffen?
                If feldUnterSpieler = HINDERNIS_KLEIN OrElse
                   feldUnterSpieler = HINDERNIS_GROSS OrElse
                   feldUnterSpieler = HINDERNIS_SELTEN Then

                    ' Explosionseffekt
                    Explosion_Anzeigen(spielfigurSpalte, ZEILE_MAX - 2)

                    If schildAktiv Then
                        ' Schild schützt einmalig
                        schildAktiv = False
                        Console.Beep(800, 150)  ' Schild-Absorbier-Sound
                    Else
                        ' Schaden berechnen
                        Dim schaden As Integer = 1
                        If feldUnterSpieler = HINDERNIS_SELTEN Then schaden = 2
                        leben -= schaden

                        ' Kollisions-Sound
                        Console.Beep(300, 200)
                        Threading.Thread.Sleep(50)
                        Console.Beep(250, 200)
                    End If

                    ' Hindernis nach Kollision löschen
                    spielfeld(ZEILE_MAX - 2, spielfigurSpalte) = " "c

                End If

                ' --------------------------------------------------------
                '  POWER-UP AUFNEHMEN
                ' --------------------------------------------------------
                Select Case feldUnterSpieler
                    Case POWERUP_HERZ
                        leben += 1
                        Console.Beep(880, 100)
                        Console.Beep(1100, 100)
                        spielfeld(ZEILE_MAX - 2, spielfigurSpalte) = " "c

                    Case POWERUP_SCHILD
                        schildAktiv = True
                        Console.Beep(600, 100)
                        Console.Beep(800, 100)
                        spielfeld(ZEILE_MAX - 2, spielfigurSpalte) = " "c

                    Case POWERUP_TURBO
                        turboAktiv = True
                        turboTimer = 50
                        Console.Beep(1000, 80)
                        Console.Beep(1200, 80)
                        spielfeld(ZEILE_MAX - 2, spielfigurSpalte) = " "c

                    Case POWERUP_SLOW
                        slowAktiv = True
                        slowTimer = 50
                        Console.Beep(500, 80)
                        Console.Beep(400, 80)
                        spielfeld(ZEILE_MAX - 2, spielfigurSpalte) = " "c
                End Select

                ' Power-Up-Timer herunterzählen
                If turboTimer > 0 Then
                    turboTimer -= 1
                    If turboTimer = 0 Then turboAktiv = False
                End If
                If slowTimer > 0 Then
                    slowTimer -= 1
                    If slowTimer = 0 Then slowAktiv = False
                End If

                ' Statuszeile ausgeben
                Status_Ausgeben(leben, punkte, level, schildAktiv, turboAktiv, slowAktiv)

                ' Wartezeit (Slow verdoppelt die Pause → langsamere Hindernisse)
                Dim schlafzeit As Single = wartezeit / bewegungsSchritte
                If slowAktiv Then schlafzeit *= 1.8
                Threading.Thread.Sleep(CInt(schlafzeit))

            Next  ' Ende Bewegungsschleife

            ' Tastaturpuffer leeren
            Do
                taste = Tastatur_Abfrage()
            Loop Until taste = NO_KEY

            ' --- Punkte zählen ---
            punkte += 10  ' +10 pro überstandener Runde

            ' Bonuspunkte bei knapper Situation (≤ 2 Leben)
            If leben <= 2 Then
                punkte += 50
            End If

            ' Level-Up-Sound alle 20 Runden
            If runde Mod 20 = 0 Then
                Console.Beep(523, 100)
                Console.Beep(659, 100)
                Console.Beep(784, 200)
            End If

            ' Wartezeit verringern (Spiel wird schneller)
            wartezeit *= 0.99
            If wartezeit < 20 Then wartezeit = 20

            ' Hindernisdichte erhöhen
            a_max *= 1.03

        Loop Until leben <= 0

        ' Highscore speichern
        Highscore_Speichern(punkte)

        ' Game-Over-Bildschirm anzeigen
        Game_Over(punkte)

    End Sub

    ' =====================================================================
    '  EINSTIEGSPUNKT
    ' =====================================================================
    Sub Main()
        Console.CursorVisible = False
        Console.Title = "HALO - Das Hindernisrennen"

        ' Konsolengröße anpassen (falls möglich)
        Try
            Console.SetWindowSize(82, 27)
            Console.SetBufferSize(82, 27)
        Catch
            ' Ignorieren wenn Größe nicht geändert werden kann
        End Try

        ' Startmenü anzeigen und Schwierigkeit wählen
        Dim schwierigkeit As Integer = Startmenue_Anzeigen()

        ' 0 = Beenden, sonst Spiel starten
        If schwierigkeit > 0 Then
            Spielablauf(schwierigkeit)
        End If

    End Sub

End Module