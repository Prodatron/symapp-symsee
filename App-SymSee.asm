;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                                S y m S e e                                 @
;@                                                                            @
;@             (c) 2004-2007 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;Todo
;+ ocp crunched screens machen müll
;- OCP nach SGX konverter

relocate_start

;==============================================================================
;### CODE-TEIL ################################################################
;==============================================================================

;### PROGRAMM-KOPF ############################################################

prgdatcod       equ 0           ;Länge Code-Teil (Pos+Len beliebig; inklusive Kopf!)
prgdatdat       equ 2           ;Länge Daten-Teil (innerhalb 16K Block)
prgdattra       equ 4           ;Länge Transfer-Teil (ab #C000)
prgdatorg       equ 6           ;Original-Origin
prgdatrel       equ 8           ;Anzahl Einträge Relocator-Tabelle
prgdatstk       equ 10          ;Länge Stack (Transfer-Teil beginnt immer mit Stack)
prgdatrsv       equ 12          ;*reserved* (3 bytes)
prgdatnam       equ 15          ;program name (24+1[0] chars)
prgdatflg       equ 40          ;flags (+1=16colour icon available)
prgdat16i       equ 41          ;file offset of 16colour icon
prgdatrs2       equ 43          ;*reserved* (5 bytes)
prgdatidn       equ 48          ;"SymExe10"
prgdatcex       equ 56          ;zusätzlicher Speicher für Code-Bereich
prgdatdex       equ 58          ;zusätzlicher Speicher für Data-Bereich
prgdattex       equ 60          ;zusätzlicher Speicher für Transfer-Bereich
prgdatres       equ 62          ;*reserviert* (26 bytes)
prgdatver       equ 88          ;required OS version (1.0)
prgdatism       equ 90          ;Icon (klein)
prgdatibg       equ 109         ;Icon (gross)
prgdatlen       equ 256         ;Datensatzlänge

prgpstdat       equ 6           ;Adresse Daten-Teil
prgpsttra       equ 8           ;Adresse Transfer-Teil
prgpstspz       equ 10          ;zusätzliche Prozessnummern (4*1)
prgpstbnk       equ 14          ;Bank (1-8)
prgpstmem       equ 48          ;zusätzliche Memory-Bereiche (8*5)
prgpstnum       equ 88          ;Programm-Nummer
prgpstprz       equ 89          ;Prozess-Nummer

prgcodbeg   dw prgdatbeg-prgcodbeg  ;Länge Code-Teil
            dw prgtrnbeg-prgdatbeg  ;Länge Daten-Teil
            dw prgtrnend-prgtrnbeg  ;Länge Transfer-Teil
prgdatadr   dw #1000                ;Original-Origin                    POST Adresse Daten-Teil
prgtrnadr   dw relocate_count       ;Anzahl Einträge Relocator-Tabelle  POST Adresse Transfer-Teil
prgprztab   dw prgstk-prgtrnbeg     ;Länge Stack                        POST Tabelle Prozesse
            dw 0                    ;*reserved*
prgbnknum   db 0                    ;*reserved*                         POST bank number
            db "SymSee":ds 18:db 0 ;Name
            db 1                    ;flags (+1=16c icon)
            dw prgicn16c-prgcodbeg  ;16 colour icon offset
            ds 5                    ;*reserved*
prgmemtab   db "SymExe10"           ;SymbOS-EXE-Kennung                 POST Tabelle Speicherbereiche
            dw dirsiz               ;zusätzlicher Code-Speicher
            dw 0                    ;zusätzlicher Data-Speicher
            dw 0                    ;zusätzlicher Transfer-Speicher
            ds 26                   ;*reserviert*
            db 0,4                  ;required OS version (4.0)
prgicnsml   db 2,8,8,#0F,#0F,#7B,#ED,#B0,#D0,#20,#C0,#30,#C0,#90,#90,#79,#E9,#0F,#0F
prgicnbig   db 6,24,24,#0F,#0F,#5F,#FF,#FF,#FF,#7F,#FC,#FF,#FF,#FF,#FF,#AF,#FF,#F0,#F7,#FF,#FF,#FF,#FF,#FF,#F8,#F7,#FF,#F2,#F5,#FF,#FF,#F1,#FF,#F0,#F0,#F7,#FF,#FE,#F7,#F0,#F0,#F0,#FF,#FF,#F2,#F0,#F4,#F0,#F1,#FF,#7C,#08,#30,#FF,#F0
            db #F3,#BE,#00,#75,#CD,#F0,#F0,#8F,#00,#75,#8C,#78,#F6,#E3,#08,#75,#CE,#F0,#F7,#F1,#80,#75,#FC,#F0,#9F,#F4,#C4,#35,#FE,#F0,#18,#90,#E2,#30,#0F,#E6,#10,#88,#F1,#12,#88,#00,#32,#08,#F4,#95,#C4,#00,#75,#00,#FE,#E1,#E9,#1B
            db #E2,#00,#FF,#F0,#F0,#F0,#84,#00,#FF,#FC,#F0,#E3,#00,#00,#F7,#FF,#F0,#F0,#F0,#FA,#FF,#FF,#FC,#F0,#F0,#F0,#FF,#FF,#FF,#FE,#F0,#F0,#FF,#FF,#FF,#FF,#FF,#FC


;### PRGPRZ -> Programm-Prozess
dskprzn     db 2
sysprzn     db 3
windatprz   equ 3   ;Prozeßnummer
windatsup   equ 51  ;Nummer des Superfensters+1 oder 0
prgwin      db 0    ;Nummer des Haupt-Fensters

prgprz  call SySystem_HLPINI
        ld a,(prgprzn)
        ld (prgwindat+windatprz),a
        ld (prgwinopt+windatprz),a

        ld bc,256*DSK_SRV_SCRCNV+MSC_DSK_DSKSRV
        ld de,gfxcnvtab
        ld hl,(prgbnknum)
        call msgsnd
        rst #30

        ld hl,jmp_sysinf        ;Computer-Typ holen
        ld de,256*1+5
        ld ix,cfgcpctyp
        ld iy,66+2+6+8
        rst #28

        ld c,MSC_DSK_WINOPN
        ld a,(prgbnknum)
        ld b,a
        ld de,prgwindat
        call msgsnd             ;Fenster aufbauen
prgprz1 call msgdsk             ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        cp MSR_DSK_WOPNER
        jp z,prgend             ;kein Speicher für Fenster -> Prozeß beenden
        cp MSR_DSK_WOPNOK
        jr nz,prgprz1           ;andere Message als "Fenster geöffnet" -> ignorieren
        ld a,(prgmsgb+4)
        ld (prgwin),a           ;Fenster wurde geöffnet -> Nummer merken

        jp optgfx               ;angehängt Grafik suchen

prgprz0 call msgget
        jr nc,prgprz0
        cp MSR_SYS_SELOPN       ;*** Browse-Fenster wurde geschlossen
        jp z,prgbrc
        cp MSR_DSK_WCLICK       ;*** Fenster-Aktion wurde geklickt
        jr nz,prgprz0
        ld e,(iy+1)
        ld a,(prgwin)
        cp e
        jr z,prgprz3
        ld a,(diawin)
        cp e
        jr nz,prgprz0
        ld a,(iy+2)             ;*** DIALOG-FENSTER
        cp DSK_ACT_CLOSE        ;*** Close wurde geklickt
        jp z,diainpc
prgprz3 ld a,(iy+2)             ;*** HAUPT-FENSTER
        cp DSK_ACT_CLOSE        ;*** Close wurde geklickt
        jp z,prgend
        cp DSK_ACT_MENU         ;*** Menü wurde geklickt
        jr z,prgprz2
        cp DSK_ACT_TOOLBAR      ;*** Toolbar wurde geklickt
        jr z,prgprz2
        cp DSK_ACT_KEY          ;*** Taste wurde gedrückt
        jr z,prgkey
        cp DSK_ACT_CONTENT      ;*** Inhalt wurde geklickt
        jr nz,prgprz0
prgprz2 ld l,(iy+8)
        ld h,(iy+9)
        ld a,l
        or h
        jr z,prgprz0
        ld a,(iy+3)             ;A=Klick-Typ (0/1/2=Maus links/rechts/doppelt, 7=Tastatur)
        jp (hl)

;### PRGKEY -> Taste auswerten
prgkeya equ 11
prgkeyt db   8:dw gfxprv    ;Del = Vorige
        db  32:dw gfxnxt    ;Space = Nächste
        db 136:dw gfxsup    ;Rauf
        db 137:dw gfxsdw    ;Runter
        db 138:dw gfxslf    ;Links
        db 139:dw gfxsrg    ;Rechts
        db  15:dw prgsrc    ;Ctrl+O = Open
        db  16:dw optopn    ;Ctrl+P = Optionen
        db  27:dw gfxful    ;ESC = Fullscreen
        db "f":dw gfxful    ;f = Fullscreen
        db "F":dw gfxful    ;F = Fullscreen

prgkey  ld hl,prgkeyt
        ld b,prgkeya
        ld de,3
        ld a,(iy+4)
prgkey1 cp (hl)
        jr z,prgkey2
        add hl,de
        djnz prgkey1
        jp prgprz0
prgkey2 inc hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld a,7
        jp (hl)

;### PRGEND -> Programm beenden
prgend  call gfxfre
        ld a,(prgprzn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        ld iy,prgmsgb
        ld (iy+0),MSC_SYS_PRGEND
        ld a,(prgcodbeg+prgpstnum)
        ld (iy+1),a
        rst #10
prgend0 rst #30
        jr prgend0

;### PRGINF -> Info-Fenster anzeigen
prginf  ld hl,prgmsginf         ;*** Info-Fenster
        ld b,1+128
prginf0 call prginf1
        jp prgprz0
prginf1 ld (prgmsgb+1),hl
        ld a,(prgbnknum)
        ld c,a
        ld (prgmsgb+3),bc
        ld a,MSC_SYS_SYSWRN
        ld (prgmsgb),a
prginf2 ld a,(prgprzn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        ld iy,prgmsgb
        rst #10
        ret


;==============================================================================
;### SUB-ROUTINEN #############################################################
;==============================================================================

SySystem_HLPFLG db 0    ;flag, if HLP-path is valid
SySystem_HLPPTH db "%help.exe "
SySystem_HLPPTH1 ds 128
SySHInX db ".HLP",0

SySystem_HLPINI
        ld hl,(prgcodbeg)
        ld de,prgcodbeg
        dec h
        add hl,de                   ;HL = CodeEnd = Command line
        ld de,SySystem_HLPPTH1
        ld bc,0
        db #dd:ld l,128
SySHIn1 ld a,(hl)
        or a
        jr z,SySHIn3
        cp " "
        jr z,SySHIn3
        cp "."
        jr nz,SySHIn2
        ld c,e
        ld b,d
SySHIn2 ld (de),a
        inc hl
        inc de
        db #dd:dec l
        ret z
        jr SySHIn1
SySHIn3 ld a,c
        or b
        ret z
        ld e,c
        ld d,b
        ld hl,SySHInX
        ld bc,5
        ldir
        ld a,1
        ld (SySystem_HLPFLG),a
        ret

hlpopn  ld a,(SySystem_HLPFLG)
        or a
        jp z,prgprz0
        ld a,(prgbnknum)
        ld d,a
        ld a,PRC_ID_SYSTEM
        ld c,MSC_SYS_PRGRUN
        ld hl,SySystem_HLPPTH
        ld b,l
        ld e,h
        call msgsnd1
        jp prgprz0


diawin  db 0

;### DIAINP -> Dialog-Fenster aufbauen
;### Eingabe    DE=Fenster
diainp  ld c,MSC_DSK_WINOPN     ;Fenster aufbauen
        ld a,(prgbnknum)
        ld b,a
        call msgsnd
diainp3 call msgdsk             ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        cp MSR_DSK_WOPNER
        ret z                   ;kein Speicher für Fenster -> dann halt nicht
        cp MSR_DSK_WOPNOK
        jr nz,diainp3           ;andere Message als "Fenster geöffnet" -> ignorieren
        ld a,(prgmsgb+4)
        ld (diawin),a           ;Fenster wurde geöffnet -> Nummer merken
        inc a
        ld (prgwindat+windatsup),a
        ret

;### DIAINPC -> Dialog-Fenster schließen
diainpc call diainp4            ;*** CANCEL
        jp prgprz0
diainp4 ld c,MSC_DSK_WINCLS     ;Dialog-Fenster schliessen
        ld a,(diawin)
        ld b,a
        jp msgsnd

;### PRGSRC -> Source auswählen
prgsrc  ld a,1
        ld hl,gfxmsk
        call prgbro
        jp prgprz0

;### PRGBRO -> Browse-Fenster öffnen
;### Eingabe    A=Typ (1=Source), HL=Textinput
prgbron db 0
prgbro  ld e,a
        ld a,(prgbron)
        or a
        ret nz
        ld a,e
        ld (prgbron),a
        ld (prgmsgb+8),hl
        ld hl,(prgbnknum)
        ld h,8
        ld (prgmsgb+6),hl
        ld hl,100
        ld (prgmsgb+10),hl
        ld hl,5000
        ld (prgmsgb+12),hl
        ld l,MSC_SYS_SELOPN
        ld (prgmsgb),hl
        jp prginf2

;### PRGBRC -> Browse-Fenster schließen
;### Eingabe    P1=Typ (0=Ok, 1=Abbruch, 2=FileAuswahl bereits in Benutzung, 3=kein Speicher frei, 4=kein Fenster frei), P2=PfadLänge
prgbrc  ld hl,(prgmsgb+1)
        inc l
        jr z,prgbrc2
        dec l
        ld hl,prgbron
        ld e,(hl)
        ld (hl),0
        jp nz,prgprz0
        xor a
        ld (dirlen),a
        jp gfxsel
prgbrc2 ld a,h
        ld (prgwindat+windatsup),a
        jp prgprz0

;### MSGGET -> Message für Programm abholen
;### Ausgabe    CF=0 -> keine Message vorhanden, CF=1 -> IXH=Absender, (recmsgb)=Message, A=(recmsgb+0), IY=recmsgb
;### Veraendert 
msgget  ld a,(prgprzn)
        db #dd:ld l,a           ;IXL=Rechner-Prozeß-Nummer
        db #dd:ld h,-1
        ld iy,prgmsgb           ;IY=Messagebuffer
        rst #08                 ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        or a
        db #dd:dec l
        ret nz
        ld iy,prgmsgb
        ld a,(iy+0)
        or a
        jp z,prgend
        scf
        ret

;### MSGDSK -> Message für Programm von Desktop-Prozess abholen
;### Ausgabe    CF=0 -> keine Message vorhanden, CF=1 -> IXH=Absender, (recmsgb)=Message, A=(recmsgb+0), IY=recmsgb
;### Veraendert 
msgdsk  call msgget
        jr nc,msgdsk            ;keine Message
        ld a,(dskprzn)
        db #dd:cp h
        jr nz,msgdsk            ;Message von anderem als Desktop-Prozeß -> ignorieren
        ld a,(prgmsgb)
        ret

;### MSGSND -> Message an Desktop-Prozess senden
;### Eingabe    C=Kommando, B/E/D/L/H=Parameter1/2/3/4/5
msgsnd  ld a,(dskprzn)
msgsnd1 db #dd:ld h,a
        ld a,(prgprzn)
        db #dd:ld l,a
        ld iy,prgmsgb
        ld (iy+0),c
        ld (iy+1),b
        ld (iy+2),e
        ld (iy+3),d
        ld (iy+4),l
        ld (iy+5),h
        rst #10
        ret

;### SYSCLL -> Betriebssystem-Funktion aufrufen
;### Eingabe    (SP)=Modul/Funktion, AF,BC,DE,HL,IX,IY=Register
;### Ausgabe    AF,BC,DE,HL,IX,IY=Register
sysclln db 0
syscll  ld (prgmsgb+04),bc      ;Register in Message-Buffer kopieren
        ld (prgmsgb+06),de
        ld (prgmsgb+08),hl
        ld (prgmsgb+10),ix
        ld (prgmsgb+12),iy
        push af
        pop hl
        ld (prgmsgb+02),hl
        pop hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        push hl
        ld (prgmsgb+00),de      ;Modul und Funktion in Message-Buffer kopieren
        ld a,e
        ld (sysclln),a
        ld iy,prgmsgb
        ld a,(prgprzn)          ;Desktop und System-Prozessnummer holen
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        rst #10                 ;Message senden
syscll1 rst #30
        ld iy,prgmsgb
        ld a,(prgprzn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        rst #18                 ;auf Antwort warten
        db #dd:dec l
        jr nz,syscll1
        ld a,(prgmsgb)
        sub 128
        ld e,a
        ld a,(sysclln)
        cp e
        jr nz,syscll1
        ld hl,(prgmsgb+02)      ;Register aus Message-Buffer holen
        push hl
        pop af
        ld bc,(prgmsgb+04)
        ld de,(prgmsgb+06)
        ld hl,(prgmsgb+08)
        ld ix,(prgmsgb+10)
        ld iy,(prgmsgb+12)
        ret

;### CLCM16 -> Multipliziert zwei Werte (16bit)
;### Eingabe    A=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1*Wert2 (16bit)
;### Veraendert AF,DE
clcm16  ld hl,0
        or a
clcm161 rra
        jr nc,clcm162
        add hl,de
clcm162 sla e
        rl d
        or a
        jr nz,clcm161
        ret

;### CLCD16 -> Dividiert zwei Werte (16bit)
;### Eingabe    BC=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1/Wert2, DE=Wert1 MOD Wert2
;### Veraendert AF,BC,DE
clcd16  ld a,e
        or d
        ld hl,0
        ret z
        ld a,b
        ld b,16
clcd161 rl c
        rla
        rl l
        rl h
        sbc hl,de
        jr nc,clcd162
        add hl,de
clcd162 ccf
        djnz clcd161
        ex de,hl
        rl c
        rla
        ld h,a
        ld l,c
        ret

;### CLCN32 -> Wandelt 32Bit-Zahl in ASCII-String um (mit 0 abgeschlossen)
;### Eingabe    DE,IX=Wert, IY=Adresse
;### Ausgabe    IY=Adresse letztes Zeichen
;### Veraendert AF,BC,DE,HL,IX,IY
clcn32t dw 1,0,     10,0,     100,0,     1000,0,     10000,0
        dw #86a0,1, #4240,#f, #9680,#98, #e100,#5f5, #ca00,#3b9a
clcn32z ds 4

clcn32  ld (clcn32z),ix
        ld (clcn32z+2),de
        ld ix,clcn32t+36
        ld b,9
        ld c,0
clcn321 ld a,"0"
        or a
clcn322 ld e,(ix+0):ld d,(ix+1):ld hl,(clcn32z):  sbc hl,de:ld (clcn32z),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(clcn32z+2):sbc hl,de:ld (clcn32z+2),hl
        jr c,clcn325
        inc c
        inc a
        jr clcn322
clcn325 ld e,(ix+0):ld d,(ix+1):ld hl,(clcn32z):  add hl,de:ld (clcn32z),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(clcn32z+2):adc hl,de:ld (clcn32z+2),hl
        ld de,-4
        add ix,de
        inc c
        dec c
        jr z,clcn323
        ld (iy+0),a
        inc iy
clcn323 djnz clcn321
        ld a,(clcn32z)
        add "0"
        ld (iy+0),a
        ld (iy+1),0
        ret

;### CLCR16 -> Wandelt String in 16Bit Zahl um
;### Eingabe    IX=String, A=Terminator, BC=Untergrenze (>=0), DE=Obergrenze (<=65534)
;### Ausgabe    IX=String hinter Terminator, HL=Zahl, CF=1 -> Ungültiges Format (zu groß/klein, falsches Zeichen/Terminator)
;### Veraendert AF,DE,IYL
clcr16  ld hl,0
        db #fd:ld l,a
clcr161 ld a,(ix+0)
        inc ix
        db #fd:cp l
        jr z,clcr163
        sub "0"
        jr c,clcr162
        cp 10
        jr nc,clcr162
        push af
        push de
        ld a,10
        ex de,hl
        call clcm16
        pop de
        pop af
        add l
        ld l,a
        ld a,0
        adc h
        ld h,a
        jr clcr161
clcr162 scf
        ret
clcr163 sbc hl,bc
        ret c
        add hl,bc
        inc de
        sbc hl,de
        jr nc,clcr162
        add hl,de
        or a
        ret

;### CLCLCS -> Wandelt Groß- in Kleinbuchstaben um
;### Eingabe    A=Zeichen
;### Ausgabe    A=lcase(Zeichen)
;### Verändert  F
clclcs  cp "A"
        ret c
        cp "Z"+1
        ret nc
        add "a"-"A"
        ret

;### CLCEXT -> Sucht Extension eines Files
;### Eingabe    HL=Pfad und Filename
;### Ausgabe    ZF=0 -> DE=Extension
;### Verändert  AF,HL
clcext  ld de,0
clcextf ld a,(hl)
        inc hl
        cp "."
        jr nz,clcexti
        ld e,l
        ld d,h
clcexti or a
        jr nz,clcextf
        ld a,e
        or d
        ret

;### STRLEN -> Ermittelt Länge eines Strings
;### Eingabe    HL=String
;### Ausgabe    HL=Stringende (0), BC=Länge (maximal 255)
;### Verändert  -
strlen  push af
        xor a
        ld bc,255
        cpir
        ld a,254
        sub c
        ld c,a
        dec hl
        pop af
        ret

;### STRINP -> Initialisiert Input-Control
;### Eingabe    IX=Control
;### Ausgabe    BC=String-Länge (maximal 255)
;### Verändert  HL,BC
strinp  ld l,(ix+0)
        ld h,(ix+1)
        call strlen
        ld (ix+2),0
        ld (ix+3),0
        ld (ix+4),c
        ld (ix+5),b
        ld (ix+6),0
        ld (ix+7),0
        ld (ix+8),c
        ld (ix+9),b
        ret


;==============================================================================
;### SORTIER-ROUTINEN #########################################################
;==============================================================================

;### SRTDAT -> Zeigertabelle sortieren
;### Eingabe    IX=(Zeiger)Tabelle, BC=Anzahl, A=Breite in Bytes, E=SpaltenIndex, L=Sortierung (0=aufsteigend, 1=absteigend),
;###            H=Typ (0=Zeiger auf Text [bis 0], 2=Wert, 3=Zeiger auf 4Byte-Wert)
;### Veraendert AF,BC,DE,HL,IX
srtsiz  db 0,0              ;Breite einer Spalte in Bytes
srtrow  db 0,0              ;Spaltenoffset in Bytes
srttab  dw 0                ;Tabelle
srtdir  db 0                ;Sortierrichtung (0=aufsteigend, 1=absteigend)
srttyp  db 0                ;Datentyp

srtdat  ld (srtsiz),a       ;Zeilenbreite merken
        ld (srtdir),hl      ;Datentyp und Sortierrichtung merken
        ld l,a
        ld a,e
        add a
        ld (srtrow),a
        ld e,a
        ld d,0
        add ix,de
        ld (srttab),ix      ;Tabelle mit Spaltenoffset merken
        ld e,c:ld d,b
        ld a,e:or d
        ret z
        dec de
        ld a,l
        call clcm16
        db #dd:ld e,l
        db #dd:ld d,h       ;DE=erstes Element
        add hl,de           ;HL=letztes Element
        jr srtpar

;### SRTPAR -> Teil einer Tabelle sortieren
;### Eingabe    DE=Erstes Element, HL=Letztes Element, (srttab)=Tabelle, (srtsiz)=Zeilenbreite
;### Veraendert AF,BC,DE,HL,IX
srtfir  dw 0                ;erstes Element
srtlas  dw 0                ;letztes Element

srtpar  ld (srtfir),de
        ld (srtlas),hl
        push hl
        or a
        sbc hl,de
        pop hl
        ret c
        ret z               ;Abbruch, falls HL<=DE
        push hl
        push de
        ld bc,(srttab)
        or a
        sbc hl,bc
        ex de,hl
        sbc hl,bc           ;HL,DE=Offsets auf die Elemente
        push bc
        add hl,de
        ld c,l
        ld b,h
        ld de,(srtsiz)
        call clcd16         ;HL=(Offset1+Offset2)/Zeilenlänge=Index1+Index2
        srl h
        rr l
        ex de,hl
        ld a,(srtsiz)
        call clcm16         ;HL=(Index1+Index2)/2*Zeilenlänge=OffsetMitte
        pop ix
        ex de,hl
        add ix,de           ;(IX)=mittleres Element
        pop hl              ;(HL)=erstes Element
        pop de              ;(DE)=letztes Element
srtpar1 call srtcmp         ;** 1. Schritt -> HL erhöhen, solange (HL)<(IX)
        jr nc,srtpar2
        ld bc,(srtsiz)
        add hl,bc
        jr srtpar1
srtpar2 ex de,hl            ;** 2. Schritt -> DE erniedrigen, solange (DE)>(IX)
srtpar3 call srtcmp
        jr nz,srtpar4
        ld bc,(srtsiz)
        or a
        sbc hl,bc
        jr srtpar3
srtpar4 push hl             ;** 3. Schritt -> wenn HL<=DE -> (HL) mit (DE) tauschen, HL erhöhen, DE erniedrigen
        or a                ;(Anmerkung -> DE/HL vertauscht)
        sbc hl,de
        pop hl
        jr c,srtpar5        ;DE<HL -> kein Tausch
        call srtpar6
        jr z,srtpar7
        ex de,hl
        call srtpar6
        ex de,hl
srtpar7 call srtswp
        ld bc,(srtsiz)
        ex de,hl
        add hl,bc           ;HL erhöhen
        ex de,hl
        sbc hl,bc           ;DE erniedrigen
srtpar5 push hl
        or a
        sbc hl,de           ;weitermachen solange DE>=HL
        pop hl
        ex de,hl            ;HL=erstes, DE=letztes
        jr nc,srtpar1
        push hl
        ld hl,(srtlas)
        push hl
        ld hl,(srtfir)
        ex de,hl            ;DE=Low, HL=letztes
        call srtpar
        pop hl              ;HL=High
        pop de              ;DE=erstes
        jp srtpar
;IX=DE?IX=HL
srtpar6 push ix
        ex (sp),hl          ;HL=IX,(SP)=HL
        sbc hl,de
        pop hl
        ret nz
        push hl
        pop ix
        ret

;### SRTSWP -> Vertauscht zwei Zeilen
;### Eingabe    HL=Zeiger innerhalb Zeile 1, DE=Zeiger innerhalb Zeile 2
;### Veraendert AF,BC
srtswp  push de
        push hl
        ld bc,(srtrow)
        or a
        sbc hl,bc
        ex de,hl            ;DE=Zeilenanfang
        sbc hl,bc           ;HL=Zeilenanfang
        ld a,(srtsiz)
        ld b,a
        ld c,a
srtswp1 ld a,(de)
        ldi
        dec hl
        ld (hl),a
        inc hl
        djnz srtswp1
        pop hl
        pop de
        ret

;### SRTCMP -> Vergleicht zwei Elemente
;### Eingabe    (HL)=Element1, (IX)=Element2
;### Ausgabe    CF=1 -> (HL)<(IX), ZF=1 -> (HL)>(IX)
;### Veraendert A,BC
srtcmp  push de
        ld e,(hl)
        inc hl
        ld d,(hl)           ;DE=Element1
        dec hl
        ld c,(ix+0)
        ld b,(ix+1)         ;BC=Element2
        ld a,(srtdir)
        or a
        jr z,srtcmp7        ;Elemente tauschen, falls absteigende Sortierung
        ld a,c:ld c,e:ld e,a
        ld a,b:ld b,d:ld d,a
srtcmp7 ld a,(srttyp)
        cp 2
        jr nz,srtcmp2
        ex de,hl            ;*** 16Bit Wert [anstelle von Zeiger]
        or a
        sbc hl,bc
        ex de,hl
srtcmp0 pop de
        ret c               ;CF=1, ZF=0 -> (HL)<(IX)
        jr z,srtcmp1
        sub a               ;CF=0, ZF=1 -> (HL)>(IX)
        ret
srtcmp1 ld a,1
        or a                ;CF=0, ZF=0 -> (HL)=(IX)
        ret
srtcmp2 push ix             ;*** Text oder 32Bit
        db #dd:ld l,c
        db #dd:ld h,b       ;IX=Ele2
        or a
        jr nz,srtcmp5
        ld b,32             ;*** Text (maximal 32 Vergleiche)
srtcmp3 dec b
        jr z,srtcmp4
        ld a,(de)   
        cp (ix+0)
        inc de
        inc ix
        jr nz,srtcmp4
        or a
        jr nz,srtcmp3
srtcmp4 pop ix
        jr srtcmp0
srtcmp5 inc de:inc de:inc de
        ld b,4              ;*** 32Bit (maximal 4 Vergleiche)
srtcmp6 dec b
        jr z,srtcmp4
        ld a,(de)
        cp (ix+3)
        dec de
        dec ix
        jr z,srtcmp6
        jr srtcmp4


;==============================================================================
;### GRAFIK-ROUTINEN ##########################################################
;==============================================================================

gfxdatbnk   equ 0       ;Bank
gfxdatadr   equ 1       ;Adresse
gfxdatsiz   equ 3       ;Länge
gfxdatxln   equ 5       ;Xlen (1B)
gfxdatyln   equ 6       ;Ylen (1B)
gfxdatxps   equ 7       ;Xpos (1W)
gfxdatyps   equ 9       ;Ypos (1W)
gfxdattyp   equ 11      ;Typ (8=normal, 10=extended)
gfxdatlen   equ 16      ;Datensatzlänge

gfxgrpmax   equ 16
gfxgrpmem   ds gfxdatlen*gfxgrpmax

;### GFXOCP -> Advanced OCP Art Studio Grafik ("SCR", "PAL) laden, ggf. vorher alte entfernen
;### Eingabe    (gfxpth)=Pfad
;### Ausgabe    CF=0 ok, CF=1 Fehler (A=Grund -> 1=File, 2=Speicher)
gfxocplin ds 80     ;Zeilenbuffer beim Entpacken
gfxocpbuf ds 512    ;Ladebuffer
gfxocphnd db 0      ;File-Handler
gfxocpzle db 0,0    ;aktuelle Textzeile (0-24) und Rasterzeile (0-7)
gfxocppoi db 80,160,100:dw 0,0,80*100
          db 80,160,100:dw 0,0,80*100
          db 0
gfxocpgfx
dw gfxnxt,255*256+10,0,  0,  0,160,100,0  ;01 Grafik Teil 01
dw gfxnxt,255*256+10,0,160,  0,160,100,0  ;02 Grafik Teil 02
dw gfxnxt,255*256+10,0,  0,100,160,100,0  ;03 Grafik Teil 03
dw gfxnxt,255*256+10,0,160,100,160,100,0  ;04 Grafik Teil 04

gfxocpext ds 3
gfxocpcol db 094,255,100,160,028,088,064,124,255,255,132,188,060,116,096,152,255,255,072,128,000,056,036,092,058,130,102,158,030,086,066,122
gfxocppal ds 8
gfxocpcnv db 0

gfxocp  call gfxfre
        xor a
        ld (gfxocpcnv),a
        ld ix,gfxgrpmem+00      ;** Speicher reservieren
        call gfxocp1
        ret c
        ld ix,gfxgrpmem+16
        call gfxocp1
        ret c
        call gfxlodi
        ld hl,gfxpth            ;** PAL-File suchen
        call clcext
        jp z,gfxocpj
        ex de,hl
        push hl
        ld de,gfxocpext
        ld bc,3
        ldir
        ld bc,-3
        add hl,bc
        ld (hl),"p":inc hl
        ld (hl),"a":inc hl
        ld (hl),"l"
        xor a
        ld (gfxocpbuf),a
        ld hl,gfxpth            ;** PAL laden
        ld a,(prgbnknum)
        db #dd:ld h,a
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOPN
        ccf
        jp nc,gfxocpk
        ld de,(prgbnknum)
        ld hl,gfxocpbuf
        ld bc,239+128
        push af
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILINP
        pop af
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILCLO
        ld a,(gfxocpbuf+128)
        cp 1
        scf
        jr nz,gfxocpk
        ld ix,gfxocpbuf+3+128
        ld de,gfxocppal
        db #fd:ld l,4
gfxocpb ld a,4
        db #fd:sub l
        ld (de),a
        inc de
        ld a,(ix+0)
        and 31
        ld l,a
        ld h,0
        ld bc,gfxocpcol
        add hl,bc
        ldi
        ld bc,12
        add ix,bc
        db #fd:dec l
        jr nz,gfxocpb
        call gfxsrt
        ld ix,gfxocppal
        ld a,(ix+0):ld (ix+1),a:ld (ix+0),1
        ld a,(ix+2):ld (ix+3),a:ld (ix+2),3
        ld a,(ix+4):ld (ix+5),a:ld (ix+4),2
        ld a,(ix+6):ld (ix+7),a:ld (ix+6),0
        call gfxsrt
        call gfxcnv
        ld a,1
        ld (gfxocpcnv),a
        or a
gfxocpk pop de
        ld hl,gfxocpext
        ld bc,3
        ldir
        ld a,1
        ret c
gfxocpj ld hl,gfxpth            ;** File öffnen
        ld a,(prgbnknum)
        db #dd:ld h,a
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOPN
        ld (gfxocphnd),a
        ld a,1
        ret c
        call gfxocp8            ;** Kopf laden
        jr nc,gfxocp4
gfxocp3 ld a,(gfxocphnd)        ;** Fehler beim laden -> Ende
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILCLO
        call gfxfre
        scf
        ld a,1
        ret
gfxocp4 ld hl,0
        ld (gfxocpzle),hl
        ld de,(gfxocpbuf+#18)
        ld hl,#4000
        sbc hl,de
        ld de,gfxocpbuf+128
        ld hl,512-128       ;DE=Buffer, HL=verbliebende Bytes
        jp nz,gfxocpc
gfxocp5 ld bc,80                ;### NICHT KOMPRIMIERT
        or a
        sbc hl,bc           ;test, ob noch eine komplette Zeile im Buffer vorliegt
        jr c,gfxocp6
        push hl                 ;** komplette Zeile liegt vor -> übernehmen
        push de
        ex de,hl
        call gfxlin
        pop de
        push af
        ld hl,80
        add hl,de
        ex de,hl
        pop af
        pop hl
        jr c,gfxocp9        ;noch nicht letzte Zeile erreicht -> weitermachen
gfxocpd jr z,gfxocp5
        ld bc,#800-2000     ;25 Zeile erreicht -> Sprung von 2048-2000 Bytes
        sbc hl,bc
        jr nc,gfxocpe
        push hl
        call gfxocp8
        pop hl
        jr c,gfxocp3
        ld c,l
        ld b,h
        ld a,l:cpl:ld l,a
        ld a,h:cpl:ld h,a
        inc hl
        ld de,gfxocpbuf
        add hl,de
        ex de,hl
        ld hl,512
        add hl,bc
        jr gfxocp5
gfxocpe ex de,hl
        add hl,bc
        ex de,hl
        jr gfxocp5          ;letzte Zeile erreicht -> fertig
gfxocp6 or a                    ;** komplette Zeile liegt nicht vor
        adc hl,bc
        ld c,l
        ld b,h
        ex de,hl
        ld de,gfxocplin     ;DE=Ziel in Zeilenbuffer
        jr z,gfxocp7
        push bc
        ldir
        pop bc
gfxocp7 push de             ;Ziel merken
        ld hl,80
        or a
        sbc hl,bc
        push hl             ;noch fehlende Bytes für Zeile merken
        call gfxocp8
        pop bc
        pop de
        jp c,gfxocp3
        ld hl,gfxocpbuf
        push bc
        ldir
        push hl
        ld hl,gfxocplin
        call gfxlin
        pop de
        pop bc
        jr c,gfxocp9
        push af
        ld hl,512
        sbc hl,bc           ;HL=verbliebende Bytes
        pop af
        jr gfxocpd
gfxocpc xor a                   ;### KOMPRIMIERT
        call gfxdcr
        jp c,gfxocp3

gfxocp9 ld a,(gfxocphnd)        ;** File schließen
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILCLO
        ld hl,gfxocpgfx     ;Grafikdaten in Anzeige eintragen
        ld de,prgwinobj+16
        ld bc,4*16
        ldir
        ld ix,gfxgrpmem
        ld iy,prgwinobj+16
        ld a,2
gfxocpa ld l,(ix+gfxdatbnk)
        ld (iy+3),l
        ld (iy+3+16),l
        ld l,(ix+gfxdatadr+0)
        ld h,(ix+gfxdatadr+1)
        ld (iy+4),l
        ld (iy+5),h
        ld bc,9
        add hl,bc
        ld (iy+4+16),l
        ld (iy+5+16),h
        ld bc,16
        add ix,bc
        ld bc,32
        add iy,bc
        dec a
        jr nz,gfxocpa
        ld a,5
        ld (prgwingrp),a
        ld hl,320           ;Pfad und Größe eintragen
        ld (gfxlodm),hl
        ld hl,200
        ld (gfxlodn),hl
        call gfxlodj
        xor a
        ret
gfxocp8 ld a,(prgbnknum)        ;** 512Byte laden
        ld e,a
        ld a,(gfxocphnd)
        ld hl,gfxocpbuf
        ld bc,512
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILINP
        ret
gfxocp1 push ix                 ;** 8K-Speicher reservieren
        ld e,1
        xor a
        ld bc,8000+19
        rst #20:dw jmp_memget
        pop ix
        jr c,gfxocp2
        ld (ix+gfxdatbnk),a
        ld (ix+gfxdatadr+0),l
        ld (ix+gfxdatadr+1),h
        ld (ix+gfxdatsiz+0),#53     ;8019=#1F53
        ld (ix+gfxdatsiz+1),#1f
        ex de,hl
        ld hl,18
        add hl,de
        ld (gfxocppoi+5+0),hl
        ld (gfxocppoi+5+9),hl
        inc hl
        ld (gfxocppoi+3+0),hl
        ld bc,40
        add hl,bc
        ld (gfxocppoi+3+9),hl
        ld hl,prgbnknum
        add a:add a:add a:add a
        or (hl)
        ld hl,gfxocppoi
        ld bc,19
        rst #20:dw jmp_bnkcop
        ret
gfxocp2 call gfxfre
        ld a,2
        scf
        ret

;### GFXDCR -> Lädt gecrunchtes OCP-File
;### Eingabe    DE=Buffer-Position, HL=Buffer-Rest
;### Ausgabe    CF=0 ok, CF=1 Fehler beim Laden
gfxdcrbzg   dw 0    ;Position im Buffer
gfxdcrbln   dw 0    ;Buffer-Rest
gfxdcrzln   dw 0    ;Länge in Zielzeile
gfxdcrblk   dw 0    ;Länge des entpackten Blockes
gfxdcrs dw 0
gfxdcr  ld (gfxdcrs),sp
        ld (gfxdcrbzg),de
        ld (gfxdcrbln),hl
        xor a
        ld (gfxdcrzln),a
        ld (gfxdcrj),a
gfxdcr1 call gfxdcrg:cp "M":scf:ret nz
        call gfxdcrg:cp "J":scf:ret nz
        call gfxdcrg:cp "H":scf:ret nz
        call gfxdcrg
        push af
        call gfxdcrg
        pop hl
        ld l,h
        ld h,a
        ld (gfxdcrblk),hl
gfxdcr2 call gfxdcrg
        cp 1
        jr z,gfxdcr3
        call gfxdcrp
        jr gfxdcr5
gfxdcr3 call gfxdcrg
        push af
        call gfxdcrg
        pop bc
        ld c,a
gfxdcr4 push bc
        ld a,c
        call gfxdcrp
        pop bc
        jr c,gfxdcr5
        djnz gfxdcr4
gfxdcr5 jr c,gfxdcr6
        jr nz,gfxdcr1
        jr gfxdcr2
gfxdcr6 xor a
        ret

;### GFXDCRE -> Verläßt bei Fehler Decrunch-Routine komplett
gfxdcre ld hl,(gfxdcrs)
        ld sp,hl
        scf
        ret

;### GFXDCRG -> Holt Byte aus komprimierten Daten
;### Ausgabe    A=Byte
gfxdcrg ld hl,(gfxdcrbln)
        dec hl
        ld (gfxdcrbln),hl
        ld a,l
        or h
        ld hl,(gfxdcrbzg)
        ld a,(hl)
        inc hl
        ld (gfxdcrbzg),hl
        ret nz
        push af
        call gfxocp8
        jr c,gfxdcre
        ld hl,gfxocpbuf
        ld (gfxdcrbzg),hl
        ld hl,512
        ld (gfxdcrbln),hl
        pop af
        ret

;### GFXDCRP -> Schreibt dekomprimiertes Byte
;### Eingabe    A=Byte
;### Ausgabe    CF=1 Ende erreicht, ZF=0 Ende von Block erreicht
gfxdcrj db 0
gfxdcrp ld e,a
        ld a,(gfxdcrj)
        sub 1
        jr c,gfxdcrr
        ld (gfxdcrj),a
        jr gfxdcrq
gfxdcrr ld hl,(gfxdcrzln)
        ld a,l
        ld bc,gfxocplin
        add hl,bc
        ld (hl),e
        inc a
        ld (gfxdcrzln),a
        cp 80
        jr c,gfxdcrq
        xor a
        ld (gfxdcrzln),a
        ld l,c
        ld h,b
        call gfxlin
        ret c
        jr z,gfxdcrq
        ld a,48
        ld (gfxdcrj),a
gfxdcrq ld hl,(gfxdcrblk)
        dec hl
        ld (gfxdcrblk),hl
        ld a,l
        or h
        jr z,gfxdcrt
        xor a
        ret
gfxdcrt inc a
        ret

;### GFXSRT -> Sortiert (gfxocppal) nach 2.Parameter aufsteigend
gfxsrt  ld c,4
gfxsrt1 ld ix,gfxocppal
        ld b,3
gfxsrt3 ld a,(ix+1)
        cp (ix+3)
        jr c,gfxsrt2
        ld l,(ix+0)
        ld h,(ix+1)
        ld e,(ix+2)
        ld d,(ix+3)
        ld (ix+0),e
        ld (ix+1),d
        ld (ix+2),l
        ld (ix+3),h
gfxsrt2 inc ix
        inc ix
        djnz gfxsrt3
        dec c
        jr nz,gfxsrt1
        ret

;### GFXCNV -> Precalculation für das Konvertieren
gfxcnvb ds 256
gfxcnv  ld hl,gfxcnvb
        xor a
gfxcnv1 ld (hl),a
        inc hl
        inc a
        jr nz,gfxcnv1
        dec h
        db #fd:ld l,a           ;Farben konvertieren
gfxcnv3 ld a,(hl)
        ld e,a
        ld d,0
        db #fd:ld h,4
gfxcnv4 ld a,e
        rlca
        and 1
        bit 3,e
        jr z,gfxcnv5
        add 2
gfxcnv5 add a
        ld c,a
        ld b,0
        ld ix,gfxocppal
        add ix,bc
        ld a,(ix+0)
        add a:add a:add a
        bit 4,a
        res 4,a
        jr z,gfxcnv6
        add 128
gfxcnv6 or d
        rlca
        ld d,a
        rlc e
        db #fd:dec h
        jr nz,gfxcnv4
        ld (hl),d
        inc hl
        db #fd:dec l
        jr nz,gfxcnv3
        ret

;### GFXLIN -> Konvertiert Farben einer Zeile und kopiert sie in den Zielspeicher
;### Eingabe    HL=Adresse, (gfxocpzle)=aktuelle Textzeile, (gfxocpzle+1)=aktuelle Rasterzeile
;### Ausgabe    (gfxocpzle+0/1)=nächste Zeile, CF=1 letzte Zeile wurde geschrieben, ZF=0 25er Wechsel findet statt
gfxlin  push hl
        ld a,(gfxocpcnv)
        or a
        jr z,gfxlin7
        ex de,hl
        ld bc,gfxcnvb
        db #dd:ld l,80
gfxlin3 ld a,(de)
        ld l,a
        ld h,0
        add hl,bc
        ldi
        inc bc
        db #dd:dec l
        jr nz,gfxlin3
gfxlin7 pop de                  ;Zieladresse berechnen
        ld a,(gfxocpzle)
        add a:add a:add a
        ld hl,gfxocpzle+1
        add (hl)
        ld ix,gfxgrpmem+00
        cp 100
        jr c,gfxlin1
        ld ix,gfxgrpmem+16
        sub 100
gfxlin1 add a
        ld l,a
        ld h,0
        ld c,l
        ld b,h
        add hl,hl
        add hl,hl
        add hl,bc       ;HL=HL*10
        add hl,hl
        add hl,hl
        add hl,hl       ;HL=HL*80
        ld c,(ix+gfxdatadr+0)
        ld b,(ix+gfxdatadr+1)
        add hl,bc
        ld bc,19
        add hl,bc
        ex de,hl        ;DE=Ziel
        ld a,(ix+gfxdatbnk)
        add a:add a:add a:add a
        ld c,a
        ld a,(prgbnknum)
        or c
        ld bc,80
        rst #20:dw jmp_bnkcop   ;Zeile kopieren
        ld hl,gfxocpzle
        ld a,(hl)
        inc a
        cp 25
        jr nc,gfxlin2
        ld (hl),a
        xor a
        ret
gfxlin2 ld (hl),0
        inc hl
        ld a,(hl)
        inc a
        and 7
        scf
        ret z
        ld (hl),a
        or a
        ret

;### GFXEXT -> Lädt "extended" Teil einer SymbOS-SGX-Grafik
gfxexttyp   equ 1   ;bit0-1=encoding type [0=cpc, 1=msx]; bit2-3=colour depth [0=4 colours, 1=16 colours]
gfxextbyt   equ 2   ;width in bytes (currently 255 max!)
gfxextxln   equ 4   ;width in pixel
gfxextyln   equ 6   ;height in pixel

gfxextymx   db 0,0  ;maximale Ylen
gfxexthed   ds 10   ;Header für extended Gfx
gfxextxps   dw 0
gfxextyps   dw 0

gfxexthds   dw 0    ;size of all headers for one 16k block

gfxext  push bc
        push ix
        ld a,(prgbnknum)
        ld e,a
        ld a,c
        ld hl,gfxlodb+3
        ld bc,5
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILINP
        pop ix
        pop bc
        ld a,1
        jp c,gfxlod5
        jp nz,gfxlod5
        ld a,(gfxlodb+gfxextbyt+1)
        or a
        ld a,1
        jp nz,gfxlod4               ;Grafik zu breit (>255) -> Fehler

        push bc
        ld a,(gfxlodb+gfxexttyp)
        cp 5
        ld de,63
        jr c,gfxext9
        ld e,126                    ;de=max XBytes
gfxext9 ld bc,(gfxlodb+gfxextbyt)
        call clcd16                 ;xbytes / xmax per xblock
        ld a,e
        or d
        jr z,gfxexta
        inc l                       ;l=number of x-blocks (1-5)
gfxexta ex de,hl
        ld a,10
        call clcm16
        ld (gfxexthds),hl
        pop bc

        ld hl,(gfxlodx)
        ld (gfxextxps),hl
        ld hl,(gfxlody)
        ld (gfxextyps),hl           ;Pos für nächste Grafik vormerken
        push bc
        ld hl,16384-256
        ld bc,(gfxexthds)
        or a
        sbc hl,bc                   ;hl=size of all headers -> 16384-256-headersize = maximum allowed datablock (-256 because of kernel code at #ff00)
        ld c,l
        ld b,h
        ld de,(gfxlodb+gfxextbyt)
        call clcd16                 ;maximale Ylen ausrechnen
        pop bc
        inc h:dec h
        ld a,-1
        jr nz,gfxext1
        ld a,l
gfxext1 ld (gfxextymx),a
gfxext2 ld hl,(gfxlodb+gfxextyln)   ;*** 16K-Block-Loop
        ld a,l
        or h
        jp z,gfxext8
        push bc
        ld a,(gfxextymx)
        inc h:dec h
        jr nz,gfxext3
        cp l
        jr c,gfxext3
        ld a,l
gfxext3 ld (gfxexthed+2),a          ;aktuelle Ylen merken
        ld e,a
        ld d,0
        or a
        sbc hl,de
        ld (gfxlodb+gfxextyln),hl
        ld a,(gfxlodb+gfxextbyt)
        call clcm16
        push hl
        ld (gfxexthed+7),hl
        ld bc,(gfxexthds)
        add hl,bc
        ld (ix+gfxdatsiz+0),l
        ld (ix+gfxdatsiz+1),h       ;Bytegröße eintragen
        ld c,l
        ld b,h
        xor a
        ld e,1
        rst #20:dw jmp_memget       ;Speicher reservieren
        pop de
        pop bc
        db #fd:ld l,a
        ld a,2
        jp c,gfxlod5                ;kein Speicher frei -> Abbruch
        db #fd:ld a,l
        ld (ix+gfxdatbnk),a
        ld (ix+gfxdatadr+0),l
        ld (ix+gfxdatadr+1),h       ;eintragen
        push ix
        push bc
        push de
        ld de,(gfxexthds)
        add hl,de
        ld e,a
        ld a,c
        pop bc
        push hl

        call gfxprt                 ;Daten laden

        pop hl
        pop bc
        pop ix
        ld a,1
        jp c,gfxlod5
        jp nz,gfxlod5
        dec hl                  ;*** Header schreiben
        push bc
        db #fd:ld h,b
        ld (gfxexthed+5),hl
        ld a,(ix+gfxdatbnk)
        ld bc,(gfxlodb+gfxexttyp-1)
        rst #20:dw jmp_bnkwbt       ;Typ eintragen
        ld c,l
        ld b,h                      ;BC=Adr Data
        ld de,(gfxexthds)
        or a
        sbc hl,de                   ;HL=Adr Köpfe
        ld de,(gfxlodb+gfxextbyt)   ;E=gesamt Xbytes
        ld a,(gfxlodb+gfxexttyp)
        cp 5
        ld d,63
        jr c,gfxext4
        ld d,126                    ;D=max XBytes
        ld (gfxlodt),a
gfxext4 ld a,e                  ;*** X-Block-Loop
        cp d
        jr c,gfxext5
        ld a,d
gfxext5 add a
        bit 6,d
        jr nz,gfxext6
        add a
gfxext6 ld (gfxexthed+1),a          ;xlen in pixel
        ld (ix+gfxdatxln),a
        ld a,(gfxlodb+gfxextbyt)    ;xlen in bytes
        ld (gfxexthed+0),a
        ld a,(gfxexthed+2)          ;ylen in pixel
        ld (ix+gfxdatyln),a
        ld (ix+gfxdattyp),10        ;control = gfxextended
        ld (gfxexthed+3),bc         ;set gfxdata adr with offset
        push de
        push hl
        push bc
        ex de,hl
        ld a,(ix+gfxdatbnk)
        ld (ix+gfxdatbnk+gfxdatlen),a
        add a:add a:add a:add a
        ld hl,prgbnknum
        add (hl)
        ld hl,gfxexthed
        ld bc,9
        rst #20:dw jmp_bnkcop       ;copy header
        call gfxlodc
        ld bc,gfxdatlen
        add ix,bc
        pop bc
        pop hl
        ld de,10
        add hl,de
        ld (ix+gfxdatadr+0),l
        ld (ix+gfxdatadr+1),h
        ld (ix+gfxdatsiz+0),0
        ld (ix+gfxdatsiz+1),0
        pop de
        ld a,d
        add c
        ld c,a
        ld a,0
        adc b
        ld b,a
        db #fd:dec h
        jr z,gfxext7
        ld a,e
        sub d
        jr z,gfxext7
        ld e,a
        jr nc,gfxext4
gfxext7 ld hl,(gfxextxps)
        ld (gfxlodx),hl
        ld hl,(gfxlody)
        ld bc,(gfxextymx)
        add hl,bc
        ld (gfxlody),hl
        pop bc
        db #fd:ld b,h
        inc b:dec b
        jp nz,gfxext2
gfxext8 ld hl,(gfxextxps)
        ld de,(gfxlodb+gfxextxln)
        add hl,de
        ld (gfxlodx),hl
        ld hl,(gfxextyps)
        ld (gfxlody),hl
        jp gfxlod3

;### GFXLOD -> Neue SymbOS-SGX-Grafik laden, ggf. vorher alte entfernen
;### Eingabe    (gfxpth)=Pfad
;### Ausgabe    CF=0 ok, CF=1 Fehler (A=Grund -> 1=File, 2=Speicher)
gfxlodp db 0    ;0=uncompressed
gfxlodx dw 0    ;aktuelle Xp
gfxlody dw 0    ;aktuelle Yp
gfxlodm dw 0    ;maximale Y-Ausdehnung (für Zeilenforschub)
gfxlodn dw 0    ;maximale Y-Ausdehnung (für Zeilenforschub)
gfxlodb ds 8    ;Buffer für Grafik-Header/Steuercode
gfxlodt db 0    ;0=max 4 Farben, >0=max 16 Farben
gfxlodu db " colours)",0

gfxlod  call gfxfre
        xor a
        ld (gfxlodt),a
        ld l,a
        ld h,a
        ld (gfxlodx),hl
        ld (gfxlody),hl
        ld (gfxlodm),hl
        ld (gfxlodn),hl
        ld hl,gfxpth
        ld a,(prgbnknum)
        db #dd:ld h,a
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOPN
        ld c,a
        ld a,1
        ret c

        ld b,gfxgrpmax
        ld ix,gfxgrpmem
gfxlod1 push bc
        push ix
        ld a,(prgbnknum)
        ld e,a
        ld a,c

gfxlods ld hl,gfxlodb
        ld bc,3
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILINP

        pop ix
        pop bc
        jp c,gfxlod4            ;kein weiterer Block ladbar -> fertig
        jp nz,gfxlod4
        ld hl,gfxlodb
        ld a,(hl)               ;Block-Code auswerten
        inc a
        jp z,gfxlodr            ;**** line feed
        dec a
        jp z,gfxlod4            ;**** Dateiende -> fertig
        res 7,(hl)
        ld hl,gfxlodp
        ld (hl),0
        bit 7,a
        jr z,gfxlod2
        inc (hl)
        res 7,a
gfxlod2 cp 64
        jp z,gfxext             ;**** Grafik "extended" laden
        jr nc,gfxlod3           ;unknown -> next

        ld (ix+gfxdattyp),8     ;**** Grafik "normal" laden
        ld hl,(gfxlodb+1)
        ld e,a
        add a
        add a
        cp l
        jp nz,gfxlod4
        ld a,e
gfxlodq ld (ix+gfxdatxln),l
        ld (ix+gfxdatyln),h     ;Maße eintragen
        ld e,h
        ld d,0
        call clcm16
        ld de,3
        add hl,de
        push hl
        ld (ix+gfxdatsiz+0),l
        ld (ix+gfxdatsiz+1),h   ;Bytegröße eintragen
        call gfxlodc
        pop hl
        push bc
        ld c,l
        ld b,h
        xor a
        ld e,1
        rst #20:dw jmp_memget   ;** Speicher reservieren
        pop bc
        ld e,a
        ld a,2
        jr c,gfxlod5            ;kein Speicher frei -> Abbruch
        ld (ix+gfxdatbnk),e
        ld (ix+gfxdatadr+0),l
        ld (ix+gfxdatadr+1),h   ;eintragen
        push bc
        push ix
        ld a,c
        push af
        ld a,e                  ;* Kopf normal
        ld bc,(gfxlodb)
        rst #20:dw jmp_bnkwwd
        ld bc,(gfxlodb+1)
        rst #20:dw jmp_bnkwbt   ;Kopf in Speicher eintragen
        ld c,(ix+gfxdatsiz+0)
        ld b,(ix+gfxdatsiz+1)
        ld a,-3
        add c
        ld c,a
        ld a,-1
        adc b
        ld b,a
        pop af
        call gfxprt             ;** Daten laden
        pop ix
        pop bc
        ld a,1
        jr c,gfxlod5            ;Fehler beim Laden -> Abbruch
        jr nz,gfxlod5
gfxloda dec b
        ld de,gfxdatlen
        add ix,de
gfxlod3 inc b
        dec b
        jp nz,gfxlod1
gfxlod4 ld a,0
gfxlod5 ld d,a                  ;D=Error-Code
        ld a,16
        sub b 
        ld e,a                  ;E=Anzahl Grafiken
        jr nz,gfxlod6
        ld d,1                  ;keine Grafiken geladen -> Fehler
gfxlod6 push de
        ld a,c
        call syscll             ;Datei schließen
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILCLO
        pop de
        inc d
        dec d
        jr z,gfxlod7
        push de                 ;**** Fehler
        call gfxfre             ;Speicher freigeben
        pop af
        scf
        ret
gfxlod7 push de                 ;**** erfolgreich geladenen Grafik initialisieren
        call gfxlodj
        call gfxlodi
        pop bc                  ;Grafikdaten für Anzeige eintragen
        ld b,c
        ld a,c
        ld ix,gfxgrpmem
        ld iy,prgwinobj+16
gfxlod9 ld c,(ix+gfxdattyp)  :ld (iy+02),c
        ld c,(ix+gfxdatbnk)  :ld (iy+03),c
        ld c,(ix+gfxdatadr+0):ld (iy+04),c
        ld c,(ix+gfxdatadr+1):ld (iy+05),c
        ld c,(ix+gfxdatxps+0):ld (iy+06),c
        ld c,(ix+gfxdatxps+1):ld (iy+07),c
        ld c,(ix+gfxdatyps+0):ld (iy+08),c
        ld c,(ix+gfxdatyps+1):ld (iy+09),c
        ld c,(ix+gfxdatxln)  :ld (iy+10),c
        ld c,(ix+gfxdatyln)  :ld (iy+12),c
        ld de,gfxdatlen
        add ix,de
        ld de,16
        add iy,de
        djnz gfxlod9
        inc a
        ld (prgwingrp),a        ;Grafikelemente anzeigen
        xor a
        ret
gfxlodj ld hl,gfxpth            ;### Grafikpfad und Größen-Anzeigen erzeugen
        ld de,prgwinsta2
        ld bc,-1
gfxlode ld a,(hl)
        or a
        jr z,gfxlodf
        ldi
        djnz gfxlode
gfxlodf push de
        pop iy
        ld (iy+0)," "
        ld (iy+1),"("
        inc iy:inc iy
        push iy
        ld de,0
        ld ix,(gfxlodm)
        ld (prgwindat+16),ix    ;Grafik-Größe
        call clcn32
        ld (iy+1)," "
        ld (iy+2),"x"
        ld (iy+3)," "
        inc iy:inc iy:inc iy:inc iy
        ld de,0
        ld ix,(gfxlodn)
        ld (prgwindat+18),ix
        call clcn32
        ld (iy+1),","
        ld (iy+2)," "
        ld (iy+3),"4"
        ld a,(gfxlodt)
        or a
        jr z,gfxlodl
        ld (iy+3),"1"
        ld (iy+4),"6"
        inc iy
gfxlodl db #fd:ld e,l
        db #fd:ld d,h
        inc de:inc de:inc de:inc de
        ld hl,gfxlodu
        ld bc,10
        ldir
        pop hl
        ld de,prgobjtxt1b
gfxlodg ld a,(hl)
        cp ")"
        jr z,gfxlodh
        ldi
        jr gfxlodg
gfxlodh ld hl,prgobjtxt1c
        ld bc,7
        ldir
        ld hl,prgwinsta1
        ld (prgwindat+32),hl    ;Status = Grafikpfad
        ret
gfxlodi ld hl,gfxgrpmem         ;### die ersten 8 reservierten Speicherbereiche in Programmkopf eintragen
        push hl:pop ix
        ld de,prgmemtab
        ld iy,8*256+gfxgrpmax
gfxlod8 ld a,(ix+gfxdatbnk)
        or a
        jr z,gfxlodk
        ld a,(ix+gfxdatsiz+0)
        or (ix+gfxdatsiz+1)
        jr z,gfxlodk
        ld bc,5
        push hl
        ldir
        pop hl
gfxlodk ld bc,gfxdatlen
        add hl,bc
        add ix,bc
        db #fd:dec l
        ret z
        db #fd:dec h
        jr nz,gfxlod8
        ret

gfxlodr ld hl,0                 ;**** Zeilenvorschub
        ld (gfxlodx),hl
        ld hl,(gfxlodn)
        ld (gfxlody),hl
        jp gfxlod3

;ix+gfxdatxln=Xlen, ix+gfxdatyln=Ylen
gfxlodc ld hl,(gfxlodx)
        ld (ix+gfxdatxps+0),l
        ld (ix+gfxdatxps+1),h   ;X-Position eintragen
        ld e,(ix+gfxdatxln)
        ld d,0
        add hl,de
        ld (gfxlodx),hl         ;nächste Xpos eintragen
        ex de,hl
        ld hl,(gfxlodm)
        sbc hl,de
        jr nc,gfxlodd
        ld (gfxlodm),de         ;rechte X-Grenze eintragen
gfxlodd ld hl,(gfxlody)
        ld (ix+gfxdatyps+0),l
        ld (ix+gfxdatyps+1),h   ;Y-Position eintragen
        ld e,(ix+gfxdatyln)
        ld d,0
        add hl,de
        ex de,hl
        ld hl,(gfxlodn)
        sbc hl,de
        ret nc
        ld (gfxlodn),de         ;untere Y-Grenze eintragen
        ret

;### GFXPRT -> lädt grafikteil und depackt optional daten
;### Eingabe    A=handler, E=bank, HL=ziel, BC=original länge, (gfxlodp)=0 -> uncompressed
;### Ausgabe    CF=0 ok, CF=1 Fehler (A=code)
;### Destroyed  AF,BC,DE,HL,IX,IY
gfxprt  ld d,a
        ld a,(gfxlodp)
        sub 1
        ccf
        ld a,d
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILCPR
        ret


;### GFXFRE -> Aktuelle Grafik entfernen bzw. Speicher freigeben
gfxfre  xor a
        ld (prgobjtxt1b),a
        inc a
        ld (prgwingrp),a        ;keine Grafiken anzeigen
        ld hl,prgwinsta0
        ld (prgwindat+32),hl    ;Status = "No Gfx"
        ld hl,157
        ld (prgwindat+16),hl
        ld hl,24
        ld (prgwindat+18),hl
        ld b,gfxgrpmax          ;Speicher freigeben
        ld hl,gfxgrpmem
gfxfre1 push bc
        push hl
        ld a,(hl):ld (hl),0:inc hl
        ld e,(hl):ld (hl),0:inc hl
        ld d,(hl):ld (hl),0:inc hl
        ld c,(hl):ld (hl),0:inc hl
        ld b,(hl):ld (hl),0:inc hl
        or a
        jr z,gfxfre2
        ld hl,0
        adc hl,de
        jr z,gfxfre2
        rst #20:dw jmp_memfre
gfxfre2 pop hl
        ld bc,gfxdatlen
        add hl,bc
        pop bc
        djnz gfxfre1
        ld hl,prgmemtab
        ld de,prgmemtab+1
        ld (hl),0
        ld bc,8*5-1
        ldir
        ret

;### GFXSEL -> Ausgewählte Grafik laden und anzeigen
;### Eingabe    (gfxpth)=Pfad
gfxsel  call gfxsel2
        jp prgprz0
gfxsel2 ld hl,gfxpth            ;** Filetyp suchen
        call clcext
        jr z,gfxsel3
        ld a,(de):call clclcs:cp "s":jr nz,gfxsel3:inc de
        ld a,(de):call clclcs:cp "g":jr nz,gfxsel3:inc de
        ld a,(de):call clclcs:cp "x":jr nz,gfxsel3
        call gfxlod             ;** SymbOS-SGX-Grafik laden
        jr gfxsel4
gfxsel3 call gfxocp             ;** OCP- oder RAW-Grafik laden
gfxsel4 push af
        ld c,MSC_DSK_WINSTA     ;Infos anzeigen
        call gfxsel0
        ld hl,0                 ;Slider resetten
        ld (prgwindat+12),hl
        ld (prgwindat+14),hl
        ld c,MSC_DSK_WINSLD
        call gfxsel0
        ld e,-1
        ld c,MSC_DSK_WINDIN     ;Grafik anzeigen
        call gfxsel0
        pop af
        push af
        call nc,dirnew          ;Directory ggf. neu einlesen
        pop af
        ret nc
        cp 1                    ;falls Error, Message ausgeben
        ld hl,prgmsgerr2a
        jr z,gfxsel1
        ld hl,prgmsgerr2b
gfxsel1 ld (prgmsgerra),hl
        ld b,1
        ld hl,prgmsgerr
        jp prginf1
gfxsel0 ld a,(prgwin)           ;Message an Desktop
        ld b,a
        jp msgsnd

;### GFXFUL -> Zwischen Fullscreen und Normal wechseln
gfxfulb db 0:ds 8               ;Größe+Pos von Fenster in "wiederhergestelltem" Zustand
gfxfulf db 0
gfxful  call gfxful0
        jp prgprz0
gfxful0 ld a,(prgwindat)
        cp 2
        jr nz,gfxful1
        ld (gfxfulb),a          ;*** Fenster war maximiert         -> Fullscreen ein
        ld (gfxfulf),a
        ld hl,prgwindat+4
        ld de,gfxfulb+1
        ld bc,8
        ldir                    ;wiederhergestellt-Daten sichern
        ld hl,-1
        ld (prgwindat+4),hl
        ld hl,-25
        ld (prgwindat+6),hl
        ld hl,(prgwindat+46)
        ld bc,8
        add hl,bc
        ld (prgwindat+8),hl
        ld hl,(prgwindat+48)
        ld bc,24+18
        add hl,bc
        ld (prgwindat+10),hl    ;wiederhergestellt-Daten auf Fullscreen setzen
        ld c,MSC_DSK_WINMID
        jp gfxsel0              ;Fenster wiederherstellen
gfxful1 ld c,MSC_DSK_WINMAX     ;*** Fenster war wiederhergestellt -> Fullscreen aus
        call gfxsel0            ;Fenster maximieren
        xor a
        ld (gfxfulf),a
        ld a,(gfxfulb)     
        or a
        jp z,prgprz0
        ld de,prgwindat+4
        ld hl,gfxfulb+1
        ld bc,8
        ldir                    ;wiederhergestellt-Daten sichern
        ret

;### GFXPRV -> Vorherige Grafik anzeigen
gfxprv  call dirprv
        jp c,prgprz0
        jp gfxsel

;### GFXNXT -> Nächste Grafik anzeigen
gfxnxt  call dirnxt
        jp c,prgprz0
        jp gfxsel

;### GFXSUP -> Grafik rauf scrollen
gfxsup  ld c,MSC_DSK_WINMVY
        ld hl,(prgwindat+14)
        jr gfxslf0

;### GFXSDW -> Grafik runter scrollen
gfxsdw  db #fd:ld l,MSC_DSK_WINMVY
        ld ix,(prgwindat+14)    ;IX=aktuelle Pos
        ld hl,(prgwindat+18)    ;HL=Gesamtlänge
        ld de,(prgwindat+48)    ;DE=Angezeigte Länge
        jr gfxsrg0

;### GFXSLF -> Grafik links scrollen
gfxslf  ld c,MSC_DSK_WINMVX
        ld hl,(prgwindat+12)
gfxslf0 ld de,16
        or a
        sbc hl,de
        jr nc,gfxslf1
        ld hl,0
gfxslf1 ex de,hl
        call gfxsel0
        jp prgprz0

;### GFXSRG -> Grafik rechts scrollen
gfxsrg  db #fd:ld l,MSC_DSK_WINMVX
        ld ix,(prgwindat+12)    ;IX=aktuelle Pos
        ld hl,(prgwindat+16)    ;HL=Gesamtlänge
        ld de,(prgwindat+46)    ;DE=Angezeigte Länge
gfxsrg0 or a
        sbc hl,de
        jp c,prgprz0
        ex de,hl            ;DE=Maxpos
        push ix
        pop hl
        ld bc,16
        add hl,bc
        sbc hl,de
        jr c,gfxsrg1
        ld hl,0
gfxsrg1 add hl,de
        ex de,hl
        db #fd:ld c,l
        call gfxsel0
        jp prgprz0


;==============================================================================
;### OPTION-ROUTINEN ##########################################################
;==============================================================================

;### OPTOPN -> Öffnet Option-Dialog
optopn  ld a,64                 ;Button zunächst ausblenden
        ld (prgdatopt1+2),a
        ld iy,prgwinobj+16+6
        ld a,(cfgcpctyp)        ;Test, ob sich Grafik als Hintergrund eignet
        and #60
        jp z,optopn6
        cp #20
        jp nz,optopn1
        ld a,(prgwingrp)            ;** 512x212 ?
        cp 8
        jp z,optopn9
        cp 5
        jp nz,optopn1
        ld hl,0
        ld de,0
        ld bc,152
        ld ix,212
        call optopn2
        jp nz,optopn1
        ld iy,prgwinobj+32+6
        ld hl,152
        ld de,0
        ld bc,152
        ld ix,212
        call optopn2
        jp nz,optopn1
        ld iy,prgwinobj+48+6
        ld hl,152*2
        ld de,0
        ld bc,152
        ld ix,212
        call optopn2
        ld iy,prgwinobj+64+6
        jr optopn8
optopn9 ld hl,0
        ld de,0
        ld bc,152
        ld ix,211
        call optopn2
        jr nz,optopn1
        ld iy,prgwinobj+48+6
        ld hl,152
        ld de,0
        ld bc,152
        ld ix,211
        call optopn2
        jr nz,optopn1
        ld iy,prgwinobj+80+6
        ld hl,152*2
        ld de,0
        ld bc,152
        ld ix,211
        call optopn2
        ld iy,prgwinobj+112+6
optopn8 jr nz,optopn1
        ld hl,152*3
        ld bc,56
        ld ix,212
        jr optopn7

optopn6 ld a,(prgwingrp)            ;** 320x200 ?
        cp 3
        jr nz,optopn1
        ld hl,0
        ld de,0
        ld bc,160
        ld ix,200
        call optopn2
        jr nz,optopn1
        ld iy,prgwinobj+32+6
        ld hl,160
        ld bc,160
        ld ix,200
optopn7 ld de,0
        call optopn2
        jr nz,optopn1
        ld a,16                 ;Anzahl, Größe und Position stimmen -> Button einblenden
        ld (prgdatopt1+2),a

optopn1 ld a,(dirlen)           ;Directory setzen
        or a
        jr z,optopn4
        ld hl,dirpth
        ld de,sldinppth
        ld bc,255
        ldir
        jr optopn5
optopn4 ld hl,256*":"+"a"
        ld (sldinppth+0),hl
        ld (sldinppth+2),a
optopn5 ld ix,prgobjopt3b
        call strinp
        ld de,prgwinopt         ;Fenster öffnen
        call diainp
        jp prgprz0
optopn2 call optopn3
        ret nz
        ex de,hl
        call optopn3
        ret nz
        ld l,c
        ld h,b
        call optopn3
        ret nz
        push ix
        pop hl
optopn3 push bc
        ld c,(iy+0)
        ld b,(iy+1)
        inc iy:inc iy
        or a
        sbc hl,bc
        pop bc
        ret

;### OPTBGR -> Grafik als Hintergrund übernehmen
optbgr  ld a,-1
        ld (gfxpth-1),a
        ld hl,jmp_sysinf 
        ld de,256*33+6
        ld ix,gfxpth-1
        ld iy,33
        rst #28
        xor a
        ld (gfxpth-1),a
        ld hl,256*2+MSC_SYS_SYSCFG
        ld (prgmsgb),hl
        ld a,(prgprzn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        ld iy,prgmsgb
        rst #10
        jp diainpc

;### OPTSLD -> Slideshow starten
optsldc db 0
optslds db 0
optsldt db 0
optsldd db 5

optsld  call diainp4            ;Dialog schließen
        rst #20:dw jmp_timget   ;Timer resetten
        ld (optsldt),a
        xor a
        ld (optslds),a
        ld ix,sldinpdur         ;Dauer setzen
        ld bc,1
        ld de,250
        call clcr16
        jr nc,optsld5
        ld l,5
optsld5 ld a,l
        ld (optsldd),a
        ld a,(sldflgful)        ;Fullscreen optional aktivieren
        or a
        jr z,optsld6
        ld a,(gfxfulf)
        or a
        jr nz,optsld6
        call gfxful0
optsld6 ld hl,sldinppth         ;Pfad setzen
        ld de,gfxpth
        ld bc,(prgobjopt3b+8)
        ldir
        ex de,hl
        ld (hl),"\"
        inc hl
        ld (hl),"?"
        inc hl
        xor a
        ld (hl),a
        ld (dirlen),a
        call dirnew
        call dirprv
        jp optslda
optsld1 ld a,(prgprzn)
        db #dd:ld l,a           ;IXL=Rechner-Prozeß-Nummer
        db #dd:ld h,-1
        ld iy,prgmsgb           ;IY=Messagebuffer
        rst #30
        rst #18
        db #dd:dec l
        jr nz,optsld2
        ld a,(iy+0)
        cp MSR_DSK_WCLICK       ;Fenster-Aktion -> Slideshow abbrechen
        jr nz,optsld2
        ld a,(iy+2)
        cp DSK_ACT_KEY
        jr nz,optsld4
        ld a,(iy+4)
        cp 27
        jr nz,optsld2
        jp prgprz0
optsld4 cp DSK_ACT_TOOLBAR
        jr z,optsld9
        cp DSK_ACT_CONTENT
        jr nz,optsld2
optsld9 ld a,(iy+8)
        or (iy+9)
        jp nz,prgprz3
optsld2 ld a,(optsldc)          ;Zählen, bis Dauer abgelaufen
        inc a
        cp 50
        jr c,optsld3
        ld a,0
optsld3 ld (optsldc),a
        jr c,optsld1
        rst #20:dw jmp_timget
        ld hl,optsldt
        cp (hl)
        jr z,optsld1
        ld (hl),a
        ld hl,optslds
        inc (hl)
        ld a,(optsldd)
        cp (hl)
        jr nz,optsld1
        ld (hl),0
optslda ld hl,(dirpos)
        push hl
        ld a,(sldflgrnd)
        or a
        ld b,1                  ;kein Random -> 1 Eintrag weiterspringen
        jr z,optsld7
        ld a,r                  ;falls Random -> 1-16 Einträge weiterspringen
        and 15
        inc a
        ld b,a
optsld7 push bc
        call dirnxt
        pop bc
        djnz optsld7
        pop bc
        push af
        ld hl,(dirpos)
        or a
        sbc hl,bc
        jr nz,optsld8
        pop af
        call dirnxt
        push af
optsld8 pop af
        jp c,optsld1
        call gfxsel2
        jp optsld1

;### OPTGFX -> Angehängte Grafik suchen
optgfx  ld hl,(prgcodbeg)       ;nach angehängter Grafik suchen
        ld de,prgcodbeg
        dec h
        add hl,de               ;HL=CodeEnde=Pfad
        ld b,255
optgfx1 ld a,(hl)
        or a
        jp z,prgprz0
        cp 32
        jr z,optgfx2
        inc hl
        djnz optgfx1
        jp prgprz0
optgfx2 inc hl
        ld de,gfxpth
        ld bc,256
        ldir
        jp gfxsel


;==============================================================================
;### DIRECTORY-ROUTINEN #######################################################
;==============================================================================

dirmax  equ 250
dirsiz  equ 4000

dirpth  ds 256  ;aktuelles Directory
dirlen  db 0    ;Pfadlänge
diranz  dw 0    ;Anzahl Einträge im Directory
dirpos  dw 0    ;Position im Directory
dirmsk  db "sgxscricn",0
dirzgr  ds dirmax*2

;### DIRSCN -> Falls File im Directory Grafikfile ist, wird dies als neues File übernommen
;### Eingabe    HL=Directory-Position
;### Ausgabe    CF=0 -> gefunden und Pfad erzeugt, CF=1 -> nichts gefunden
dirscnl db 0
dirscn  add hl,hl
        ld bc,dirzgr
        add hl,bc
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld l,e
        ld h,d
        ld c,0
dirscn1 ld a,(hl)
        inc c
        inc hl
        cp "."
        jr z,dirscn2
        or a
        jr nz,dirscn1
        scf             ;kein Punkt gefunden -> kein Extension, also kein gültiges File
        ret
dirscn2 ld a,c
        add 4
        ld (dirscnl),a
        push hl
        pop ix
        ld hl,dirmsk-1
dirscn3 inc hl
        ld a,(hl)       ;Test, ob gültige Filemaske
        or a
        scf
        ret z           ;keine gültige Filemaske gefunden -> Ende
        ld c,a
        inc hl
        ld b,(hl)
        inc hl
        ld a,(ix+2)
        call clclcs
        cp (hl)
        jr nz,dirscn3
        ld a,(ix+0)
        call clclcs
        cp c
        jr nz,dirscn3
        ld a,(ix+1)
        call clclcs
        cp b
        jr nz,dirscn3
        push de
        ld hl,dirpth    ;neuen Pfad zusammensetzen
        ld de,gfxpth
        ld a,(dirlen)
        ld c,a
        ld b,0
        ldir
        ld a,"\"
        ld (de),a
        inc de
        pop hl
        ld a,(dirscnl)
        ld c,a
        ld b,0
        ldir
        or a
        ret

;### DIRPRV -> Vorherige Grafik aus Directory holen
;### Ausgabe    CF=0 -> ok, (gfxpth)=neue Grafik, CF=1 -> kein Eintrag vorhanden
dirprv  ld hl,(diranz)
        ld a,l
        or h
        scf
        ret z
        ld c,l
        ld b,h              ;BC=Anzahl Einträge
        ld hl,(dirpos)      ;HL=Pos
dirprv1 ld a,l
        or h
        jr nz,dirprv2
        ld hl,(diranz)
dirprv2 dec hl              ;HL=neue Pos
        push bc
        push hl
        call dirscn         ;an Pos HL Grafikdatei suchen
        pop hl
        pop bc
        jr nc,dirprv3
        dec bc
        ld a,c
        or b
        jr nz,dirprv1
        scf
        ret
dirprv3 ld (dirpos),hl
        ret

;### DIRNXT -> Nächste Grafik aus Directory holen
;### Ausgabe    CF=0 -> ok, (gfxpth)=neue Grafik, CF=1 -> kein Eintrag vorhanden
dirnxt  ld hl,(diranz)
        ld a,l
        or h
        scf
        ret z
        ld c,l
        ld b,h              ;BC=Anzahl Einträge
        ld hl,(dirpos)      ;HL=Pos
dirnxt1 inc hl
        ld de,(diranz)
        or a
        sbc hl,de
        jr nz,dirnxt2
        sbc hl,de
dirnxt2 add hl,de           ;HL=neue Pos
        push bc
        push hl
        call dirscn         ;an Pos HL Grafikdatei suchen
        pop hl
        pop bc
        jr nc,dirprv3
        dec bc
        ld a,c
        or b
        jr nz,dirnxt1
        scf
        ret

;### DIRNEW -> Directory neu einlesen, falls Grafik aus neuem Pfad kommt
dirnewp dw 0
dirnew  ld hl,gfxpth            ;*** Länge des neuen Pfades feststellen
        ld bc,255*256+0
dirnew1 ld a,(hl)
        or a
        jr z,dirnew4
        cp "/"
        jr z,dirnew2
        cp "\"
        jr nz,dirnew3
dirnew2 ld a,255
        sub b
        ld c,a                  ;C=Länge des Pfades bis vor dem (Back)Slash
dirnew3 inc hl
        djnz dirnew1
dirnew4 ld a,c
        or a
        jr nz,dirnew5
        ld l,a                  ;kein Pfad gefunden -> kein Directory einlesen
        ld h,a
        ld (diranz),hl
        ret
dirnew5 ld hl,gfxpth            ;*** neuen mit altem Pfad vergleichen und neuen übernehmen
        ld de,dirpth
        db #dd:ld l,c
        ld b,0
        ld (dirnewp),bc
        ld a,(dirlen)
        cp c
        jr z,dirnew6
        inc b
dirnew6 ld a,c
        ld (dirlen),a
dirnew7 ld a,(de)
        call clclcs
        ld c,a
        ld a,(hl)
        call clclcs
        cp c
        jr z,dirnew8
        set 0,b
dirnew8 ld (de),a
        inc hl
        inc de
        db #dd:dec l
        jr nz,dirnew7
        xor a
        ld (de),a
        dec b
        ret nz                  ;neuer Pfad=alter Pfad -> Directory nicht neu einlesen
        push de
        ld a,"\"
        ld (de),a
        inc de
        xor a
        ld (de),a
        ld hl,dirpth            ;*** Directory laden
        ld a,(prgbnknum)
        db #dd:ld h,a
        ld de,dirbuf
        ld bc,dirsiz
        ld iy,0
        ld (diranz),iy
        ld (dirpos),iy
        db #dd:ld l,16
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRINP
        pop de
        ld c,a
        ld a,0
        ld (de),a
        ld a,c
        ret c                   ;Fehler -> kein Directory vorhanden
        ld de,dirmax
        sbc hl,de
        jr c,dirnew9
        ld hl,0
dirnew9 add hl,de
        ld (diranz),hl          ;maximal XXX Einträge
        ld e,l                  ;*** Zeigertabelle generieren
        ld hl,dirbuf
        ld ix,dirzgr
dirnewa ld bc,9
        add hl,bc
        ld (ix+0),l
        ld (ix+1),h
        inc ix:inc ix
        xor a
        ld bc,-1
        cpir
        dec e
        jr nz,dirnewa
        ld ix,dirzgr            ;*** Liste sortieren
        ld bc,(diranz)
        ld a,2
        ld e,0
        ld hl,256*0+0
        call srtdat
        ld bc,(diranz)          ;*** aktuelle Grafik suchen
        ld hl,dirzgr
        ld de,0
dirnewb push de
        push bc
        push hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ex de,hl                ;DE=File in Directory
        ld hl,(dirnewp)
        ld bc,gfxpth+1
        add hl,bc               ;HL=geladene Grafik
dirnewc ld a,(de)
        call clclcs
        ld c,a
        ld a,(hl)
        call clclcs
        cp c
        jr nz,dirnewd
        inc hl
        inc de
        or a
        jr nz,dirnewc
        pop hl
        pop hl
        pop hl
        ld (dirpos),hl
        ret
dirnewd pop hl
        pop bc
        pop de
        inc hl:inc hl
        inc de
        dec bc
        ld a,c
        or b
        jr nz,dirnewb
        ret

;### Directory-Buffer
dirbuf  db 0


;==============================================================================
;### DATEN-TEIL ###############################################################
;==============================================================================

prgdatbeg

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #00,#00,#00,#00,#0E,#0E,#EE,#EE,#EE,#EE,#EE,#EE,#0E,#EE,#EE,#33,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#E0,#E0,#EE,#EE,#33,#33,#3E,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#E3,#33,#3E,#EE,#EE,#EE
db #33,#E3,#3E,#3E,#EE,#EE,#EE,#EE,#33,#33,#EE,#EE,#33,#33,#33,#33,#3E,#EE,#EE,#EE,#EE,#EE,#3E,#EE,#33,#33,#31,#13,#33,#33,#EE,#EE,#EE,#EE,#33,#E3,#11,#11,#1E,#11,#11,#11,#11,#1E,#EE,#EE,#0E,#33
db #08,#88,#88,#33,#66,#66,#11,#11,#11,#EE,#E0,#E3,#88,#88,#8E,#36,#66,#63,#33,#33,#31,#11,#E0,#00,#88,#88,#8E,#36,#66,#33,#33,#33,#36,#61,#11,#E0,#08,#88,#8E,#36,#66,#33,#11,#33,#36,#66,#31,#1E
db #18,#88,#8E,#36,#66,#11,#11,#33,#66,#66,#3E,#11,#1E,#88,#80,#36,#66,#61,#13,#33,#66,#63,#38,#81,#11,#E8,#88,#33,#66,#66,#16,#66,#66,#63,#E8,#88,#11,#1E,#88,#03,#E6,#66,#66,#66,#66,#63,#08,#88
db #3E,#31,#10,#8E,#3E,#66,#66,#66,#66,#3E,#88,#88,#EE,#E3,#11,#10,#E1,#16,#66,#6E,#33,#E8,#88,#88,#EE,#EE,#33,#11,#11,#11,#33,#33,#30,#88,#88,#88,#EE,#EE,#EE,#E3,#33,#31,#11,#E0,#88,#88,#88,#88
db #3E,#EE,#EE,#EE,#33,#33,#31,#11,#11,#11,#E1,#E1,#EE,#EE,#EE,#EE,#EE,#33,#33,#33,#33,#31,#11,#11,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#E3,#33,#33,#33,#33,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#EE,#33

;### Toolbar-Buttons
tolfultxt   db "Full",0
tolopttxt   db "Options...",0

;1W source, 1W destination+9, 1B byte width, 1B pixel width, 1B height, 1B number of sourcebytes <=127)
gfxcnvtab
dw tolprvspr0,tolprvspr+9:db 8,16,12,12*4
dw tolnxtspr0,tolnxtspr+9:db 8,16,12,12*4
dw tolselspr0,tolselspr+9:db 8,16,12,12*4
dw 0

tolprvspr db 4,16,12:dw tolprvspr+10,tolprvspr+9,4*12:db 0
ds 4*12
tolprvspr0
db #FF,#FF,#FF,#CF
db #8F,#0F,#0F,#4B
db #8F,#2D,#0F,#4B
db #8F,#69,#0F,#4B
db #8F,#F0,#E1,#4B
db #9E,#F0,#E1,#4B
db #9E,#F0,#E1,#4B
db #8F,#F0,#E1,#4B
db #8F,#69,#0F,#4B
db #8F,#2D,#0F,#4B
db #8F,#0F,#0F,#4B
db #F0,#F0,#F0,#C3

tolnxtspr db 4,16,12:dw tolnxtspr+10,tolnxtspr+9,4*12:db 0
ds 4*12
tolnxtspr0
db #FF,#FF,#FF,#CF
db #8F,#0F,#0F,#4B
db #8F,#1E,#0F,#4B
db #8F,#1E,#87,#4B
db #9E,#F0,#C3,#4B
db #9E,#F0,#E1,#4B
db #9E,#F0,#E1,#4B
db #9E,#F0,#C3,#4B
db #8F,#1E,#87,#4B
db #8F,#1E,#0F,#4B
db #8F,#0F,#0F,#4B
db #F0,#F0,#F0,#C3

tolselspr db 4,16,12:dw tolselspr+10,tolselspr+9,4*12:db 0
ds 4*12
tolselspr0
db #FF,#FF,#FF,#CF
db #8F,#0F,#0F,#4B
db #8F,#3C,#0F,#4B
db #8F,#78,#87,#4B
db #8F,#F0,#C3,#4B
db #9E,#F0,#E1,#4B
db #9E,#F0,#E1,#4B
db #8F,#0F,#0F,#4B
db #9E,#F0,#E1,#4B
db #9E,#F0,#E1,#4B
db #8F,#0F,#0F,#4B
db #F0,#F0,#F0,#C3

;### Verschiedenes
prgmsginf1  db "SymSee",0
prgmsginf2  db " Version 1.9 (Build "
read "..\..\..\SVN-Main\trunk\build.asm"
            db "pdt)",0
prgmsginf3  db " Copyright <c> 2024 SymbiosiS",0

prgmsgerr1  db "Can't display selected graphic",0
prgmsgerr2a db "Error while loading",0
prgmsgerr2b db "Not enough memory",0
prgmsgerr0  db 0

prgwintit   db "SymSee 1.9",0
prgwinsta0  db "No image loaded",0
prgwinsta1  db "Image: "
prgwinsta2  ds 256+32

;### Option-Fenster
prgtitopt   db "Options",0
prgbutabo   db "About",0
prgbuthlp   db "Help",0
prgbutoky   db "Close",0
prgbutsld   db "Start Slideshow",0
prgbutbck   db "Use as background",0

prgobjtxt1  db "Image",0
prgobjtxt1a db "Resolution: "
prgobjtxt1b ds 22
prgobjtxt1c db " pixel",0
prgobjtxt2  db "Slideshow",0
prgobjtxt3a db "Image path",0
prgobjtxt4a db "Display duration",0
prgobjtxt4c db "sec.",0
prgobjtxt6  db "Run in fullscreen",0
prgobjtxt7  db "Random order",0


;==============================================================================
;### TRANSFER-TEIL ############################################################
;==============================================================================

prgtrnbeg
;### PRGPRZS -> Stack für Programm-Prozess
        ds 128
prgstk  ds 6*2
        dw prgprz
prgprzn db 0
prgmsgb ds 14

;### INFO-FENSTER #############################################################

prgmsginf  dw prgmsginf1,4*1+2,prgmsginf2,4*1+2,prgmsginf3,4*1+2,prgicnbig

;### ERROR-FENSTER ############################################################

prgmsgerr  dw prgmsgerr1,4*1+2
prgmsgerra dw prgmsgerr0,4*1+2,prgmsgerr0,4*1+2

;### HAUPT-FENSTER ############################################################

prgwindat dw #5f02,0,07,05,296,132,0,0,109,24,109,24,10000,10000,prgicnsml,prgwintit,prgwinsta0,0,prgwingrp,prgwingrt,15:ds 136+14

prgwingrt db 6,0:dw prgwintol,0,0,00,0,0,00
prgwintol
dw     00,255*256+0, 2,         0,0,10000,10000,0   ;00=Hintergrund
dw gfxprv,255*256+10,tolprvspr ,  1,  1, 16,12,0    ;01=Button "Previous"
dw gfxnxt,255*256+10,tolnxtspr , 17,  1, 16,12,0    ;02=Button "Next"
dw prgsrc,255*256+10,tolselspr , 33,  1, 16,12,0    ;04=Button "Open"
dw gfxful,255*256+16,tolfultxt , 49,  1, 25,12,0    ;03=Button "Fullscreen"
dw optopn,255*256+16,tolopttxt , 76,  1, 40,12,0    ;05=Button "Options"

prgwingrp db 1,0:dw prgwinobj,0,0,256*00+00,0,0,00
prgwinobj
dw 0,255*256+0,0, 0,0,10000,10000,0     ;00 Hintergrund
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;01 Grafik Teil 01
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;02 Grafik Teil 02
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;03 Grafik Teil 03
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;04 Grafik Teil 04
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;05 Grafik Teil 05
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;06 Grafik Teil 06
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;07 Grafik Teil 07
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;08 Grafik Teil 08
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;09 Grafik Teil 09
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;10 Grafik Teil 10
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;11 Grafik Teil 11
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;12 Grafik Teil 12
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;13 Grafik Teil 13
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;14 Grafik Teil 14
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;15 Grafik Teil 15
dw gfxnxt,255*256+8,0, 0,0,0000,0000,0  ;16 Grafik Teil 16

;### OPTIONS ##################################################################

prgwinopt   dw #1501,4+16,96,15,128,147,0,0,128,147,128,147,128,147, prgicnsml,prgtitopt,0,0,prggrpopt,0,0:ds 136+14
prggrpopt   db 17,0:dw prgdatopt,0,0,256*4+17,0,0,0
prgdatopt
dw 00,     255*256+0,2, 0,0,10000,10000,0               ;00=Hintergrund
dw prginf, 255*256+16,prgbutabo,    3,132, 39,12,0      ;01=Button "About"
dw hlpopn, 255*256+16,prgbuthlp,   45,132, 38,12,0      ;02=Button "Help"
dw diainpc,255*256+16,prgbutoky,   86,132, 39,12,0      ;03=Button "Close"
dw 00,     255*256+3, prgobjopt1,   0,  1,128,52,0      ;04=Rahmen Image
dw 00,     255*256+1, prgobjopt1a,  8, 13,112, 8,0      ;05=Beschreibung Pfad
dw 00,     255*256+1, prgobjopt1b,  8, 23,112, 8,0      ;06=Beschreibung Auflösung
prgdatopt1
dw optbgr, 255*256+16,prgbutbck,   14, 34,100,12,0      ;07=Button "Use as Background"
dw 00,     255*256+3, prgobjopt2,   0, 53,128,78,0      ;08=Rahmen Slideshow
dw 00,     255*256+1, prgobjopt3a,  8, 65, 50, 8,0      ;09=Beschreibung Pfad
dw 00,     255*256+32,prgobjopt3b, 55, 63, 65,12,0      ;10=Input Pfad
dw 00,     255*256+1, prgobjopt4a,  8, 79, 65, 8,0      ;11=Beschreibung Verzögerung1
dw 00,     255*256+32,prgobjopt4b, 77, 77, 20,12,0      ;12=Input Verzögerung
dw 00,     255*256+1, prgobjopt4c, 99, 79, 23, 8,0      ;13=Beschreibung Verzögerung2
dw 00,     255*256+17,prgobjopt6,   8, 91,112, 8,0      ;14=Check Fullscreen
dw 00,     255*256+17,prgobjopt7,   8,101,112, 8,0      ;15=Check Random Order
dw optsld, 255*256+16,prgbutsld,   14,112,100,12,0      ;16=Button "Start Slideshow"

prgobjopt1  dw prgobjtxt1,2+4
prgobjopt1a dw gfxpth,2+4+128
prgobjopt1b dw prgobjtxt1a,2+4+128
prgobjopt2  dw prgobjtxt2,2+4
prgobjopt3a dw prgobjtxt3a,2+4
prgobjopt3b dw sldinppth,0,2,0,2,255,0
prgobjopt4a dw prgobjtxt4a,2+4
prgobjopt4b dw sldinpdur,0,1,0,1,3,0
prgobjopt4c dw prgobjtxt4c,2+4
prgobjopt6  dw sldflgful,prgobjtxt6,2+4
prgobjopt7  dw sldflgrnd,prgobjtxt7,2+4

sldinppth   db "a:":ds 256-2
sldinpdur   db "5":ds 4-1
sldflgful   db 1
sldflgrnd   db 0

gfxmsk  db "*  ",0
gfxpth  ds 256

cfgcpctyp   db 0

prgtrnend

relocate_table
relocate_end
