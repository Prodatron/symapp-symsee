;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                                S y m S e e                                 @
;@                   (default application texts [english])                    @
;@                                                                            @
;@             (c) 2004-2025 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;### POINTER ##################################################################

tolfultxt       db 1:dw tolfultxt_eng
tolopttxt       db 1:dw tolopttxt_eng

prgmsginf1      db 1:dw prgmsginf1_eng

prgmsgerr1      db 1:dw prgmsgerr1_eng
prgmsgerr2a     db 1:dw prgmsgerr2a_eng
prgmsgerr2b     db 1:dw prgmsgerr2b_eng

prgwintit       db 1:dw prgwintit_eng
prgwinsta0      db 1:dw prgwinsta0_eng
prgwinsta1      db 1:dw prgwinsta1_eng
prgwinsta3      db 1:dw prgwinsta3_eng

prgtitopt       db 1:dw prgtitopt_eng
prgbutabo       db 1:dw prgbutabo_eng
prgbuthlp       db 1:dw prgbuthlp_eng
prgbutoky       db 1:dw prgbutoky_eng
prgbutsld       db 1:dw prgbutsld_eng
prgbutbck       db 1:dw prgbutbck_eng

prgobjtxt1      db 1:dw prgobjtxt1_eng
prgobjtxt1a     db 1:dw prgobjtxt1a_eng
prgobjtxt2      db 1:dw prgobjtxt2_eng
prgobjtxt3a     db 1:dw prgobjtxt3a_eng
prgobjtxt4a     db 1:dw prgobjtxt4a_eng
prgobjtxt4c     db 1:dw prgobjtxt4c_eng
prgobjtxt6      db 1:dw prgobjtxt6_eng
prgobjtxt7      db 1:dw prgobjtxt7_eng


;### TEXTS ####################################################################

tolfultxt_eng   db "Full",0
tolopttxt_eng   db "Options...",0

prgmsginf1_eng  db "SymSee",0

prgmsgerr1_eng  db "Can't display selected graphic",0
prgmsgerr2a_eng db "Error while loading",0
prgmsgerr2b_eng db "Not enough memory",0

prgwintit_eng   db "SymSee 1.10",0
prgwinsta0_eng  db "No image loaded",0
prgwinsta1_eng  db "Image: ",0
prgwinsta3_eng  db " colours)",0

prgtitopt_eng   db "Options",0
prgbutabo_eng   db "About",0
prgbuthlp_eng   db "Help",0
prgbutoky_eng   db "Close",0
prgbutsld_eng   db "Start Slideshow",0
prgbutbck_eng   db "Use as background",0

prgobjtxt1_eng  db "Image",0
prgobjtxt1a_eng db "Resolution: ",0
prgobjtxt2_eng  db "Slideshow",0
prgobjtxt3a_eng db "Image path",0
prgobjtxt4a_eng db "Display duration",0
prgobjtxt4c_eng db "sec.",0
prgobjtxt6_eng  db "Run in fullscreen",0
prgobjtxt7_eng  db "Random order",0


;### RESERVE
ds 20
