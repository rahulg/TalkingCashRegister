ASM86 MISC.ASM
ASM86 TIMER.ASM
LINK86 TIMER.OBJ, MISC.OBJ TO TIMER.LNK
LOC86 TIMER.LNK TO TIMER.ABS ST(START) AD(SEGMENTS(CODE_SEG(100H)))
OH86 TIMER.ABS TO TIMER.HEX
DEL TIMER.TXT
RENAME TIMER.HEX TIMER.TXT
PAUSE