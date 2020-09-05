\version "2.8.1"
\include "scale-common.ly"
\score {
 \new Staf\relative c'{ \cadenzaOn <c c>1 <c d> <c e> <c f> <c g'> <c a'> <c b'> <c c'> <c d'> <c e'> }
 \addlyrics{ unison second third fourth fifth sixth seventh octave ninth decim }
 \addlyrics{ "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" }
}
