\version "2.7.40"
\include "scale-common.ly"
\score {
 \new Staff\relative c'{ \cadenzaOn
    <c d>1 <d e> <e f> <f g> <g a> <a b> <b c>}
 \addlyrics{ major major minor major major major minor }
}
