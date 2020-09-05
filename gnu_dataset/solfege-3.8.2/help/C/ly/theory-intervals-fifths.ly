\version "2.7.40"
\include "scale-common.ly"
\score {
 \new Staff\relative c'{ \cadenzaOn
    <c g'>1 <d a'> <e b'> <f c'> <g d'> <a e'> <b f'>}
 \addlyrics{ pure pure pure pure pure pure diminished }
}
