\version "2.7.40"
\include "scale-common.ly"
\score {
 \new Staff\relative c'{ \cadenzaOn
    <c a'>1 <d b'> <e c'> <f d'> <g e'> <a f'> <b g'>}
 \addlyrics{ major major minor major major minor minor }
}
