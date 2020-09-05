\version "2.7.40"
\include "scale-common.ly"
\score {
 \new Staff\relative c'{ \cadenzaOn
    <c b'>1 <d c'> <e d'> <f e'> <g f'> <a g'> <b a'>}
 \addlyrics{ major minor minor major minor minor minor }
}
