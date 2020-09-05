\version "2.7.40"
\include "scale-common.ly"
\score {
 \new Staff\relative c'{ \cadenzaOn
    <c f>1 <d g> <e a> <f b> <g c> <a d> <b e>}
 \addlyrics{ pure pure pure augmented pure pure pure }
}
