\version "2.7.40"
\include "scale-common.ly"
\score {
 \new Staff\relative c'{ \cadenzaOn
    <c e>1 <d f> <e g> <f a> <g b> <a c> <b d>}
 \addlyrics{ major minor minor major major minor minor }
}
