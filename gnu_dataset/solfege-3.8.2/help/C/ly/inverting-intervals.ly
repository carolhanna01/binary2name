\version "2.7.40"
\include "scale-common.ly"
\score {
<<
 \new Staff\relative c'{ <d fis>1  <fis d'> 
 <c bes'> <bes' c> }
 \new Lyrics\lyricmode {
    \override LyricText #'self-alignment-X = #-0.9
    "major second" "minor sixths" "minor seventh" "major second" }
>>
}
