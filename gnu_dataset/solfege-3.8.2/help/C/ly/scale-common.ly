\header { tagline = "" }
\layout { 
    ragged-right = ##t
    \context {
      \Staff
      \remove "Time_signature_engraver"
    }
    \context {
       \Lyrics
       \override LyricSpace #'minimum-distance = #1.0
    }
}

