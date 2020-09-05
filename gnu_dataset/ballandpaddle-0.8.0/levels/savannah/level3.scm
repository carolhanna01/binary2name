(set-background "listen.png")
(set-skin "skin.png")
(set-bonus-time 80000)
(load "../utils.scm")

(define (block-vert-stripes x y step n height type)
  (if (> n 0)
    (begin
      (block-rect x y 1 height type)
      (block-vert-stripes (+ x step) y step (- n 1) height type))))

(block-vert-stripes 0 0 58 9 10 "light-brown")

