(define (block-row-aux x y width type)
  (if (> width 0)
    (begin
      (level-add-block x y type)
      (block-row-aux (+ x (get-block-type-width type)) y 
                     (- width 1) type))))

(define (block-row x y width type)
  (block-row-aux x y width type))

(define (block-rect-aux x y width height type)
  (if (> height 0)
    (begin
      (block-row x y width type)
      (block-rect-aux x (+ y (get-block-type-height type)) 
                        width (- height 1) type))))

(define (block-rect x y width height type)
  (block-rect-aux x y width height type))

