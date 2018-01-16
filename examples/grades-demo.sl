; see 
; https://docs.google.com/spreadsheets/d/1BDWk0LncTibd3lPtuRgEaT69j4L-sMiiKzRwvxNLh5Y/edit#gid=0

(def (grade hw test)
  (/ (+ hw test)
     (+ 50.0 400.0)))

(grade 43 360)
(grade 50 400)
(grade  0 400)
(grade 50 200)
