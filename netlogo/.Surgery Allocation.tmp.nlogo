extensions [csv]
globals
[
  ordered-surgeries
  total-waiting-time
  total-surgeries
  max-waiting-time
  total-prep-time
  max-prep-time
]

;; agents
breed [hospitals hospital]
breed [surgeons surgeon]
breed [surgeries surgery]

surgeries-own [
  surgery-id
  duration ;; list with each element being [hospital-id]
  urgency
  surgery-type
  surgery-specialty
  assigned-op-room
  assigned-surgeon
  assigned-day
  start-time
  prep-time
  hosp-id
]

hospitals-own [
  hospital-id
  hospital-type
]

patches-own [
  or-schedule
  or-type
  or-hosp-id
]

surgeons-own [
  surgeon-id
  surgeon-specialty
  surgeon-hosp-id
  surgeon-expertise
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INTERFACE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  reset-ticks
  set total-waiting-time 0
  set total-surgeries 0
  set max-waiting-time 0
  set total-prep-time 0
  set max-prep-time 0
  create-surgeries-data
  show count surgeries
  create-hospitals-data
  create-surgeons-data
  order-surgeries
end

to go
  show (get-day-free-time (list 1 2) 10 30 1)

  ;ask (item 0 ordered-surgeries)
  ;[
  ;  allocate-operating-block
  ;]
  ;foreach ordered-surgeries
  ;[
  ;  first-surgery -> ask first-surgery [allocate-operating-block]
  ;  tick
  ;]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GLOBAL FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; order surgeries by urgency TESTED
to order-surgeries
  set ordered-surgeries sort-on [(- urgency)] surgeries
end

;; obtain best surgeon that would do the surgery in each hospital. heuristic -> most free time TESTED
to-report get-surgeon-per-hospital [specialty]
  let hospitals-surgeon []
  ask hospitals
  [
    set hospitals-surgeon (insert-item (length hospitals-surgeon) hospitals-surgeon (get-surgeon hospital-id specialty))
  ]
  report hospitals-surgeon
end

;; calculate surgery's duration taking into account its type and the level of expertise of the surgeon, received as argument
to-report calculate-duration [s-type expertise]
  let s-duration 0
  ifelse s-type = "big"
  [ set s-duration 120 ]
  [ ifelse s-type = "medium"
    [ set s-duration 90 ]
    [ set s-duration 45 ]
  ]

  ifelse expertise = "new"
  [set s-duration (s-duration + 30)]
  [
    if expertise = "expert"
    [set s-duration (s-duration - 20)]
  ]
  report s-duration
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SURGERY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to allocate-operating-block
  ;; get surgeon who would perform the surgery in each hospital
  let surgeon-hospital (get-surgeon-per-hospital surgery-specialty) ;[hospital-id surgeon-id surgeon-expertise]
  show surgeon-hospital

  ;; obtain duration of the surgery with each surgeon
  foreach surgeon-hospital
  [
    a-surgeon -> set duration (insert-item (length duration) duration (list (item 0 a-surgeon)  (calculate-duration surgery-type (item 2 a-surgeon))))
  ]

  ;; get operating rooms schedule

  let ors-list patches with [or-hosp-id != 0] ;; obtain all operating rooms
  let schedule-results []
  let s-hosp-id hosp-id
  if heuristic = "minimize-prep-time" or heuristic = "minimize-waiting-time"
  [
    set ors-list patches with [or-hosp-id = s-hosp-id] ;; obtain operating rooms of surgery's hospital
  ]
  ;; obtain each operating room's best schedule
  ask ors-list
  [
    ;; set schedule insert-item (length schedule) schedule pycor
  ]
  ;; compare schedules
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OPERATING ROOM FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; operating rooms procedure to insert surgery in its schedule
to insert-surgery [s-day s-start-time s-duration s-prep-time s-surgeon s-surgery]
  ifelse (length or-schedule) <= s-day
  [
    set or-schedule (insert-item (length or-schedule) or-schedule [])
    let or-schedule-day (item s-day or-schedule)
    set or-schedule-day (insert-item (length or-schedule-day) or-schedule-day s-surgery)
    set or-schedule (replace-item s-day or-schedule or-schedule-day)
  ]
  [
    let or-schedule-day (item s-day or-schedule)
    set or-schedule-day (insert-item (length or-schedule-day) or-schedule-day s-surgery)
    set or-schedule (replace-item s-day or-schedule or-schedule-day)
  ]
end

;; operating room procedure to calculate surgery prep time
to-report calculate-prep-time [s-type schedule-day]
  let s-prep-time 0
  ifelse s-type = "big"
  [set s-prep-time 30] ;; 30 minutes for equipment
  [
    ifelse s-type = "medium"
    [set s-prep-time 20] ;; 20 minutes for equipment
    [set s-prep-time 10] ;; 10 minutes for equipment
  ]
  ;; TODO - check if there was a surgery before with the same type
  set s-prep-time (s-prep-time + 10) ;; 10 minutes for other activities
  report s-prep-time
end

to-report calculate-transference-cost [s-hosp-id or-hospital-id]
  ifelse s-hosp-id = or-hospital-id
  [ report 0 ]
  [
    ;; TODO
    report 0
  ]
end

;; operating rooms procedure to calculate and return the best schedule for surgery. return [pxcor pycor day start-time prep-time transf-cost] TODO
to-report calculate-schedule [specialty s-duration s-type s-hosp-id]
;; TODO
  ifelse empty? or-schedule
  [
    let s-prep-time (calculate-prep-time s-type [])
    let s-trans-cost (calculate-transference-cost s-hosp-id or-hosp-id)
    report (list pxcor pycor 0 0 s-prep-time s-trans-cost)
  ]
  [
    let index 0
    let stop-day-analysis? false
    let potential-schedules [] ;; [day start-time prep-time]
    while [index <= (length or-schedule) and not stop-day-analysis?] ;;um dia mais além!!
    [
      ifelse index = (length or-schedule)
      [
        let s-prep-time (calculate-prep-time s-type [])
        set potential-schedules (insert-item (length potential-schedules) potential-schedules (list (length potential-schedules) 0 s-prep-time))
      ]
      [
        let day (item index or-schedule)
        ;; TODO
      ]
      set index (index + 1)
    ]
  ]
end

;; obtain the free time blocks of a operating room in a day TODO TESTED
to-report get-day-free-time [schedule-day s-duration s-prep-time or-hospital-id]
  let free-time (list (list 0 (operating-hours * 60)))
  let index 0
  let s-start-time 0 ;; DUMMY
  let s-h-duration 20 ;; DUMMY
  while [index < (length schedule-day)]
  [
    show (word "free time " free-time)
    ask surgeries with [surgery-id = (item index schedule-day)]
    [
      let s-start-time start-time
      let s-h-duration (get-duration-hospital s-duration or-hospital-id)


    let end-surgery (s-start-time + s-prep-time + s-h-duration)
    while [index-free-time < (length free-time)]
    [
      if end-surgery > (item 0 (item index-free-time free-time)) and s-start-time < (item 1 (item index-free-time free-time))
      [
        let substitute []
        let index-to-add (index-free-time + 1)
        if s-start-time > (item 0 (item index-free-time free-time))
        [
          set free-time (insert-item index-to-add free-time (list (item 0 (item index-free-time free-time)) s-start-time))
          set index-to-add (index-to-add + 1)
        ]

        if end-surgery < (item 1 (item index-free-time free-time))
        [ set free-time (insert-item index-to-add free-time (list end-surgery (item 1 (item index-free-time free-time)))) ]

        set free-time (remove-item index-free-time free-time)
      ]
      set index-free-time (index-free-time + 1)
    ]
    ;]
    set s-start-time (end-surgery + 5);; DUMMY
    set index (index + 1)
  ]
  report free-time
end

;; parse surgery duration array to obtain duration for one hospital
to-report get-duration-hospital [s-duration or-hospital-id]
  let index 0
  while [index < (length s-duration)]
  [
    if (item 0 (item index s-duration)) = or-hospital-id
    [
      report (item 1 (item index s-duration))
    ]
    set index (index + 1)
  ]
  report -1
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HOSPITAL FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function to get surgeon of the specialty with the most free time. returns [hospital-id surgeon-id surgeon-expertise]
to-report get-surgeon [s-hosp-id specialty]
  let all-surgeons []
  ask surgeons with [surgeon-hosp-id = s-hosp-id and surgeon-specialty = specialty]
  [
    set all-surgeons insert-item (length all-surgeons) all-surgeons get-occupied-time
  ]
  let min-value (item 2 (item 0 all-surgeons))
  let best-surgeon (item 0 (item 0 all-surgeons))
  let expertise (item 3 (item 0 all-surgeons))
  foreach all-surgeons
  [
    selected-surgeon -> if min-value > (item 2 selected-surgeon)
    [
      set min-value (item 2 selected-surgeon)
      set best-surgeon (item 0 selected-surgeon)
      set expertise (item 3 selected-surgeon)
    ]
  ]
  report (list s-hosp-id best-surgeon expertise)
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SURGEON FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; obtain a surgeons occupied time. returns [surgeon-id hospital-id occupied-time expertise]
to-report get-occupied-time
  ;; TODO
  report (list surgeon-id surgeon-hosp-id (random 100) surgeon-expertise) ;; [surgeon-id hospital-id occupied-time expertise]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOAD DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; id,urgency,surgery-type,surgery-specialty
to create-surgeries-data
  file-open (word data-folder "/surgeries.csv")
  while [ not file-at-end? ] [
    let data csv:from-row file-read-line
    create-surgeries 1 [
      set size 1
      setxy -15 15
      set duration []
      set surgery-id item 0 data
      set urgency item 1 data
      set surgery-type item 2 data
      set surgery-specialty item 3 data
      set hosp-id item 4 data
      ifelse urgency = 1
      [set color green]
      [
        ifelse urgency = 2
        [set color yellow]
        [set color red]
      ]
    ]
 ]
  file-close
end

;; id,type,small or number,medium or number,big or number
to create-hospitals-data
  file-open (word data-folder "/hospitals.csv")
  let or-big 0
  let or-medium 0
  let or-small 0
  let last-or 0
  random-seed 11
  while [ not file-at-end? ] [
    let data csv:from-row file-read-line
    let hospital-color (random 140)
    create-hospitals 1 [
      set size 2
      set color black
      setxy 20 20
      set hospital-id item 0 data
      set hospital-type item 1 data
      set or-big item 2 data
      set or-medium item 3 data
      set or-small item 4 data
    ]
    let i 0
    while [i < or-medium + or-small + or-big]
    [
      let y (40 - ((last-or * 4 + 2) mod 40) - 20)
      let x (38 - (floor ((last-or * 4 + 2) / 40)) * 4 - 20)
      ask patches with [pxcor = x and pycor = y] [
        set pcolor hospital-color
        set or-hosp-id item 0 data
        set or-schedule []
        ifelse i < or-small
        [ set or-type "small" ]
        [ ifelse i < or-small + or-medium
          [set or-type "medium"]
          [set or-type "big"]
        ]
      ]
      set i (i + 1)
      set last-or (last-or + 1)
    ]
  ]
  file-close
end

;; id,specialty,hospital
to create-surgeons-data
  file-open (word data-folder "/surgeons.csv")
  while [ not file-at-end? ] [
    let data csv:from-row file-read-line
    create-surgeons 1 [
      set size 1
      set color 5
      setxy -15 -15
      set surgeon-id item 0 data
      set surgeon-specialty item 1 data
      set surgeon-hosp-id item 2 data
      ;; TODO: add expertise to csvs
      let expertise (random 3)
      ifelse expertise = 1
      [
        set surgeon-expertise "new"
        set color 79
      ]
      [
        ifelse expertise = 2
        [
          set surgeon-expertise "veteran"
          set color 77
        ]
        [
          set surgeon-expertise "expert"
          set color 75
        ]
      ]
    ]
  ]
  file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
379
10
791
423
-1
-1
9.854
1
10
1
1
1
0
1
1
1
-20
20
-20
20
0
0
1
ticks
30.0

BUTTON
280
15
343
48
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
10
14
189
59
heuristic
heuristic
"minimize-prep-time" "minimize-waiting-time" "across-hospitals"
2

BUTTON
209
15
272
48
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
10
77
188
137
data-folder
data/simple_example
1
0
String

SLIDER
10
153
189
186
operating-hours
operating-hours
4
24
12.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
