extensions [csv]
globals
[
  ordered-surgeries
  total-waiting-time
  total-surgeries
  total-or
  total-hospitals
  total-surgeons
  average-waiting-time
  max-waiting-time
  total-prep-time
  max-prep-time
  average-prep-time
  number-transfer
  average-transfer-cost
  max-transfer-cost
  total-transfer-cost
]

;; agents
breed [hospitals hospital]
breed [surgeons surgeon]
breed [surgeries surgery]

surgeries-own [
  surgery-id                       ;; identifier
  duration                         ;; duration that surgery would take per hospital -> list with each element being hospital-id
  urgency                          ;; 1, 2 or 3
  surgery-type                     ;; big, medium or small
  surgery-specialty
  assigned-or-coords               ;; [x, y] from the assigned operating room
  assigned-surgeon                 ;; assigned surgeon
  surgeon-per-hospital             ;; surgeon that would do the surgery in each hospital -> list with [hospital-id surgeon-id surgeon-expertise]
  assigned-day                     ;; surgery assigned day
  start-time                       ;; surgery assigned start time (before preparation)
  prep-time                        ;; time that surgery takes to prepare
  hosp-id                          ;; initial hospital in which the patient was signed up for surgery
  final-hosp-id                    ;; assigned hospital after allocation
  actual-duration                  ;; int with actual duration of the surgery, in minutes
  surgery-transfer-cost
  state
]

hospitals-own [
  hospital-id
  hospital-type                    ;; public or private
  hospital-x
  hospital-y
  number-ors
]

patches-own [
  or-schedule
  or-hosp-id
]

surgeons-own [
  surgeon-id
  surgeon-specialty
  surgeon-hosp-id
  surgeon-expertise
  surgeon-schedule                 ;; bidimensional list: each row represents a day; each column represents a surgery
  occupied-time
  surgeon-state
  surgeon-init-pos
  move-coords
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INTERFACE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  set-patch-size 15
  resize-world -15 15 -15 15
  clear-all
  clear-plot
  reset-ticks
  set total-waiting-time 0
  set total-surgeries 0
  set max-waiting-time 0
  set total-prep-time 0
  set max-prep-time 0
  create-surgeries-data
  create-hospitals-data
  create-surgeons-data
  order-surgeries
  set total-hospitals count hospitals
  set total-surgeons count surgeons
  set total-or count patches with [pcolor != black]
  set number-transfer 0
  set average-transfer-cost 0
  set max-transfer-cost 0
  set total-transfer-cost 0
end

to go
  foreach ordered-surgeries
  [
    first-surgery -> ask first-surgery [
      allocate-operating-block
    ]
  ]
  save-schedule
end

to show-results
  let time 0
  let all-schedules []
  ask patches with [or-hosp-id != 0]
  [
    set all-schedules (lput or-schedule all-schedules)
  ]

  let day 0
  while [day <= max [assigned-day] of surgeries]
  [
    set time  0
    while [time <= 60 * operating-hours]
    [
      ask surgeries with [assigned-day = day and start-time = time]
      [
        update-global-variables assigned-day prep-time surgery-transfer-cost
        facexy (item 0 assigned-or-coords) (item 1 assigned-or-coords)
        set state "move"
      ]
      ask surgeries with [assigned-day = day and start-time + prep-time = time]
      [
        let selected-surgeon assigned-surgeon
        let m-coords assigned-or-coords
        ask surgeons with [surgeon-id = selected-surgeon]
        [
          set move-coords m-coords
          set surgeon-state "move"
        ]
      ]
      ask surgeries with [assigned-day = day and start-time + prep-time + actual-duration = time]
      [
        set state "hide"
        let selected-surgeon assigned-surgeon
        ask surgeons with [surgeon-id = selected-surgeon]
        [
          set move-coords surgeon-init-pos
          set surgeon-state "move"
        ]
      ]
      move
      set time (time + 1)
    ]
    set day (day + 1)
  ]
end

to move
  ask surgeries with [state = "move"]
  [
    facexy (item 0 assigned-or-coords) (item 1 assigned-or-coords)
    forward 1
    let patch-coords (list 0 0)
    ask patch-here
    [
      set patch-coords (list pxcor pycor)
    ]
    if (item 0 patch-coords) = (item 0 assigned-or-coords) and (item 1 patch-coords) = (item 1 assigned-or-coords)
    [
      set state "arrived"
    ]
  ]
  ask surgeries with [state = "hide"]
  [
    hide-turtle
  ]
  ask surgeons with [surgeon-state = "move"]
  [
    facexy (item 0 move-coords) (item 1 move-coords)
    forward 1
    let patch-coords (list 0 0)
    ask patch-here
    [
      set patch-coords (list pxcor pycor)
    ]
    if ((item 0 patch-coords) = (item 0 move-coords) and (item 1 patch-coords) = (item 1 move-coords)) or ((item 0 move-coords) = (item 0 surgeon-init-pos) and (item 0 patch-coords) > (item 0 move-coords) - 0.5)
    [
      set surgeon-state "operating"
    ]
  ]
  tick
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GLOBAL FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; order surgeries by urgency
to order-surgeries
  set ordered-surgeries sort-on [(- urgency)] surgeries
end

;; obtain best surgeon that would do the surgery in each hospital. heuristic -> most free time
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
  [ set s-duration 180 ]
  [ ifelse s-type = "medium"
    [ set s-duration 120 ]
    [ set s-duration 60 ]
  ]

  ifelse expertise = "new"
  [set s-duration (s-duration + 30)]
  [
    if expertise = "expert"
    [set s-duration (s-duration - 20)]
  ]
  report s-duration
end

;; allocation of surgeons and ors
to allocate-operating-block
  let transfer-cost 0
  ;; check if surgery should be transferred
  ifelse hospital-transfer = "none"
  [ set final-hosp-id hosp-id ]
  [
    let result-hosp (get-hospital-for-surgery hosp-id surgery-specialty)
    set final-hosp-id (item 0 result-hosp)
    set transfer-cost (item 1 result-hosp)
  ]

  ;; get surgeon who would perform the surgery in each hospital
  set surgeon-per-hospital (get-surgeon-per-hospital surgery-specialty) ;[hospital-id surgeon-id surgeon-expertise]

  ;; obtain duration of the surgery with each surgeon
  foreach surgeon-per-hospital
  [
    a-surgeon -> set duration (insert-item (length duration) duration (list (item 0 a-surgeon)  (calculate-duration surgery-type (item 2 a-surgeon))))
  ]

  ;; get operating rooms schedule
  let ors-list patches with [or-hosp-id != 0] ;; obtain all operating rooms
  let available-schedules []
  let s-hosp-id final-hosp-id
  if heuristic = "minimize-prep-time" or heuristic = "minimize-waiting-time"
  [
    set ors-list patches with [or-hosp-id = s-hosp-id] ;; obtain operating rooms of surgery's hospital
  ]
  ;; obtain each operating room's best schedule
  let ors []
  ask ors-list
  [
    set ors (lput (list or-schedule or-hosp-id pxcor pycor) ors)
  ]
  foreach ors
  [
    op-room ->
    let best-or-schedule (calculate-schedule
      (item 0 op-room) (item 1 op-room) (get-duration-hospital duration (item 1 op-room)) surgery-type surgery-specialty final-hosp-id (get-surgeon-hospital surgeon-per-hospital (item 1 op-room)))
    ;; best-or-schedule -> [pxcor pycor day time-block prep-time]
    set best-or-schedule (list (item 2 op-room) (item 3 op-room) (item 0 best-or-schedule) (item 1 best-or-schedule) (item 2 best-or-schedule))
    set available-schedules (lput best-or-schedule available-schedules)
  ]
  let best-schedule (get-best-schedule-surgery available-schedules)
  let assigned-or-hosp-id 0
  let s-dur duration
  let s-id surgery-id
  ask patches with [pxcor = (item 0 best-schedule) and pycor = (item 1 best-schedule)]
  [
    set assigned-or-hosp-id or-hosp-id
    insert-surgery (item 2 best-schedule) (item 0 (item 3 best-schedule)) (get-duration-hospital s-dur or-hosp-id) (item 4 best-schedule) s-id
  ]
  set actual-duration (get-duration-hospital duration assigned-or-hosp-id)
  set assigned-surgeon (get-surgeon-hospital surgeon-per-hospital assigned-or-hosp-id)
  set assigned-or-coords (list (item 0 best-schedule) (item 1 best-schedule))
  set prep-time (item 4 best-schedule)
  set start-time (item 0 (item 3 best-schedule))
  set assigned-day (item 2 best-schedule)
  set surgery-transfer-cost transfer-cost
  let a-surgeon assigned-surgeon
  let a-duration actual-duration
  let assigned-surgeon-or-coords assigned-or-coords
  ask surgeons with [surgeon-id = a-surgeon]
  [
    insert-surgery-surgeon (item 2 best-schedule) a-duration s-id
  ]
end

to update-global-variables [waiting-time s-prep-time s-transfer-cost]
  set total-surgeries (total-surgeries + 1)
  set total-waiting-time (total-waiting-time + waiting-time)
  set max-waiting-time (max (list max-waiting-time waiting-time))
  set total-prep-time (total-prep-time + s-prep-time)
  set max-prep-time (max (list max-prep-time prep-time))
  set average-waiting-time (total-waiting-time / total-surgeries)
  set average-prep-time (total-prep-time / total-surgeries)
  if s-transfer-cost > 0
  [
    set max-transfer-cost (max (list s-transfer-cost max-transfer-cost))
    set total-transfer-cost (total-transfer-cost + s-transfer-cost)
    set number-transfer (number-transfer + 1)
    set average-transfer-cost (total-transfer-cost / total-hospitals)
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SURGERY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-hospital-for-surgery [original-hospital specialty]
  let hosp-info []
  let original-hosp-info []
  ask hospitals [
    let metric 0
    if hospital-transfer = "surgeon occupancy"
    [
      set metric (get-total-occupied-time specialty)
    ]
    if hospital-transfer = "waiting time"
    [
      set metric (max (list get-min-last-day-or get-min-last-day-surgeon specialty))
    ]
    if hospital-transfer = "number surgeries"
    [
      set metric get-total-surgeries
    ]
    set hosp-info (lput (list hospital-id metric (calculate-transfer-cost original-hospital hospital-id) number-ors) hosp-info)
    if hospital-id = original-hospital
    [ set original-hosp-info (list hospital-id metric (calculate-transfer-cost original-hospital hospital-id) number-ors)]
  ]
  let total-metric 0
  let i 0
  while [i < (length hosp-info)] [
    set total-metric (total-metric + (item 1 (item i hosp-info)))
    set i (i + 1)
  ]
  let avg-metric (total-metric / (length hosp-info))
  let transfer? false
  ifelse hospital-transfer = "number surgeries"
  [
    let best-hospital original-hosp-info
    set i 0
    while [i < (length hosp-info)]
    [
      if (item 1 (item i hosp-info)) / (item 3 (item i hosp-info)) < 2 * (item 1 original-hosp-info) / (item 3 original-hosp-info) and ((item 2 best-hospital) = 0 or (item 2 best-hospital) > (item 2 (item i hosp-info)))
      [ set best-hospital (item i hosp-info) ]
      set i (i + 1)
    ]
    report (list (item 0 best-hospital) (item 2 best-hospital))
  ]
  [
    let best-hospital original-hosp-info
    set i 0
    while [i < (length hosp-info)]
    [
      if (item 1 (item i hosp-info)) < 2 * (item 1 original-hosp-info) and ((item 2 best-hospital) = 0 or (item 2 best-hospital) > (item 2 (item i hosp-info)))
      [ set best-hospital (item i hosp-info) ]
      set i (i + 1)
    ]
    report (list (item 0 best-hospital) (item 2 best-hospital))
  ]
end

to-report calculate-transfer-cost [ori-hosp dest-hosp]
  ifelse ori-hosp = dest-hosp
  [ report 0 ]
  [
    let x1 0
    let y1 0
    let x2 0
    let y2 0
    let dest-coords (list 0 0)
    let private? false
    ask hospitals with [hospital-id = ori-hosp]
    [
      set x1 hospital-x
      set y1 hospital-y
    ]
    ask hospitals with [hospital-id = dest-hosp]
    [
      set x2 hospital-x
      set y2 hospital-y
      if hospital-type = "private"
      [ set private? true ]
    ]
    let transfer-cost sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))
    if private? = true
    [ set transfer-cost (transfer-cost + 10) ]
    report transfer-cost
  ]
end

to-report get-best-schedule-surgery [available-schedules]
  let best-schedule (item 0 available-schedules)
  let index 0
  while [index < (length available-schedules)]
  [
    ifelse heuristic = "minimize-prep-time" and (item 4 (item index available-schedules)) < (item 4 best-schedule)
    [
      set best-schedule (item index available-schedules)
    ]
    [
      if heuristic = "minimize-waiting-time" and (item 2 (item index available-schedules)) < (item 2 best-schedule)
      [
        set best-schedule (item index available-schedules)
      ]
    ]
    set index (index + 1)
  ]
  report best-schedule
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OPERATING ROOM FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; operating rooms procedure to insert surgery in its schedule
to insert-surgery [s-day s-start-time s-duration s-prep-time s-surgery]
  while [(length or-schedule) <= s-day]
  [
    set or-schedule (insert-item (length or-schedule) or-schedule [])
  ]
  let or-schedule-day (item s-day or-schedule)
  set or-schedule-day (insert-item (length or-schedule-day) or-schedule-day s-surgery)
  set or-schedule (replace-item s-day or-schedule or-schedule-day)
end

;; operating room procedure to calculate surgery prep time
to-report calculate-prep-time [s-type s-specialty schedule-day]
  ;; set base preparation time to bring equipment to the room, according to type of surgery
  let s-prep-time 0
  ifelse s-type = "big"
  [set s-prep-time 40] ;; 40 minutes for equipment
  [
    ifelse s-type = "medium"
    [set s-prep-time 20] ;; 20 minutes for equipment
    [set s-prep-time 10] ;; 10 minutes for equipment
  ]
  let surgery-same-spec 1
  ask surgeries with [ (member? surgery-id schedule-day) = true]
  [
    if s-specialty = surgery-specialty
    [
      set surgery-same-spec 0
    ]
  ]
  set s-prep-time (s-prep-time * surgery-same-spec + 20) ;; 20 minutes for other activities
  report s-prep-time
end

;; operating rooms procedure to calculate and return the best schedule for surgery. return [day start-time prep-time]
to-report calculate-schedule [operating-room-schedule or-hospital-id s-duration s-type s-specialty s-hosp-id s-surgeon]
  let surgeon-last-day 0

  ask surgeons with [surgeon-id = s-surgeon]
  [
    set surgeon-last-day get-last-day
  ]
  let available-schedules [] ;; list of lists [[day prep-time [start-time end-time]]]
  let index 0
  while [index <= (length operating-room-schedule) or index <= surgeon-last-day]
  [
    ifelse index >= (length operating-room-schedule) ;; the whole last day is available
    [
      let s-prep-time (calculate-prep-time s-type s-specialty [])
      set available-schedules (lput (list index s-prep-time (list (list 0 (operating-hours * 60)))) available-schedules)
    ]
    [
      let schedule-day (item index operating-room-schedule)
      let s-prep-time (calculate-prep-time s-type s-specialty schedule-day)
      set available-schedules (lput (list index s-prep-time (get-day-free-time schedule-day or-hospital-id (s-duration + s-prep-time))) available-schedules)
    ]
    set index (index + 1)
  ]
  let surg-schedule []
  ask surgeons with [surgeon-id = s-surgeon]
  [
    set surg-schedule surgeon-schedule
  ]
  set available-schedules (check-surgeon-availability surg-schedule available-schedules s-duration)
  report (compute-best-schedule available-schedules)
end

;; compute best schedule out of schedules received as arguments taking into consideration the used heuristic
;; returns [day time-block prep-time]
to-report compute-best-schedule [available-schedules]
  let best-schedule (list (item 0 (item 0 available-schedules)) (item 0 (item 2 (item 0 available-schedules))) (item 1 (item 0 available-schedules)))
  let index 0
  while [index < (length available-schedules)]
  [
    ifelse heuristic = "minimize-prep-time"
    [
      if (item 1 (item index available-schedules)) < (item 2 best-schedule)
      [ set best-schedule (list (item 0 (item index available-schedules)) (item 0 (item 2 (item index available-schedules))) (item 1 (item index available-schedules)))]
    ]
    [
      if heuristic = "minimize-waiting-time"
      [
        if (item 0 (item 0 (item 2 (item 0 available-schedules)))) < (item 0 (item 1 best-schedule))
        [ set best-schedule (list (item 0 (item index available-schedules)) (item 0 (item 2 (item index available-schedules))) (item 1 (item index available-schedules)))]
      ]
    ]
    set index (index + 1)
  ]
  report best-schedule
end

;; obtain the free time blocks of a operating room in a day
to-report get-day-free-time [schedule-day or-hospital-id surgery-prep-duration] ;;surgery-duration -> int
  let free-time (list (list 0 (operating-hours * 60)))
  let index 0
  while [index < (length schedule-day)]
  [
    ask surgeries with [surgery-id = (item index schedule-day)]
    [
      let s-start-time start-time
      let s-duration actual-duration
      let s-prep-time prep-time

      let index-free-time 0
      let end-surgery (s-start-time + s-prep-time + s-duration)
      while [index-free-time < (length free-time)]
      [
        if end-surgery > (item 0 (item index-free-time free-time)) and s-start-time < (item 1 (item index-free-time free-time))
        [
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
    ]
    set index (index + 1)
  ]
  ;; remove available schedules where the surgery doesn't fit
  set index 0
  while [index < (length free-time)]
  [
    ifelse (item 1 (item index free-time)) - ((item 0 (item index free-time))) < surgery-prep-duration
    [set free-time (remove-item index free-time)]
    [set index (index + 1)]
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

;; parse surgery duration array to obtain duration for one hospital- s-surgeons contains list of [hospital-id surgeon-id surgeon-expertise]
to-report get-surgeon-hospital [s-surgeons or-hospital-id]
  let index 0
  while [index < (length s-surgeons)]
  [
    if (item 0 (item index s-surgeons)) = or-hospital-id
    [
      report (item 1 (item index s-surgeons))
    ]
    set index (index + 1)
  ]
  report -1
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HOSPITAL FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get surgeon of the specialty with the most free time. returns [hospital-id surgeon-id surgeon-expertise]
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

to-report get-min-last-day-surgeon [specialty]
  let min-last-day -1
  let hospit-id hospital-id
  ask surgeons with [surgeon-hosp-id = hospit-id and surgeon-specialty = specialty] [
    let last-day get-last-day
    ifelse min-last-day = -1
    [ set min-last-day last-day ]
    [ set min-last-day (min (list min-last-day last-day))]
  ]
  if min-last-day = -1
  [ set min-last-day 0 ]
  report min-last-day
end

to-report get-min-last-day-or
  let min-last-day -1
  let hospit-id hospital-id
  ask patches with [or-hosp-id = hospit-id] [
    let last-day (length or-schedule)
    ifelse min-last-day = -1
    [ set min-last-day last-day ]
    [ set min-last-day (min (list min-last-day last-day))]
  ]
  if min-last-day = -1
  [ set min-last-day 0 ]
  report min-last-day
end

to-report get-total-surgeries
  let total 0
  let hospit-id hospital-id
  ask surgeries with [hospit-id = hosp-id]
  [
    if final-hosp-id = 0
    [ set total (total + 1) ]
  ]
  report total
end

to-report get-total-occupied-time [specialty]
  let s-occupied-time 0
  ask surgeons with [surgeon-hosp-id = hospital-id and surgeon-specialty = specialty] [
    set s-occupied-time (s-occupied-time + occupied-time)
  ]
  report s-occupied-time
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SURGEON FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; obtain a surgeons occupied time. returns [surgeon-id hospital-id occupied-time expertise]
to-report get-occupied-time
  report (list surgeon-id surgeon-hosp-id occupied-time surgeon-expertise)
end

to-report get-last-day
  report length surgeon-schedule
end

;; check available schedules for surgeon
to-report check-surgeon-availability [surg-schedule available-schedules surgery-duration] ;; available-schedules -> list of [day prep-time [[start-time end-time] ...]]
  let index 0
  set surg-schedule (obtain-schedule surg-schedule)
  while [index < (length available-schedules)]
  [
    let day-available-schedule (item 2 (item index available-schedules))
    let day (item 0 (item index available-schedules))
    if day < (length surg-schedule) ;; if surgeon has surgeries scheduled in this day or days after
    [
      if not empty? (item day surg-schedule) ;; if surgeon has surgeries scheduled in this day -> compute times where both the OR and the surgeon are available
      [
        let surg-day-schedule (item day surg-schedule)
        let index-day-or 0
        while [index-day-or < (length day-available-schedule)]
        [
          let das (item index-day-or day-available-schedule)
          let index-day-s 0
          while [index-day-s < (length surg-day-schedule) and das != [] ]
          [
            let sds (item index-day-s surg-day-schedule)
            if (item 1 sds) > (item 0 das) and (item 0 sds) < (item 1 das)
            [
              let index-to-add (index-day-or + 1)
              if (item 0 sds) > (item 0 das)
              [
                set day-available-schedule (insert-item index-to-add day-available-schedule (list (item 0 das) (item 0 sds)))
                set index-to-add (index-to-add + 1)
              ]

              if (item 1 sds) < (item 1 das)
              [ set day-available-schedule (insert-item index-to-add day-available-schedule (list (item 1 sds) (item 1 das))) ]

              set day-available-schedule (remove-item index-day-or day-available-schedule)
              ifelse (length day-available-schedule) < index-day-or
              [ set das (item index-day-or day-available-schedule) ]
              [ set das [] ]
            ]
            set index-day-s (index-day-s + 1)
          ]
          set index-day-or (index-day-or + 1)
        ]
      ]
    ]
    let replacement (replace-item 2 (item index available-schedules) day-available-schedule)
    set available-schedules (replace-item index available-schedules replacement)
    set index (index + 1)
  ]

  ;; remove from available schedules the schedules in which the surgery doesn't fit
  set index 0
  while [index < (length available-schedules)]
  [
    let day-available-schedule (item 2 (item index available-schedules))
    let day-prep-time (item 1 (item index available-schedules))
    if not empty? day-available-schedule
    [
      let index-day 0
      while [index-day < (length day-available-schedule)]
      [
        ifelse (item 1 (item index-day day-available-schedule)) - (item 0 (item index-day day-available-schedule)) < (surgery-duration + day-prep-time)
        [
          set day-available-schedule (remove-item index-day day-available-schedule)
          set available-schedules (replace-item index available-schedules (list (item 0 (item index available-schedules)) day-prep-time day-available-schedule))
        ]
        [set index-day (index-day + 1)]
      ]
    ]
    if empty? day-available-schedule
    [
      set available-schedules (remove-item index available-schedules)
      set index (index - 1)
    ]
    set index (index + 1)
  ]
  report available-schedules
end

;; translate surgery ids to time blocks
to-report obtain-schedule [s-schedule]
  let time-block-schedule []
  let index 0
  while [index < (length s-schedule)]
  [
    let schedule-day (item index s-schedule)
    let day-schedule []
    let index-j 0
    while [index-j < (length schedule-day)]
    [
      ask surgeries with [surgery-id = (item index-j schedule-day)]
      [
        let s-start-time (start-time + prep-time)
        let s-end-time (s-start-time + actual-duration)
        set day-schedule (lput (list s-start-time s-end-time) day-schedule)
      ]
      set index-j (index-j + 1)
    ]
    set time-block-schedule (lput day-schedule time-block-schedule)
    set index (index + 1)
  ]
  report time-block-schedule
end

;; insert surgery in surgeon's schedule TESTED
to insert-surgery-surgeon [s-day s-duration s-surgery]
  while [(length surgeon-schedule) <= s-day]
  [
    set surgeon-schedule (lput [] surgeon-schedule)
  ]
  let surgeon-schedule-day (item s-day surgeon-schedule)
  set surgeon-schedule-day (insert-item (length surgeon-schedule-day) surgeon-schedule-day s-surgery)
  set surgeon-schedule (replace-item s-day surgeon-schedule surgeon-schedule-day)
  set occupied-time (occupied-time + s-duration)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOAD DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; id,urgency,surgery-type,surgery-specialty
to create-surgeries-data
  file-open (word data-folder "/surgeries.csv")
  while [ not file-at-end? ] [
    let data csv:from-row file-read-line
    create-surgeries 1 [
      set size 0.8
      set shape "face sad"
      set duration []
      set surgery-id item 0 data
      set urgency item 1 data
      set surgery-type item 2 data
      set surgery-specialty item 3 data
      set hosp-id item 4 data
      set final-hosp-id 0
      let random-x 0
      let random-y 0
      ifelse random 1 = 1
      [set random-x (random-float 1)]
      [set random-x (random-float -1)]
      ifelse random 1 = 1
      [set random-y (random-float 1)]
      [set random-y (random-float -1)]
      set xcor -13 + random-x
      set ycor (16 - hosp-id * 4 + random-y)
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
  let last-or 0
  let or-number 0
  while [ not file-at-end? ] [
    let data csv:from-row file-read-line
    let hospital-color (random 140)
    create-hospitals 1 [
      set size 2
      set color black
      setxy 15 15
      set hospital-id item 0 data
      set hospital-color (hospital-id * 10 + 5)
      set hospital-type item 1 data
      set or-number item 2 data
      set number-ors or-number
      set hospital-x item 3 data
      set hospital-y item 4 data
    ]
    let i 0
    while [i < or-number]
    [
      let y (15 - ((last-or * 4 + 2) mod 30))
      let x (5 - (floor ((last-or * 4 + 2) / 30)) * 4)
      ask patches with [pxcor = x and pycor = y] [
        set pcolor hospital-color
        set or-hosp-id item 0 data
        set or-schedule []
      ]
      set i (i + 1)
      set last-or (last-or + 1)
    ]
  ]
  file-close
end

;; id,specialty,hospital, expertise
to create-surgeons-data
  file-open (word data-folder "/surgeons.csv")
  while [ not file-at-end? ] [
    let data csv:from-row file-read-line
    create-surgeons 1 [
      set size 1.5
      set color 5
      set shape "person doctor"
      set surgeon-id item 0 data
      set surgeon-specialty item 1 data
      set surgeon-hosp-id item 2 data
      set surgeon-schedule []
      set occupied-time 0
      let random-x 0
      let random-y 0
      ifelse random 1 = 1
      [set random-x (random-float 1)]
      [set random-x (random-float -1)]
      ifelse random 1 = 1
      [set random-y (random-float 1)]
      [set random-y (random-float -1)]
      set surgeon-init-pos (list (13 + random-x) (16 - surgeon-hosp-id * 4 + random-y))
      set xcor 13 + random-x
      set ycor (16 - surgeon-hosp-id * 4 + random-y)
      let expertise item 3 data
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SAVE DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save schedule in csv file
to save-schedule
  let schedules []
  ask patches with [or-hosp-id != 0] [
    set schedules (lput (list or-hosp-id or-schedule) schedules)
  ]
  let i 0
  foreach schedules [
    schedule -> print-schedule schedule i
    set i (i + 1)
  ]
end

to print-schedule [schedule n]
  let j 0
  let i 0
  let filename (word "data/results/or-" n "_hosp-" (item 0 schedule) ".csv")
  let info []
  while [i < (length (item 1 schedule))] [ ;;for each day
    while [j < (length (item i (item 1 schedule)))] [ ;;for each surgery
      ask surgeries with [ surgery-id = (item j (item i (item 1 schedule))) ] [
        set info (lput (list "---------- Surgery " surgery-id "-----------") info)
        set info (lput (list "urgency: " urgency) info)
        set info (lput (list "surgery-type: " surgery-type) info)
        set info (lput (list "surgery-specialty: " surgery-specialty) info)
        set info (lput (list "assigned-surgeon: " assigned-surgeon) info)
        set info (lput (list "final-hosp-id:" final-hosp-id) info)
        set info (lput (list "assigned-day: " assigned-day) info)
        set info (lput (list "start-time: " start-time) info)
        set info (lput (list "end-time: " (start-time + prep-time + actual-duration)) info)
        set info (lput (list "prep-time: " prep-time) info)
        set info (lput (list "actual-duration: " actual-duration) info)
        set info (lput (list "") info)
      ]
      set j (j + 1)
    ]
    set i (i + 1)
    set j 0
  ]
  csv:to-file filename info
end
@#$#@#$#@
GRAPHICS-WINDOW
413
18
886
492
-1
-1
15.0
1
10
1
1
1
0
0
0
1
-15
15
-15
15
1
1
1
ticks
30.0

BUTTON
129
212
299
245
Allocate Operating Blocks
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
28
41
207
86
heuristic
heuristic
"minimize-prep-time" "minimize-waiting-time"
1

BUTTON
130
172
299
205
Setup Experiment
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
28
95
206
155
data-folder
data/overflow
1
0
String

SLIDER
218
107
397
140
operating-hours
operating-hours
8
16
8.0
1
1
NIL
HORIZONTAL

PLOT
908
46
1108
196
Waiting Time
Ticks
Waiting Time
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Average" 1.0 0 -2674135 true "" "plot average-waiting-time"

MONITOR
1047
222
1172
267
Average prep time
average-prep-time
2
1
11

MONITOR
1066
276
1158
321
Max prep time
max-prep-time
2
1
11

MONITOR
910
277
1035
322
Max waiting time
max-waiting-time
2
1
11

MONITOR
908
222
1037
267
Average waiting time
average-waiting-time
2
1
11

MONITOR
29
337
124
382
Total surgeries
total-surgeries
17
1
11

PLOT
1117
46
1317
196
Preparation Time
Ticks
Preparation Time
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13840069 true "" "plot average-prep-time"

CHOOSER
217
41
397
86
hospital-transfer
hospital-transfer
"none" "waiting time" "surgeon occupancy" "number surgeries"
1

MONITOR
151
400
246
445
Total surgeons
total-surgeons
17
1
11

MONITOR
30
400
123
445
Total hospitals
total-hospitals
17
1
11

MONITOR
149
337
245
382
Number of ORs
total-or
17
1
11

BUTTON
129
252
300
285
Show Allocation Results
show-results
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1206
274
1305
319
Number Transfers
number-transfer
2
1
11

MONITOR
1185
221
1320
266
Average transfer cost
average-transfer-cost
2
1
11

TEXTBOX
32
304
182
329
DATA:
15
0.0
1

TEXTBOX
907
15
1057
34
RESULTS:
15
0.0
1

TEXTBOX
30
14
180
33
System Setup:\n
15
0.0
1

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

person doctor
false
0
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -13345367 true false 135 90 150 105 135 135 150 150 165 135 150 105 165 90
Polygon -7500403 true true 105 90 60 195 90 210 135 105
Polygon -7500403 true true 195 90 240 195 210 210 165 105
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -1 true false 105 90 60 195 90 210 114 156 120 195 90 270 210 270 180 195 186 155 210 210 240 195 195 90 165 90 150 150 135 90
Line -16777216 false 150 148 150 270
Line -16777216 false 196 90 151 149
Line -16777216 false 104 90 149 149
Circle -1 true false 180 0 30
Line -16777216 false 180 15 120 15
Line -16777216 false 150 195 165 195
Line -16777216 false 150 240 165 240
Line -16777216 false 150 150 165 150

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
NetLogo 6.1.1
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
