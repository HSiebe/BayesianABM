extensions [ table r ]

turtles-own [ observations testimonies opinion vote ]
globals [ modelstring target-node truth all-observations common-observations votes silent-agents n-correct-votes n-correct-votes-all-runs time-to-correct-consensus exchange-sequence undisclosed-observations is-hidden-profile? n-hidden-profile-generation-attempts net-filename ]


to generate-BNs
  ; just generate and save Bayesian network
  setup-r
  setup-bn
  save-bn
  reset-ticks
end

to setup-new-bn
  set n-hidden-profile-generation-attempts 1
  ; set default of 100 attempts to generate a hidden profile
  if not (max-n-attempts-to-generate-hp > 0) [
    set max-n-attempts-to-generate-hp 100
  ]

  setup-r
  create-agents
  setup-bn
  save-bn

  set modelstring r:get "generate_modelstring(junction)"

  setup-observations
  ; restart setup to generate a new BN if no hidden profile could be generated after the maximum number of attempts
  if n-correct-votes > 0 [
    setup-new-bn
  ]
  set is-hidden-profile? n-correct-votes = 0
  reset-ticks
end


to setup-existing-bn
  set n-hidden-profile-generation-attempts 1
  ; set default of 100 attempts to generate a hidden profile
  if not (max-n-attempts-to-generate-hp > 0) [
    set max-n-attempts-to-generate-hp 100
  ]

  setup-r
  create-agents
  load-bn

  set modelstring r:get "generate_modelstring(junction)"

  setup-observations
  set is-hidden-profile? n-correct-votes = 0
  reset-ticks
end



; use this to run experiment on the same BN and distribution of observations (this only varies the communication sequence)
to clear-shared-evidence
  clear-plot
  ; clear silent agents and exchange sequence
  set silent-agents []
  set exchange-sequence []

  ; clear earlier shared observations
  ask turtles [
    set testimonies table:make
  ]

  ; reset junctionUpdate
  r:eval "junctionUpdate <- junction"

  ; recalculate opinions and votes
  calculate-agent-opinions
  if print-statements [ show (word "opinions: " [ opinion ] of turtles) ]
  calculate-votes

  set n-correct-votes report-n-correct-votes
  if print-statements [ show (word "number correct votes: " n-correct-votes) ]
  ; restart procedure if not hidden profile
  if n-correct-votes > 0 [
    ; clear-shared-evidence
  ]
  reset-ticks
end



to go
  if n-hidden-profile-generation-attempts > max-n-attempts-to-generate-hp [
    ; terminate if no hidden profile could be generated within the specified number of attempts.
    stop
  ]
  if disable-deliberation [
   ; if we are only interested in collecting data on the generation of hidden profile generation,
   ; we can terminate the simulation run here.
   stop
  ]

  ; deliberation stops when no agent has anything left to say.
  if length silent-agents = n-agents [
    set undisclosed-observations filter [x -> not member? x exchange-sequence] (remove target-node r:get "junction$universe$nodes")
    if print-statements [ output-print (word "Nobody has observations to share that fit the strategy; undisclosed observations: " undisclosed-observations) ]
    stop
  ]

  let update-time-to-correct-consensus? not (n-correct-votes = n-agents)

  ask one-of turtles with [ not member? who silent-agents ] [
    share-observations
  ]
  if print-statements [ print (word "silent: " silent-agents) ]
  calculate-agent-opinions
  calculate-votes
  set n-correct-votes report-n-correct-votes
  if (n-correct-votes = n-agents) and update-time-to-correct-consensus? [ set time-to-correct-consensus ticks + 1]
  if print-statements [ print (word "number of correct votes: " n-correct-votes) ]
  plot-opinion-evolution
  tick
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP PROCEDURES

to setup-agent-observations
  ; populate shared-observations and agent observations with common observations
  set common-observations table:make
  let common-observations-list n-of n-common-observations table:to-list all-observations
  if print-statements [ output-print (word "Common observations: " common-observations-list) ]

  foreach common-observations-list [i ->
    table:put common-observations (first i) (last i)
  ]

  ; distribute the rest of the observations as equally as possible among the agents
  let unique-observations filter [i -> not member? i common-observations-list ] table:to-list all-observations
  if print-statements [ output-print (word "Unique observations: " unique-observations) ]

  let i []
  while [length unique-observations > 0][
    ask turtles [
      if length unique-observations > 0 [
        set i one-of unique-observations
        set unique-observations remove i unique-observations
        table:put observations (first i) (last i)
        ;show unique-observations
      ]
    ]
  ]
end

to calculate-agent-opinions
  ask turtles [
    let observations-string "list("
    let total-observations remove-duplicates (sentence table:to-list observations table:to-list testimonies table:to-list common-observations)

    ; agents without any source of evidence don't form an opinion
    if (length total-observations > 0) [
      foreach total-observations [observation ->
        set observations-string (word observations-string "\"" (first observation) "\"=\"" (last observation) "\", ")
      ]
      ;show observations-string
      set observations-string but-last but-last observations-string
      let command (word "evaluateEvidence(junctionUpdate, \"" (word target-node "\", ")  observations-string "))")
      ;show command
      set opinion first r:get command
      set label precision opinion 3
    ]
  ]
end

to calculate-votes
  let pdistance (1 / n-policy-options)
  let keys n-values n-policy-options [i -> i]

  foreach but-last keys [key ->
    table:put votes key count turtles with [ opinion >= key * pdistance and opinion < (key + 1) * pdistance ]
    ; for the last key we need to include certainty (probability of one) in the bin:
    table:put votes (last keys) count turtles with [ opinion >= (last keys) * pdistance and opinion <= ((last keys) + 1) * pdistance ]
  ]

  ; set votes on turtles too
  foreach but-last keys [key ->
    ask turtles [
      if opinion >= key * pdistance and opinion < (key + 1) * pdistance [ set vote key ]
    ]
  ]
  ; for the last position, we need to include certainty (probability of one) in the bin
  ask turtles [
    if opinion >= ((last keys) * pdistance) and opinion <= ((last keys) + 1) * pdistance [ set vote (last keys) ]
  ]
end


to-report report-n-correct-votes
  ; what is the correct vote?
  let pdistance (1 / n-policy-options)
  let keys n-values n-policy-options [i -> i]

  foreach but-last keys [key ->
    if (truth >= key * pdistance and truth < (key + 1) * pdistance) [
      ; report number of initial votes for the correct alternative
      report table:get votes key
    ]
  ]
  if-else (truth >= (last keys) * pdistance and truth <= ((last keys) + 1) * pdistance) [
      ; report number of initial votes for the correct alternative
      report table:get votes (last keys)
  ][
    print "Error in correct votes calculation"
  ]
end

to setup-r
  clear-all
  r:clear
  r:eval ".libPaths(\"[path to]/R/win-library/4.0\")"
  r:setPlotDevice
  r:eval "source(\"[path to]/BNcode.R\")"
  r:eval "setwd(\"[path to working directory]/networks\")"
end


to create-agents
  ;if n-agents > (n-total-observations - n-common-observations) [set n-agents (n-total-observations - n-common-observations)]
  create-turtles n-agents [
    forward 8
    set shape "person"
    set size 5
    set label-color black
  ]
    ask patches [
    set pcolor white
  ]
end

to initiate-variables
  set undisclosed-observations []
  set exchange-sequence []
  set all-observations table:make
  set common-observations table:make
  set votes table:make
  set silent-agents []
  set time-to-correct-consensus 999

  ask turtles [
    set observations table:make
    set testimonies table:make
  ]
end

to setup-bn
  r:eval (word "dag <- generateGraph(" (n-total-observations + 1) ")")
  r:eval "setBinaryLevels(dag)"
  ;show r:get "ls()"
  r:eval "generateParameters(dag)"
  r:eval "cpt <- generateConditionalProbabilityTable(dag)"
  r:eval "bn <- custom.fit(dag, cpt)"
  r:eval "junction <- compile(as.grain(bn))"

  ; copy for reasoning
  r:eval "junctionUpdate <- junction"
end

to load-bn
  if length behaviorspace-experiment-name > 0 [
    set net-filename (word (floor ((behaviorspace-run-number - 1) / n-experiments-per-network) + 1) ".net")
    r:eval (word "junction <- loadHuginNet(\"" net-filename "\")" )
    ; copy for reasoning
    r:eval "junctionUpdate <- junction"
  ]

  ; does filename have to be empty in order for behaviourspace experiment to work?
  if length filename > 0 [
    r:eval (word "junction <- loadHuginNet(\"" filename ".net\")" )
    ; copy for reasoning
    r:eval "junctionUpdate <- junction"
  ]
end

to save-bn
  if behaviorspace-run-number > 0 [
    r:eval (word "saveHuginNet(junction, \"" behaviorspace-run-number ".net\")" )
    set net-filename (word behaviorspace-run-number ".net")
  ]
end


to create-all-observations
  ; for all nodes except target-node, choose either randomly between "yes" and "no" or let observations be biased by BN
  ;let nodes r:get "nodes(bn)"
  let nodes r:get "junction$universe$nodes"
  if print-statements [ print (word "Nodes: " nodes ", leaf.nodes: " r:get "leaf.nodes(as.bn.fit(junction, including.evidence = TRUE))") ]
  ;set target-node one-of nodes
  let leafs r:get "leaf.nodes(as.bn.fit(junction, including.evidence = TRUE))"
  ; set target-node (one of) the graph leaf(s)
  set target-node ifelse-value (is-list? leafs) [ one-of leafs ] [ leafs ]


  ifelse likely-evidence [
    ;; Sample the network to generate observations based on the probabilistic dependencies specified in the network structure and the associated conditional probability tables.
    let realization r:get "dataFrameToListOfLists(rbn(as.bn.fit(junction), n = 1))"
    foreach realization [observation-list ->
     if (first observation-list) != target-node [
        table:put all-observations (first observation-list)(last observation-list)
      ]
    ]
  ][
    ;; Generate completely random evidence, rather than sampling the network
    foreach nodes [node ->
      if node != target-node [
        table:put all-observations node (ifelse-value random 2 = 1 ["yes"]["no"])
      ]
    ]
  ]
  if print-statements [ print (word "all observations: " table:to-list all-observations) ]
end


to determine-truth
  clear-output
  r:eval "truth <- junction"
  let command ""
  let observations-string "list("

  foreach table:to-list all-observations [observation ->
    if first observation != target-node [
      set observations-string (word observations-string "\"" (first observation) "\"=\"" (last observation) "\", ")
    ]
  ]
  set observations-string but-last but-last observations-string
  set command (word "truth <- updateBeliefs(truth, " observations-string "))")
  r:eval command

  set truth first r:get (word "getPosterior(truth, \"" target-node "\")")
  if print-statements [ output-print (word "Truth: P(" target-node " = \"yes\") = " precision truth 3) ]
end


to setup-observations
  initiate-variables
  create-all-observations
  determine-truth

  setup-agent-observations
  calculate-agent-opinions
  if print-statements [ print (word "opinions: " [ opinion ] of turtles) ]
  calculate-votes
  if print-statements [ print (word "number of initially correct votes: " report-n-correct-votes) ]
  set n-correct-votes report-n-correct-votes

  set n-hidden-profile-generation-attempts (n-hidden-profile-generation-attempts + 1)
  ; print (word "generation attempt: " n-hidden-profile-generation-attempts)

  ;; keep track of number of correct votes before resampling the Bayesian network
  if n-correct-votes-all-runs = 0 [
    set n-correct-votes-all-runs []
  ]
  set n-correct-votes-all-runs lput n-correct-votes n-correct-votes-all-runs

  ifelse not always-make-max-attempts [
    ; generate a new realization of the evidence if no hidden profile
    if n-correct-votes > 0 and n-hidden-profile-generation-attempts < max-n-attempts-to-generate-hp [
      setup-observations
    ]
  ][
    ; generate a new realization of the evidence if no hidden profile
    if n-hidden-profile-generation-attempts < max-n-attempts-to-generate-hp [
      setup-observations
    ]
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GO PROCEDURES

; turtle procedure
to share-observations
  let observation []

  ; available for sharing: private observations + common observations
  let available-observations shuffle sentence table:to-list observations table:to-list common-observations

  ; remove unique or common observations that are already shared (i.e. exist in testimonies)
  foreach available-observations [candidate ->
    if member? (first candidate) table:keys testimonies [ set available-observations remove candidate available-observations ]
  ]
  if print-statements [ print (word "available observations: " available-observations) ]

  if strategy = "random" [
    ifelse length available-observations > 0 [
      ; share one of privately available observations randomly
      set observation one-of available-observations
      ; add to own store of testimonies
      table:put testimonies (first observation) (last observation)
    ][
      set observation []
    ]
  ]

  if strategy = "advocacy" [
    if support-type = "confirmation" [
      ; argument must extremize opinion towards the preferred vote
      set observation (report-advocacy-confirming-observation available-observations)
    ]
    if support-type = "compatibility" [
      ; argument must result in an opinion that remains in the bin that corresponds with the preferred vote
      set observation (report-advocacy-compatible-observation available-observations)
    ]
  ]

  if strategy = "conformity" [
    let counts table:values votes
    let majority-vote position (max counts) counts

    if support-type = "confirmation" [
      ; argument must extremize opinion towards the preferred vote
      set observation (report-majority-confirming-observation majority-vote available-observations)
    ]
    if support-type = "compatibility" [
      ; argument must result in an opinion that remains in the bin that corresponds with the preferred vote
      set observation (report-majority-compatible-observation majority-vote available-observations)
    ]
  ]

  if print-statements [ show (word "my shared observation: " observation) ]
  ifelse length observation > 0 [
    set silent-agents [] ; reset silent-agents because someone who was previously silent might now have something to share
    set exchange-sequence lput (first observation) exchange-sequence

    ; add observation to all other agents' stores of testimonies
    ask other turtles [
      table:put testimonies (first observation) (last observation)
    ]
  ][
    set silent-agents remove-duplicates sentence silent-agents [ who ] of self ; add agent to silent-agents
  ]
end


; turtle procedure
; an observation is counted as confirmation as long as it keeps the target node's probability in the preferred bin
to-report report-advocacy-compatible-observation [ available-observations ]
  ; find lower and upper bounds for the position that is supported by self
  let pdistance (1 / n-policy-options)
  let lower pdistance *  vote
  let upper pdistance * (vote + 1)
  if print-statements [ show (word "my preferred policy is supported by the probabilities between " lower " and " upper ".") ]

  let result 0

  ; check whether a previously unshared private observation supports the majority position conditional on earlier shared testimony)
  foreach available-observations [candidate ->
    set result posterior-given-candidate-argument candidate
    if print-statements [ show (word "opinion compatibility check for " candidate ": is " result " >= " lower " and <= " upper "? " (result >= lower and result <= upper)) ]
    if result >= lower and result <= upper [
      table:put testimonies (first candidate) (last candidate)
      report candidate
    ]
  ]
  ; otherwise, report an empty observation []
  report []
end


to-report report-advocacy-confirming-observation [ available-observations ]
  let result 0
  let prior-opinion 0

  ; get opinion given only already shared testimony, i.e. without considering undisclosed candidate argument
  set prior-opinion (posterior-given-candidate-argument [])
  if print-statements [ show (word "opinion given previous testimony: " prior-opinion) ]

  ; check whether a previously unshared private observation supports own position conditional on earlier shared testimony)
  foreach available-observations [candidate ->
    set result posterior-given-candidate-argument candidate

    let denominator (n-policy-options - 1)
    let numerator vote
    let target numerator / denominator

    if print-statements [ show (word "opinion given both previous testimony and " candidate ": " result ". opinion support check: is this closer to bin target " target " then opinion given only previous testimony " prior-opinion "? " (abs (target - result) < abs (target - prior-opinion))) ]
    if abs (target - result) < abs (target - prior-opinion) [
      table:put testimonies (first candidate) (last candidate)
      report candidate
    ]
  ]
  ; otherwise, report an empty observation []
  report []
end

; turtle procedure
; an observation is counted as confirmation as long as it keeps the target node's probability in the preferred bin
to-report report-majority-compatible-observation [ majority-vote available-observations ]
  ; find lower and upper bounds for the position that is supported by majority
  let pdistance (1 / n-policy-options)
  let lower pdistance *  majority-vote
  let upper pdistance * (majority-vote + 1)
  if print-statements [ show (word "the most popular policy option is supported by the probabilities between " lower " and " upper ".") ]

  let result 0

  ; check whether a previously unshared private observation supports the majority position conditional on earlier shared testimony)
  foreach available-observations [candidate ->
    set result posterior-given-candidate-argument candidate
    if print-statements [ show (word "opinion compatibility check for " candidate ": is " result " >= " lower " and <= " upper "? " (result >= lower and result <= upper)) ]
    if result >= lower and result <= upper [
      table:put testimonies (first candidate) (last candidate)
      report candidate
    ]
  ]
  ; otherwise, report an empty observation []
  report []
end

; turtle procedure
; actual confirmation: ; the target node's probability should get further removed from alternative bins, i.e. closer to the preferred bin target.
; the bin target is the position furtherst removed from alternative bins.
; with 2 options: 0/1 and 1/1
; with 3 options: 0/2, 1/2, 2/2
; with 4 options: 0/3, 1/3, 2/3, 3/3
; etc!
to-report report-majority-confirming-observation [ majority-vote available-observations ]
  let result 0
  let prior-opinion 0

  ; get opinion given only already shared testimony, i.e. without considering undisclosed candidate argument
  set prior-opinion (posterior-given-candidate-argument [])
  if print-statements [ show (word "opinion given previous testimony: " prior-opinion) ]

  ; check whether a previously unshared private observation supports the majority conditional on earlier shared testimony)
  foreach available-observations [candidate ->
    set result posterior-given-candidate-argument candidate

    let denominator (n-policy-options - 1)
    let numerator majority-vote
    let target numerator / denominator

    if print-statements [ show (word "opinion given both previous testimony and " candidate ": " result ". opinion support check: is this closer to bin target " target " then opinion given only previous testimony " prior-opinion "? " (abs (target - result) < abs (target - prior-opinion))) ]
    if abs (target - result) < abs (target - prior-opinion) [
      table:put testimonies (first candidate) (last candidate)
      report candidate
    ]
  ]
  ; otherwise, report an empty observation []
  report []
end

; turtle procedure
; reports the opinion given previously shared testimony and an (optional) candidate argument
; if no candidate argument is to be provided, pass an empty list instead.
to-report posterior-given-candidate-argument [ candidate ]
  let evidence-list []
  let observations-string "list("
  if-else length candidate > 0 [
    set evidence-list (sentence (list(candidate)) table:to-list testimonies)
  ][
    set evidence-list table:to-list testimonies
  ]
  foreach evidence-list [obs ->
    set observations-string (word observations-string "\"" (first obs) "\"=\"" (last obs) "\", ")
  ]
  ; if list of evidence to conditionalize on is not empty, remove the comma and horizontal space from the string in order to get the formatting right
  if length evidence-list > 0 [ set observations-string but-last but-last observations-string ]
  let command (word "evaluateEvidence(junctionUpdate, \"" (word target-node "\", ")  observations-string "))")
  if print-statements [ show command ]
  report first r:get command
end



to show-bn
  ; Configure graphviz figure margins to avoid error
  r:eval "par(mar=c(1,1,1,1))"
  r:eval "graphviz.plot(as.bn.fit(junction, including.evidence = TRUE))"
end


; plotting
to plot-opinion-evolution
  set-current-plot "opinion-evolution-plot"
  set-plot-pen-mode 0
  ask turtles [
    create-temporary-plot-pen (word "agent-" who)
    set-plot-pen-color color
    plotxy ticks precision opinion 3
  ]
  let pdistance 1.0 / n-policy-options
  let counter pdistance
  while [counter < 1][
    create-temporary-plot-pen (word "option-" counter)
    set-plot-pen-color black
    set-plot-pen-mode 2
    plotxy ticks counter
    set counter counter + pdistance
  ]
end

to print-actual-n-runs
  print (word "The correct number of total runs for this experiment should be: " (n-experiments-per-network * n-networks))
end
@#$#@#$#@
GRAPHICS-WINDOW
577
10
829
263
-1
-1
7.4
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

SLIDER
16
10
188
43
n-agents
n-agents
2
15
7.0
1
1
NIL
HORIZONTAL

BUTTON
837
215
965
248
NIL
setup-new-bn
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
909
253
964
286
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
839
427
967
460
NIL
show-bn\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
200
378
830
461
11

PLOT
199
10
571
373
opinion-evolution-plot
time
opinion
0.0
6.0
0.0
1.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -7500403 true "" ""

BUTTON
837
291
965
324
NIL
clear-shared-evidence
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
16
54
188
87
n-total-observations
n-total-observations
1
15
11.0
1
1
NIL
HORIZONTAL

SLIDER
16
139
188
172
n-policy-options
n-policy-options
2
5
2.0
1
1
NIL
HORIZONTAL

SLIDER
16
96
188
129
n-common-observations
n-common-observations
0
3
4.0
1
1
NIL
HORIZONTAL

BUTTON
837
253
892
286
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
0

CHOOSER
15
181
187
226
strategy
strategy
"random" "advocacy" "conformity"
2

MONITOR
577
281
828
326
n-correct-votes
n-correct-votes
0
1
11

MONITOR
577
327
828
372
NIL
exchange-sequence
0
1
11

BUTTON
837
120
965
180
NIL
setup-existing-bn
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
839
393
967
426
print-statements
print-statements
1
1
-1000

INPUTBOX
837
58
966
118
filename
NIL
1
0
String

TEXTBOX
838
12
963
66
file must be in same folder (omit extention in filename)
11
0.0
1

TEXTBOX
839
346
960
364
analysis
11
0.0
1

CHOOSER
14
235
186
280
support-type
support-type
"confirmation" "compatibility"
0

PLOT
974
10
1354
412
votes-evolution-plot
time
n-correct-votes
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot n-correct-votes"

INPUTBOX
1379
243
1534
303
n-experiments-per-network
1.0
1
0
Number

TEXTBOX
1381
171
1531
241
Number of runs per network (copy from Behavior Space modal after setting number of desired repetitions per condition for each network):
11
0.0
1

MONITOR
974
416
1132
461
NIL
time-to-correct-consensus
17
1
11

INPUTBOX
1380
352
1535
412
n-networks
1000.0
1
0
Number

TEXTBOX
1380
308
1530
350
Number of *.net files in dir, named in sequential order from 1.net:
11
0.0
1

BUTTON
1380
417
1535
463
NIL
print-actual-n-runs\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1381
10
1531
136
Behavior Space\n\n\n\n\n\n\nMake sure to only run 1 instance (thread) at a time!
11
0.0
1

SWITCH
15
289
184
322
likely-evidence
likely-evidence
0
1
-1000

SWITCH
14
425
177
458
disable-deliberation
disable-deliberation
0
1
-1000

TEXTBOX
16
404
183
432
Only simulate prevalence of HPs:
11
0.0
1

INPUTBOX
14
341
178
401
max-n-attempts-to-generate-hp
200.0
1
0
Number

SWITCH
839
361
967
394
debug
debug
1
1
-1000

SWITCH
12
499
192
532
always-make-max-attempts
always-make-max-attempts
0
1
-1000

TEXTBOX
16
466
166
494
If you don't want to stop when a HP is generated:
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This is a Bayesian agent-based model of persuasive argument exchange that represents relations between arguments using Bayesian networks. We generate complex hidden profiles and simulate group deliberation with different communications strategies, e.g. advocacy of own opinion, conformity with majority opinion, and random.

## HOW IT WORKS

Agent selects an observation to share with all other agents from her store 'observations', according to her strategy. If an observation matches the strategy, she removes it from her 'observations' and adds it to everyone's store 'testimonies'.

If no observation matches the strategy, an empty observation is shared and nobody updates their opinions. If each agents has shared at least one empty observation in sequence, the simulation stops.


## HOW TO USE IT

In behaviorspace, select either the random experiment to generate a series of random graphs, or [integer]. The latter loads the graph from the corresponding random graph experiment run. That is, an experiment with name '5' uses the graph from the 5th behaviourspace-run.

You can also manually type a number for the graph you want to load. Or, create a new random graph with setup-new-bn button (this graph will be saved as '0.net', possibly overwriting an existing file!).


## CREDITS AND REFERENCES

Siebe, H. (2024). Modelling the prevalence of hidden profiles with complex argument structures. Proceedings of the Annual Meeting of the Cognitive Science Society. https://escholarship.org/uc/item/1kg1k9dn
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
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="generate-BNs" repetitions="1000" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>generate-BNs</setup>
    <go>go</go>
    <final>r:stop</final>
    <metric>n-correct-votes</metric>
    <metric>exchange-sequence</metric>
    <metric>undisclosed-observations</metric>
    <metric>silent-agents</metric>
    <metric>is-hidden-profile?</metric>
    <metric>n-hidden-profile-generation-attempts</metric>
    <metric>net-filename</metric>
    <metric>table:to-list all-observations</metric>
    <metric>target-node</metric>
    <metric>table:to-list common-observations</metric>
    <metric>truth</metric>
    <metric>time-to-correct-consensus</metric>
    <metric>likely-evidence</metric>
    <enumeratedValueSet variable="disable-deliberation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="always-make-max-attempts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-policy-options">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-total-observations">
      <value value="11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-agents">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-common-observations">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-statements">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likely-evidence">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="run-experiments-for-each-bn" repetitions="5000" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup-existing-bn</setup>
    <go>go</go>
    <final>r:stop</final>
    <metric>n-correct-votes</metric>
    <metric>exchange-sequence</metric>
    <metric>undisclosed-observations</metric>
    <metric>silent-agents</metric>
    <metric>is-hidden-profile?</metric>
    <metric>n-hidden-profile-generation-attempts</metric>
    <metric>net-filename</metric>
    <metric>table:to-list all-observations</metric>
    <metric>target-node</metric>
    <metric>table:to-list common-observations</metric>
    <metric>truth</metric>
    <metric>time-to-correct-consensus</metric>
    <metric>likely-evidence</metric>
    <enumeratedValueSet variable="filename">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disable-deliberation">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-policy-options">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-total-observations">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy">
      <value value="&quot;advocacy&quot;"/>
      <value value="&quot;conformity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="support-type">
      <value value="&quot;confirmation&quot;"/>
      <value value="&quot;compatibility&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-agents">
      <value value="3"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-common-observations">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-statements">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likely-evidence">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="only-generate-hidden-profiles" repetitions="1000" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup-existing-bn</setup>
    <go>go</go>
    <final>r:stop</final>
    <metric>modelstring</metric>
    <metric>n-correct-votes</metric>
    <metric>n-correct-votes-all-runs</metric>
    <metric>n-agents</metric>
    <metric>n-total-observations</metric>
    <metric>is-hidden-profile?</metric>
    <metric>n-hidden-profile-generation-attempts</metric>
    <metric>net-filename</metric>
    <metric>table:to-list all-observations</metric>
    <metric>target-node</metric>
    <metric>table:to-list common-observations</metric>
    <metric>truth</metric>
    <enumeratedValueSet variable="filename">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-n-attempts-to-generate-hp">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-policy-options">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-total-observations">
      <value value="11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-common-observations">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-agents">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disable-deliberation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="always-make-max-attempts">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likely-evidence">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-statements">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
