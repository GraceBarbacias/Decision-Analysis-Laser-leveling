example_decision_function <- function(x, varnames){
  
  #### Grundidee
  # Man überlegt sich eine Menge von Annahmen über Variablen, die man nicht kennt. (man rät dessen Wahrscheinlichkeitsverteilungen)
  # Wird diese funktion dann aufgerufen, wird an Hand von den Wahrscheinlichkeitsverteilungen eine Belegung aller Variablen zufällig ausgewählt.
  # Mit diesen Variablenbelegungen wird weiter unten dann der Netto-Ertrag erstens dafür berechnet, dass Interventions-strips angelegt werden und zweitens dafür dass keine Interventions-strips angelegt werden. (Einfach eine Fallunterscheidung)
  # Statt dies nur für ein Jahr zu machen, wird dies für eine vorher festgelegte Anzahl an Jahren gemacht. Und dieser Vorgang (als wäre es nicht schon genug ;) wird dann bspws. 10.000x wiederholt. Also 10.000 Zeitreihen mit unterschiedlichen Ergebnissen jeweils für die 2 Fälle.
 
  # Die Wahrscheinlichkeitsverteilungen der Variablen werden nicht hier sondern in einer .csv definiert.  Diese Funktion bekommt dann eine Reihe von Variablen übergeben, die dort vorher angelegt und benannt wurden.
  # Wenn gewisse Werte fix sind, kann einfach eine konstante Verteilung, die genau einen Wert darstellt, angegeben werden.
  # Bei der einmaligen Ausführung der Funktion wird nach entsprechender Wahrscheinlichkeitsverteilung ein konkreter Wert für jede Variable ausgewählt
  # Die Variablen werden in der csv benannt und dessen Benennung muss mit der in dieser Datei übereinstimmen. Varialen die aus der csv stammen sind bswp.: intervention_NonPopInvolv, TLU_no_intervention, var_CV, n_years,  profit_per_TLU, intervention_fruit_area_ha, intervention_fruit_yield_t_ha, usw. usw.

  # achja, die csv kannst du dir hier ansehen:  https://github.com/CWWhitney/BurkinaExample/blob/master/Sediment_input_table.csv


  # calculate ex-ante risks: impact the implementation of interventions ####
  # Im-Voraus-Risiko: Wahrscheinlichkeit dafür, dass die Intervention nicht umgesetzt wird, sich jedoch dafür entschieden wird
  # Das Risiko besteht dann in etwaigen Planungskosten, die unnötigerweise entstehen

  intervention_NonPopInvolvEvent <-
    chance_event(intervention_NonPopInvolv, 1, 0, n = 1)
  
  # chance event wählt mit Wahrscheinlichkeit "intervention_NonPopInvolv" die erste Option aus und entprechend mit Wahrsch. (1 - intervention_NonPopInvolv) die zweite Option. 
  # (Die erste Option ist in diesem Fall 1 und die zweite Option 0 - und dies ist der Wert der am Ende dann in der Variable intervention_NonPopInvolvEvent gespeichert wird)

  # pre-calculation of common random draws for all intervention model runs ####
  
  # profits from Tropical Livestock Units (TLU)
  # hier werden einfach die gewinne durch vieh berechnet

  TLU <- vv(TLU_no_intervention, var_CV, n_years)
  TLU_profit <- vv(profit_per_TLU, var_CV, n_years)
  
  ## vv(mittelwert, varianz, n) erstellt ein array mit n einträgen, jeder eintrag wird dabei zufällig von einer standardnormalverteilung ausgewählt, dessen mittelwert und varianz vorher angegeben wird. 
  #  bspw. bei körpergrößen würde man einen mittelwert von 1.75 m? haben und eine varianz von 0.1 m? und so würden dann fast alle einträge des arrays einen wert zwischen 1.55 und 1.95 aufweisen
  # wichtig ist noch, dass die varianz und der mittelwert in diesem Fall sogar selbst über wahrscheinlichkeitsverteilungen angegeben ist und somit auch erst ausgewählt wird
  
  # hier werden einfach die gewinne durch anbau von obst berechnet
  # benefits of fruit
  precalc_intervention_fruit_benefits <-
    vv(intervention_fruit_area_ha, var_CV, n_years) *
    vv(intervention_fruit_yield_t_ha, var_CV, n_years) *
    vv(intervention_fruit_profit_USD_t, var_CV, n_years)
  
  # hier werden einfach die gewinne durch anbau von gemüse berechnet
  # benefits of vegetables
  precalc_intervention_vegetable_benefits <-
    vv(intervention_vegetable_area_ha, var_CV, n_years) *
    vv(intervention_vegetable_yield_t_ha, var_CV, n_years) *
    vv(intervention_vegetable_profit_USD_t, var_CV, n_years)
  
  # hier werden einfach die gewinne durch anbau von "Regenfestes Zeug?!" berechnet
  # benefits of rainfed crops
  precalc_intervention_rainfed_crop_benefits <-
    vv(intervention_rainfed_crop_area_ha, var_CV, n_years) *
    vv(intervention_rainfed_crop_yield_t_ha, var_CV, n_years) *
    vv(intervention_rainfed_crop_profit_USD_t, var_CV, n_years)
  
  #  Intervention ####
  
  # die simulation wird 2x ausgeführt:
  # 1. es wird sich dafür entschieden, intervention strips anzulegen
  # 2. es wird sich dagegen entschieden, intervention strips anzulegen

  for (decision_intervention_strips in c(FALSE,TRUE))
      {

  if (decision_intervention_strips)
  {
    # falls sich dafür entschieden wird
    intervention_strips <- TRUE # gibt an, dass die streifen angelegt wurden
    intervention_strips_PlanningCost <- TRUE
    intervention_strips_cost <- TRUE
  } else
  {
    # falls sich dagegen entschieden wird
    intervention_strips <- FALSE 
    intervention_strips_PlanningCost <- FALSE
    intervention_strips_cost <- FALSE
  }
  
  if (intervention_NonPopInvolvEvent) {
    # nur relevant falls sich dafür entschieden wurde
    # wenn diese bedingung erfüllt ist, werden die interventions strips zwar geplant, aber nicht umgesetzt
    intervention_strips <- FALSE # somit werden diese doch nicht angelegt
    intervention_strips_cost <- FALSE # und keine kosten entstehen für das anlegen, beachte jedoch dass die intervention_strips_PlanningCost immer noch vorhanden sind
  }
  
  # Costs ####
  if (intervention_strips_cost) {
    # falls diese angelegt werden, fallen eine reihe von kosten an
    cost_intervention_strips <-
      intervention_adaptation_cost + intervention_tech_devices_cost + intervention_nursery_cost +
      intervention_wells_cost +
      intervention_training_cost + intervention_mngmt_oprt_cost + intervention_mngmt_follow_cost +
      intervention_mngmt_audit_cost
  } else
    cost_intervention_strips <- 0
  
  if (intervention_strips_PlanningCost) {
      # planungskosten die anfallen
    plan_cost_intervention_strips <-
      intervention_communication_cost + intervention_zoning_cost
  } else
    plan_cost_intervention_strips <- 0
  
  # array mit n_years einträgen, jeder eintrag steht für die unterhaltungskosten der intervention strips für ein jahr; wird auf 0 initialisert (keine kosten)
  maintenance_cost <- rep(0, n_years)
  
  if (intervention_strips)
    # falls die intervention strips angelegt werden, entstehen jedoch unterhaltungskosten
    # vv (siehe oben)
    maintenance_cost <-
    maintenance_cost + vv(maintenance_intervention_strips, var_CV, n_years)
  
  # kosten der intervention pro jahr
  # zunächst werden alle unterhaltungskosten übernommen
  intervention_cost <- maintenance_cost
  # im ersten jahr kommen noch die kosten zum anlegen sowie planungskosten hinzu
  intervention_cost[1] <-
    intervention_cost[1] + cost_intervention_strips + plan_cost_intervention_strips

  
  # Benefits from  cultivation in the intervention strips ####
  
  # nun werden einnahmen durch durch die interventions strips berechnet
  # intervention_strips ist 0, falls diese nicht angelegt werden, wodurch sich der gewinn wegmultipliziert ;)

  intervention_fruit_benefits <-
    as.numeric(intervention_strips) * precalc_intervention_fruit_benefits
  intervention_vegetable_benefits <-
    as.numeric(intervention_strips) * precalc_intervention_vegetable_benefits
  intervention_rainfed_crop_benefits <-
    as.numeric(intervention_strips) * precalc_intervention_rainfed_crop_benefits
  
  # Total benefits from crop production (agricultural development and riparian zone) ####
  # alle einnahmen werden zusammen gerechnet (das ist eine addition von 3 arrays, wo die einzelnen einträge entsprechend miteinander addiert werden)
  crop_production <-
    intervention_fruit_benefits +
    intervention_vegetable_benefits +
    intervention_rainfed_crop_benefits
  
  # Benefits from livestock ####
  # The following allows considering that intervention strips may
  # restrict access to the reservoir for livestock.
  
  # durch das anlegen der strips bekommt vieh weniger essen?!
  # TLU_intervention gibt anzahl an vieh an (entsorechend weniger falls strips da sind, sonst wirds einfach übernommen)

  if (intervention_strips)
    TLU_intervention <-
    TLU * (1 + change_TLU_intervention_perc / 100)
  else
    TLU_intervention <- TLU
  
  # berechne nun final netto-ertrag (net_benefits)
  # d.h. net_benefits = einnahmen - ausgaben

  if (decision_intervention_strips){
    # falls die entscheidung für die strips ausgefallen ist
    livestock_benefits <- TLU_intervention * TLU_profit # anzahl an vieh * profitProVieh

    # rechne den profit für das vieh auf den profit von crops der strips (crop_production ist array mit n_years einträgen, jeder eintrag wird + livestock_benefits gerechnet) 
    total_benefits <- crop_production + livestock_benefits
    # netto-ertrag sind die gesamteinnahmen - gesamtausgaben für interventionen
    net_benefits <- total_benefits - intervention_cost

    # sprechere in result_interv (result intervention)
    result_interv <- net_benefits}
  
  
  if (!decision_intervention_strips){
    # nur gewinn aus vieh, dafür jedoch auch keine kosten für interventions (sollte 0 sein)
    livestock_benefits <- TLU_no_intervention * TLU_profit
    total_benefits <- livestock_benefits
    net_benefits <- total_benefits - intervention_cost
    
    
    # sprechere in result_interv (result NO intervention)
    result_n_interv <- net_benefits}
  
    } #close intervention loop bracket

NPV_interv <-
  discount(result_interv, discount_rate, calculate_NPV = TRUE)
#diskontierte (schmälere den ertrag/verlust mit zunehmender zeit um discount_rate %) (ich rate nur: der ertrag ist weniger wert, wenn er in der zukunft liegt))
# wahrscheinlich sieht das ergebnis so aus: NPV_interv = [result_interv[1] * (1-discount_rate)^0, result_interv[2] * (1-discount_rate)^1, result_interv[3] * (1-discount_rate)^2, ..., result_interv[n_years] * (1-discount_rate)^(n_years-1)]

NPV_n_interv <-
  discount(result_n_interv, discount_rate, calculate_NPV = TRUE)
# das gleiche

# Beware, if you do not name your outputs (left-hand side of the equal sign) in the return section, 
# the variables will be called output_1, _2, etc.


# hier werden jetzt zwei Zeitreihen zurückgegeben (als array!)
# Interv_NPV: gibt den netto-ertrag pro Jahr an, falls eine intervention stattgefunden hat
# Interv_NPV: gibt den netto-ertrag pro Jahr an, falls KEINE! intervention stattgefunden hat
# die anderen zeitreihen stellen eine art handlungsweise dar bzw. geben an, ob sich die entscheidung für die intervention strips in dem jew. jahr gelohnt haben oder nicht (positiv = gelohnt) (im ersten fall wird es diskontiert und im zweifen fall normal betrachtet)

return(list(Interv_NPV = NPV_interv,
            NO_Interv_NPV = NPV_n_interv,
            NPV_decision_do = NPV_interv - NPV_n_interv,
            Cashflow_decision_do = result_interv - result_n_interv))
}