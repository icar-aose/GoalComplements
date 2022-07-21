class GoalComplements(rootgoal:GoalNode, satisfied:List[GoalNode]) {
  var goals_to_be_processed : List[GoalNode] = List(rootgoal)
  var complements : List[ ComplItem ] = List(ComplItem(Set(rootgoal)))
  var it = 0

  while (goals_to_be_processed.nonEmpty) {
    val target_goal = goals_to_be_processed.head

    println(s"*** iteration $it")
    println("Goals: "+goals_to_be_processed)
    println("Compls: " + complements)
    println("target: "+target_goal)
    it = it +1

    goals_to_be_processed = goals_to_be_processed.tail

    target_goal match  {
      case GoalANDDecomposition(name,operands) =>
        println("Action: AND decompo. - replacing node with "+operands)
        append_to_goalslist(operands)
        remove_g_from_complements(target_goal)
        append_set_into_complements(operands.toSet)

      case GoalALTDecomposition(name,operands) =>
        println("Action: ALT decompo. - replacing node with 1 set for each element in "+operands)
        remove_g_from_complements(target_goal)
        for (g <- operands) {
          append_to_goalslist(List(g))
          append_set_into_complements(Set(g))
        }

      case ConjunctionFormula(operands) =>
        println("Action: Conjunction - replacing node with "+operands)
        append_to_goalslist(operands)
        remove_g_from_complements(target_goal)
        append_set_into_complements(operands.toSet)

      case At(ConjunctionFormula(operands),time) =>
        println("Action: Timed Conjunction - replacing node with conjunction of timed "+operands)
        val timed_operands = for (o <- operands) yield At(o,time)
        one_to_one_replace(target_goal,ConjunctionFormula(timed_operands))

      case DisjunctionFormula(operands) =>
        println("Action: Disjunction - replacing node with 1 set for each element in "+operands)
        remove_g_from_complements(target_goal)
        for (g <- operands) {
          append_to_goalslist(List(g))
          append_set_into_complements(Set(g))
        }

      case At(DisjunctionFormula(operands),time) =>
        println("Action: Timed Disjunction - replacing node with disjunction of timed "+operands)
        val timed_operands = for (o <- operands) yield At(o,time)
        one_to_one_replace(target_goal,DisjunctionFormula(timed_operands))

      case NegatedFormula(operand) =>

      case UniversalQuant(variable, domain, formula) =>
        println("Action: Universal Quantifier - replacing node by unrolling all the occurences of variable ")
        remove_g_from_complements(target_goal)
        var ex_form_list : List[GoalNode] = List.empty
        for (index <- 1 to domain.set_size) {
          val exform = formula.instatiate(Map(variable->NumeralParam(index)))
          ex_form_list = exform :: ex_form_list
        }
        append_to_goalslist(ex_form_list)
        append_set_into_complements(ex_form_list.toSet)

      case ExistentialQuant(variable, domain, formula) =>
        println("Action: Universal Quantifier - replacing node by a set for each occurence of variable ")
        remove_g_from_complements(target_goal)
        for (index <- 1 to domain.set_size) {
          val exform = formula.instatiate(Map(variable->NumeralParam(index)))
          append_to_goalslist(List(exform))
          append_set_into_complements(Set(exform))
        }

      case MetricFinally(operand,interval) =>
        println("Action: Finally - replacing node with a timed version of the "+operand+" for the whole interval")
        remove_g_from_complements(target_goal)
        var temp_form_list : List[BLTFOL] = List.empty
        for (time <- interval.start to interval.end) {
          val timedform = At(operand,time)
          temp_form_list = timedform :: temp_form_list
        }
        append_to_goalslist(List(DisjunctionFormula(temp_form_list)))
        append_set_into_complements( Set(DisjunctionFormula(temp_form_list)) )


      case MetricGlobally(operand,interval) =>
        println("Action: Globally - replacing node with a set for each timed version of the "+operand+" for the whole interval")
        remove_g_from_complements(target_goal)
        for (time <- interval.start to interval.end) {
          val timedform = At(operand,time)
          append_to_goalslist(List(timedform))
          append_set_into_complements(Set(timedform))
        }

      case MetricUntil(leftoperand, rightoperand, interval) =>
        println("Action: Until - replacing node with a set for each alternative combination of "+leftoperand+" and "+rightoperand+" for the whole interval")
        remove_g_from_complements(target_goal)
        for (r_time <- interval.start to interval.end) {
          var conj : List[BLTFOL] = List.empty
          conj = At(rightoperand,r_time) :: conj
          for (l_time <- interval.start to r_time-1) {
            conj = At(leftoperand,l_time) :: conj
          }
          append_to_goalslist(conj)
          append_set_into_complements(conj.toSet)
        }

      case At(formula, time) =>
        println("doing nothing")

      case _ =>
        println("doing nothing")

    }

  }

  def append_to_goalslist(operands: List[GoalNode]) : Unit = {
    val filtered_operands = operands.filter(!satisfied.contains(_))
    goals_to_be_processed = goals_to_be_processed ::: filtered_operands
  }

  def remove_g_from_complements(target_goal: GoalNode) : Unit = {
    complements = complements.filter(_ != ComplItem(Set(target_goal)) )
  }

  def append_set_into_complements(value: Set[GoalNode]): Unit = {
    val filtered_value = value.filter( !satisfied.contains(_))
    complements = ComplItem(filtered_value) :: complements
  }

  def one_to_one_replace(current: GoalNode, newnode: GoalNode) : Unit = {
    append_to_goalslist(List(newnode))
    remove_g_from_complements(current)
    append_set_into_complements(Set(newnode))
  }


}

case class ComplItem( item: Set[GoalNode]) {
  override def toString: String = "{"+item.mkString(",")+"}"
}
