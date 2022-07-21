object Example1 extends App {
  val G = GoalALTDecomposition("root_goal",List(
    UniversalQuant(VariableParam("rb"),VariableDomain("4rb",4),PredicateAtom("p1",List(VariableParam("rb")))),
    MetricFinally(
      ConjunctionFormula(List(
        PredicateAtom("p2",List()),
        PredicateAtom("p3",List()) )
      ),MetricRange(1,3))
  ))

  val compls = new GoalComplements(G,List())
  println("Goals: "+compls.goals_to_be_processed)
  println("Compls: " + compls.complements)
}


object Example2 extends App {
  val G = MetricUntil(PredicateAtom("a",List()),PredicateAtom("b",List()),MetricRange(1,3))

  val compls = new GoalComplements(G,List())
  println("Goals: "+compls.goals_to_be_processed)
  println("Compls: " + compls.complements)
}
