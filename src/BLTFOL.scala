trait GoalNode {
  def instatiate(map : Map[VariableParam,ConstantParam]) : GoalNode
}
trait BLTFOL extends GoalNode
trait Atom extends BLTFOL


case class GoalANDDecomposition(name:String,operands : List[GoalNode]) extends GoalNode {
  override def toString: String = name //"("+operands.mkString(" AND ")+")"

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = {
    GoalANDDecomposition(name, for (o <- operands) yield o.instatiate(map) )
  }
}

case class GoalALTDecomposition(name:String,operands : List[GoalNode]) extends GoalNode  {
  override def toString: String = name //"("+operands.mkString(" ALT ")+")"

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = {
    GoalALTDecomposition( name, for (o <- operands) yield o.instatiate(map) )
  }
}


case class ExistentialQuant(variable: VariableParam, domain: VariableDomain, formula : BLTFOL) extends BLTFOL {
  override def toString: String = "\u2203 "+variable+ " in "+domain.name+":"+formula

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = {
    if (map.contains(variable)) {
      formula.instatiate(map)
    } else {
      ExistentialQuant(variable, domain, formula.instatiate(map).asInstanceOf[BLTFOL])
    }
  }
}

case class UniversalQuant(variable : VariableParam, domain: VariableDomain, formula : BLTFOL) extends BLTFOL {
  override def toString: String = "\u2200 "+variable+ " in "+domain.name+":"+formula

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = {
    if (map.contains(variable)) {
      formula.instatiate(map)
    } else {
      ExistentialQuant(variable, domain, formula.instatiate(map).asInstanceOf[BLTFOL])
    }
  }

}

case class At(formula : BLTFOL,time:Int) extends BLTFOL {
  override def toString: String = "("+formula+")@"+time

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = {
    At(formula.instatiate(map).asInstanceOf[BLTFOL],time)
  }
}

case class NegatedFormula(operand : BLTFOL) extends BLTFOL {
  override def toString: String = "-"+operand

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = {
    NegatedFormula(operand.instatiate(map).asInstanceOf[BLTFOL])
  }
}
case class ConjunctionFormula(operands : List[BLTFOL]) extends BLTFOL {
  override def toString: String = "("+operands.mkString(" and ")+")"

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = {
    ConjunctionFormula(for (o <- operands) yield o.instatiate(map).asInstanceOf[BLTFOL] )
  }
}
case class DisjunctionFormula(operands : List[BLTFOL]) extends BLTFOL {
  override def toString: String = "("+operands.mkString(" or ")+")"

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = {
    DisjunctionFormula(for (o <- operands) yield o.instatiate(map).asInstanceOf[BLTFOL] )
  }
}


case class MetricGlobally(operand : BLTFOL,interval:MetricRange) extends BLTFOL {
  override def toString: String = "("+"G["+interval.start+","+interval.end+"]"+operand+")"

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = {
    MetricGlobally(operand.instatiate(map).asInstanceOf[BLTFOL],interval)
  }
}
case class MetricFinally(operand : BLTFOL,interval:MetricRange) extends BLTFOL {
  override def toString: String = "("+"G["+interval.start+","+interval.end+"]"+operand+")"

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = {
    MetricFinally(operand.instatiate(map).asInstanceOf[BLTFOL],interval)
  }
}
case class MetricUntil(leftoperand : BLTFOL, rightoperand : BLTFOL,interval:MetricRange) extends BLTFOL {
  override def toString: String = "("+leftoperand+" U["+interval.start+","+interval.end+"]"+rightoperand+")"

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = {
    MetricUntil(leftoperand.instatiate(map).asInstanceOf[BLTFOL],rightoperand.instatiate(map).asInstanceOf[BLTFOL],interval)
  }
}


case class TrueAtom() extends Atom {
  override def toString: String = "\u22A4"

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = TrueAtom()
}
case class FalseAtom() extends Atom {
  override def toString: String = "\u22A5"

  override def instatiate(map: Map[VariableParam, ConstantParam]): GoalNode = FalseAtom()
}

case class PredicateAtom(predicate:String, terms: List[Param] ) extends Atom {
  override def toString : String = predicate+"("+term_list_string+")"
  private def term_list_string : String = {
    var a_string: String = ""
    for (i <- terms.indices) {
      a_string += terms(i).toString
      if (i<terms.length-1)
        a_string += ","
    }
    a_string
  }

  def isGround : Boolean = {
    var ground = true
    for (t<-terms if t.isInstanceOf[VariableParam])
      ground = false
    ground
  }

  def instatiate(assignments : Map[VariableParam,ConstantParam]):PredicateAtom = {
    val ground_terms = for (t<-terms) yield replace_var(t,assignments)
    PredicateAtom(predicate,ground_terms)
  }

  private def replace_var(t: Param,assignments : Map[VariableParam,ConstantParam]):ConstantParam = {
    t match {
      case AtomParam(_) => t.asInstanceOf[AtomParam]
      case NumeralParam(_) => t.asInstanceOf[NumeralParam]
      case TrueParam() => TrueParam()
      case FalseParam() => FalseParam()

      case VariableParam(name) =>
        assignments(VariableParam(name))

      case _=> FalseParam()
    }
  }
}

sealed abstract class Param
abstract class ConstantParam extends Param {
  override def hashCode(): Int = toString.hashCode
}
case class VariableParam(name : String) extends Param {
  override def toString: String = s"var($name)"
}

case class AtomParam(atom : String) extends ConstantParam {
  override def toString: String = atom
}
case class NumeralParam(num : Int) extends ConstantParam {
  override def toString: String = num.toString
}
case class TrueParam() extends ConstantParam {
  override def toString: String = "true"
}
case class FalseParam() extends ConstantParam {
  override def toString: String = "false"
}

case class VariableDomain(name:String,set_size : Int)

case class MetricRange(start:Int,end:Int)