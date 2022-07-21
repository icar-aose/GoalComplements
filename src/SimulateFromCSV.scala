object ReadFile extends App {
  val c = new SimulateFromCSV("res/example.csv",";")
}



class SimulateFromCSV(filename:String,sep:String=",") {
  val bufferedSource = io.Source.fromFile(filename)
  var someheader : Option[Array[String]] = None
  var timeline : List[Map[String,Boolean]] = List.empty

  for (line <- bufferedSource.getLines) {

    val cols = line.split(sep).map(_.trim)
    if (someheader.isEmpty)
      someheader = Some(cols)
    else {

      var time_map : Map[String,Boolean] = Map.empty
      for (i <- 0 to cols.size-1) {
        val h = someheader.get(i)
        val cell = cols(i).toBoolean

        if (cell) {
          val map = h -> cell
          time_map += map
        }
      }

      timeline = time_map :: timeline
    }

  }

  timeline = timeline.reverse

  println("End: ")
  var t=0
  for (item <- timeline) {
    println(t+": "+item)
    t = t+1
  }
}
