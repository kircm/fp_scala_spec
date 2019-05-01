

val m = Seq("A2", "B1", "C3", "D1", "E4", "F1")

val n = Seq("1", "2", "3", "4")


val a: Seq[(String, String)] = m.flatMap(m => n.filter(n => m.contains(n)).map(n => (m, n)))

a.groupBy(g => g._1)








