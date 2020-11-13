def max(l: List[Int]): Int = {
  def aux(l: List[Int], acc: Int):Int = {
    if(l.isEmpty) acc
    else if(l.head > acc) aux(l.tail,l.head)
    else aux(l.tail,acc)
  }
  aux(l,l.head)
}

max(List(1, 2, 3))

List(1, 2, 3).reduceLeft((x,y) => if( x > y) x else y)

def reduceRight2(ini:Int)(l:List[Int],f: (Int,Int) => Int):Int = {
  if (l.isEmpty) ini
  else f(l.head,reduceRight2(ini)(l.tail,f))
}

def reduceLeft2(ini:Int)(l:List[Int],f: (Int,Int) => Int):Int = {
  if (l.isEmpty) ini
  else reduceLeft2(f(ini,l.head))(l.tail,f)
}

reduceRight2(1)(List(1, 2, 3),(x,y) => if( x > y) x else y)
reduceLeft2(1)(List(1, 2, 3),(x,y) => if( x > y) x else y)