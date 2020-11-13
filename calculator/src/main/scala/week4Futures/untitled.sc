val lis = List(1,2,3,4,5)

lis.foldLeft(0)((l,acc) => acc - l)

lis.foldRight(0)((l,acc) => l - acc)