import week4_1.{BankAccount, Consolidator}


val b1 = new BankAccount
val b2 = new BankAccount

val con = new Consolidator(List(b1,b2))

//con.totalBalance
b1.meter(3)
b2.meter(22)
b2.sacar(10)