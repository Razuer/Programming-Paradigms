def modifiedPascalI(n: Int): Array[Int] = {
  if n<0 then return Array[Int]()

  val tab = new Array[Int](n + 1)
  val tabCopy = new Array[Int](n + 1)
  tab(0) = 1

  var i = 1
  while i <= n do {
    tab.copyToArray(tabCopy)
    if i % 2 == 0 then {
      var j = 1
      while j <= i do {
        tab(j) = tabCopy(j - 1) + tabCopy(j)
        j += 1
      }
    }
    else {
      var j = 1
      while j <= i do {
        tab(j) = tabCopy(j - 1) - tabCopy(j)
        j += 1
      }
    }
    i += 1
  }
  tab
}

var test = modifiedPascalI(-5)
test = modifiedPascalI(0)
test = modifiedPascalI(1)
test = modifiedPascalI(2)
test = modifiedPascalI(3)
test = modifiedPascalI(4)
test = modifiedPascalI(5)
test = modifiedPascalI(6)
test = modifiedPascalI(15)