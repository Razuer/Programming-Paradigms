def modifiedPascalI(n: Int): Array[Int] = {
  if n<0 then return Array[Int]()

  val oddTab = new Array[Int](n + 1)
  val evenTab = new Array[Int](n + 1)
  oddTab(0) = 1
  evenTab(0) = 1

  var i = 1
  while i <= n do {
    if i % 2 == 0 then {
      var j = 1
      while j <= i do {
        evenTab(j) = oddTab(j - 1) + oddTab(j)
        j += 1
      }
    }
    else {
      var j = 1
      while j <= i do {
        oddTab(j) = evenTab(j - 1) - evenTab(j)
        j += 1
      }
    }
    i += 1
  }
  if n % 2 == 0 then evenTab
  else oddTab
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