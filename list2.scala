def isShorterThan(x@: List[Any], y@: List[Any]): Boolean = {
  var xTail = x.head :: x.tail; // FIXME need 2nd-class variable
  var yTail = y.head :: y.tail; // FIXME need 2nd-class variable

  while (!yTail.isEmpty && !xTail.isEmpty) {
    xTail = xTail.tail;
    yTail = yTail.tail
  };

  !yTail.isEmpty && xTail.isEmpty
};

def tail(x@: List[Any], y@: List[Any], z@: List[Any]): List[Any] = {
  if (isShorterThan(y, x)) {
    tail(tail(x.tail, y, z), tail(y.tail, z, x), tail(z.tail, x, y))
  } else {
    z.head :: z.tail
  }
};

def makeList(len@: Int): List[Any] = listTabulate[Any](len, (i: Int) => i);

def run(xLen@: Int, yLen@: Int, zLen@: Int): Int = {
  val result@ = tail(makeList(xLen), makeList(yLen), makeList(zLen));
  listLength[Any](result)
};

val result@ = run(15, 10, 6);
if (result == 10) {
  printChar('O'); printChar('K')
} else {
  printInt(result)
};
printChar('\n')
