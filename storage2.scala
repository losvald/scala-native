def run(n@: Int): Int = {
  var count = 0;
  def buildTreeDepth@(depth@: Int, random@: Array[Int]) // TODO Array[Int@]
      : Array[Any] = {
    count = count + 1;
    if (depth == 1) {
      new Array[Any](rngNextInt(random) % 10 + 1)
    } else {
      def f@(i@: Int) = buildTreeDepth(depth - 1, random);
      arrayTabulate[Any](4, f)
    }
  };

  buildTreeDepth(n, makeRNG(0));
  count
};

val result@ = run(7);
if (result == 5461) { // == 4^6 + 4^5 + 4^4 + 4^3 + 4^2 + 4^1 + 4^0
  printChar('O'); printChar('K')
} else {
  printInt(result)
};
printChar('\n')
