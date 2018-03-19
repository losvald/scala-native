def isShorterThan@(x@: List[Any@], y@: List[Any@]): Boolean = {
  if (!y.isEmpty && !x.isEmpty) isShorterThan(x.tail, y.tail)
  else !y.isEmpty && x.isEmpty
};

def withTail@[U](x@: List[Any@], y@: List[Any@], z@: List[Any@],
  f@: List@[Any@] => U
): U = {
  if (isShorterThan(y, x)) {
    withTail[U](x.tail, y, z, {def _1@(x2@: List[Any@]) = {
      withTail[U](y.tail, z, x, {def _2@(y2@: List[Any@]) = {
        withTail[U](z.tail, x, y, {def _3@(z2@: List[Any@]) = {
          withTail[U](x2, y2, z2, f)
        }; _3})
      }; _2})
    }; _1})
  } else {
    f(z)
  }
};

def withList@[U](len@: Int, f@: List@[Any@] => U): U = {
  def rec@[U](len@: Int, lst@: List[Int@], f@: List@[Any@] => U): U = {
    if (len == 0) f(lst)
    else rec[U](len - 1, len :: lst, f)
  };
  rec[U](len, Nil, f)
};

def listLength2@[T](l@: List[T@]): Int = // unlike listLength, use "T @local"
  if (l.isEmpty) 0 else 1 + listLength2[T](l.tail);

def run@(xLen@: Int, yLen@: Int, zLen@: Int): Int = {
  withList[Int](xLen, {def _1@(x@: List[Any@]) = {
    withList[Int](yLen, {def _2@(y@: List[Any@]) = {
      withList[Int](zLen, {def _3@(z@: List[Any@]) = {
        withTail[Int](x, y, z, {def _4@(result@: List[Any@]) = {
          listLength2[Any](result)
        }; _4})
      }; _3})
    }; _2})
  }; _1})
};

val result@ = run(15, 10, 6);
if (result == 10) {
  printChar('O'); printChar('K')
} else {
  printInt(result)
};
printChar('\n')
