// This code is based on the SOM class library.
//
// Copyright (c) 2001-2016 see AUTHORS.md file
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the 'Software'), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

def run(n@: Int): Int = {
  var movesDone@ = 0;
  val piles@: Array[Int] = arrayTabulate[Int](3, (i: Int) => -1);
  val next@: Array[Int] = arrayTabulate[Int](n + 1, (i: Int) => -1);

  def pushDisk@(disk: Int, pile@: Int): Unit = {
    val top = piles(pile);
    if (top != -1 && disk >= top)
      printString("ERROR: Cannot put a big disk on a smaller one\n");

    next(disk) = top; // next can be Array[Int@] only if piles is
    piles(pile) = disk // disk can then be 2nd-class, too
  };

  def popDiskFrom@(pile@: Int): Int = {
    val top = piles(pile); // piles can't be Array[Int@] because top is returned
                           // possible workaround: 0 + piles(pile)
    if (top == -1)
      printString("ERROR: Attempting to remove a disk from an empty pile\n");

    piles(pile) = next(top);
    next(top) = -1;
    top // workaround to enable top to be 2nd-class: 0 + top
  };    // alternative single workaround: inline this function

  def moveTopDisk@(fromPile@: Int, toPile@: Int): Unit = {
    pushDisk(popDiskFrom(fromPile), toPile);
    movesDone = movesDone + 1
  };

  def buildTowerAt@(pile@: Int, disks: Int): Unit = {
    var i@ = disks; // i (and disks) may be 2nd-class only if piles is
    while (i >= 0) {
      pushDisk(i, pile);
      i = i - 1
    };
    ()
  };

  def moveDisks@(disks@: Int, fromPile@: Int, toPile@: Int): Unit = {
    if (disks == 1) {
      moveTopDisk(fromPile, toPile)
    } else {
      val otherPile@ = (3 - fromPile) - toPile;
      moveDisks(disks - 1, fromPile, otherPile);
      moveTopDisk(fromPile, toPile);
      moveDisks(disks - 1, otherPile, toPile)
    }
  };

  buildTowerAt(0, n + 0); // workaround to accept n as 1st-class Int
  moveDisks(n, 0, 1);
  movesDone
};

val result = run(13);
if (result == 8191) {
  printChar('O'); printChar('K')
} else {
  printInt(result)
};
printChar('\n')
