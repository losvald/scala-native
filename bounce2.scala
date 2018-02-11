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

def abs(n: Int) = if (n < 0) -n else n;

def mkBounce@(random@: Array[Int]): () => Boolean = {
  var x: Int    = rngNextInt(random)  % 500;
  var y: Int    = rngNextInt(random)  % 500;
  var xVel: Int = (rngNextInt(random) % 300) - 150;
  var yVel: Int = (rngNextInt(random) % 300) - 150;

  def bounce(): Boolean = {
    val xLimit@: Int = 500;
    val yLimit@: Int = 500;
    var bounced = false;

    x = x + xVel;
    y = y + yVel;
    if (x > xLimit) {
      x = xLimit+0; xVel = 0-abs(xVel); bounced = true
    };
    if (x < 0) { x = 0; xVel = abs(xVel); bounced = true };
    if (y > yLimit) {
      y = yLimit+0; yVel = 0-abs(yVel); bounced = true
    };
    if (y < 0) { y = 0; yVel = abs(yVel); bounced = true };

    bounced
  };
  bounce
};

def run@(ballCount@: Int, seed@: Int) = {
  val random@ = makeRNG(seed);
  def mkBall@(i: Int) = mkBounce(random);
  val balls@: Array[() => Boolean] = arrayTabulate[() => Boolean](ballCount,
    mkBall);

  var bounces = 0;
  def countBounce@(bounce@: () => Boolean): Unit = {
    if (bounce())
      bounces = bounces + 1
  };

  var i = ballCount / 2;
  while (i > 0) {
    i = i - 1;
    arrayForeach[() => Boolean](
      // (bounce: () => Boolean) => if (bounce()) bounces = bounces + 1,
      countBounce,
      balls)
  };

  bounces
};

val result@ = run(100, 125);
if (result == 1331) {
  printChar('O'); printChar('K')
} else {
  printInt(result)
};
printChar('\n')
