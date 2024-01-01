function fromInches(i) = i * 25.4;

clipWidth = fromInches(1);
clipLength = fromInches(3+3/16);
partHeight = fromInches(1);

difference () {
  cube([clipWidth+10, clipLength+10, partHeight+10]);
  translate ([5,5,-1])
      cube([clipWidth, clipLength, partHeight]);
}