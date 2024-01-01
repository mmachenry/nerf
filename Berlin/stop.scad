use <parts.scad>

$fn=100;

boltHoleD = fromInches(0.1440); // clearance drill hole for #6-32
boltDepth = 10;

nutWidth = fromInches(5/16);
nutHeight = fromInches(7/64) + 0.2;
nutInset = 2;

holeSize = fromInches(0.2570); // clearance drill hole for 1/4"
standOffLength = fromInches(3/8)+2;
partHeight = holeSize+2;
tubeID = fromInches(1.360);

plungerRod = fromInches(3/8);
plungerRodHoleWidth = plungerRod + 2;

module nutPocket () {
    x = nutWidth + 0.2;
    y = partHeight + 2;
    union () {
        cylinder(h = boltDepth, d = boltHoleD);
        translate ([-1/2*x, -1/2*y, nutInset])
            cube ([x, y, nutHeight]);
    }
}

module plungerRodHole () {
    translate ([plungerRodHoleWidth/-2, plungerRodHoleWidth/-2, -1])
        cube ([plungerRodHoleWidth, plungerRodHoleWidth, partHeight + 2]);
}

module stop () {
    difference () {
        cylinder (d = tubeID, h = partHeight);
        for (i = [0:4]) {
            rotate([0,0,90*i])
                translate([0,tubeID/2,partHeight/2])
                rotate([90,0,0])
                nutPocket();
        }
        plungerRodHole();
    }
}

stop();
