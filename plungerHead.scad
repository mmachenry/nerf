$fn = 100;

function fromInches (i) = i * 25.4;

module squishablePlunger (backPlateHeight, shaftHeight, shaftDiameter, drillSize) {
    difference () {
        union () {
            baseDisk(backPlateHeight);
            translate ([0,0,backPlateHeight]) shaft(shaftHeight-1, shaftDiameter);
        }

        plungerRodHole(
            fromInches(0.4),
            backPlateHeight+shaftHeight-1-fromInches(0.15));

        /* The drill hole */
        cylinder(h = backPlateHeight+shaftHeight, d = drillSize);
    }
}

module baseDisk (height) {
    cylinder(h = height, d = fromInches (1.250));
}

module shaft (height, diameter) {
    cylinder(h = height, d = diameter);
}

module plungerRodHole (width, height) {
    translate ([-1*width/2,-1*width/2, -1])
        cube ([width,width,height+1]);
}

/*
For McMaster-Carr part 9562K46 and matching instructions from
http://nerfhaven.com/forums/topic/25180-rainbow-pistol-write-up/ 
*/

squishablePlunger(
    backPlateHeight = fromInches(0.150),
    shaftHeight = fromInches(0.325),
    shaftDiameter = fromInches(1),
    drillSize = fromInches(0.1695) // For #8 close fit screw
    );