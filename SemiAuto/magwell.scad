$fn = 100;
function fromInches (i) = 25.4*i;

/*
I plan to use a 9/16" Delrin rod as the ram, which obviously
has the same OD as 9/16" brass. This nests air tight within
19/32" brass tube which has a wall thickness of 0.014" which
is slightly less than 1/64". Two wall thicknesses thus amount
to bringing the ID down to 18/32 = 9/16. This is why 9/16"
brass nests nice and air tight within 19/32" brass.
*/

/*
Brass tube wall thickness is 0.014", which is a little less than 1/64".
Therefore the in inner diameter of 19/32 is 19/32 - 0.014*2.
That's for an air tight fit. So for the guide in the back I'll do
19/32 - 1/64 as a compromise.
*/
ramRodOD = fromInches(19/32-1/64);

chamberOD = fromInches(1.106)+0.4; // for a press fit

/* Chosen dimentions of the outer box */
partWidth = fromInches(1.5);
partHeight = fromInches(2);
backWallWidth = 15;
frontWallWidth = 15;

// clearance for #6-32 screw
boltHoleD = fromInches(0.1495);

/* Measured dimensions of the Nerf clip */
clipHeight = 114; //measured
clipLength = 80.88; //measured
clipWidth = 25.4; //measured
retainerLength = 46.18; //measured
retainerOD = 25.56; //measured
retainerWall = 3.8; //measured but sloppily
retainerID = retainerOD - 2*retainerWall;
retainerGapWidth = 10; // measured at 11mm, subtracting 1mm for close fit.
/*
Measured at 2.36. Adding a nice big gap because it doesn't
need to actually need to be a close fit. So adding 1mm instead of 0.4mm.
*/
directionalTabWidth = 2.36 + 1; //measured
directionalTabHeight = sqrt(pow(retainerOD/2,2) - pow(retainerGapWidth/2,2));


partLength = clipLength + backWallWidth + frontWallWidth;

module retainerTabs () {
    difference () {
        union () {
            cylinder(h = retainerLength, r = retainerOD/2);
            directionalTab();
        }
        cylinder(h = retainerLength, r = ramRodOD/2);
        translate([-1*retainerOD/2,0,0])
            cube([retainerOD, retainerOD/2, retainerLength]);
        translate([-1*retainerGapWidth/2,-1*retainerOD/2,0])
            cube([retainerGapWidth, retainerOD, retainerLength]);
    }
}

module clipBody () {
    translate([-1*retainerOD/2,0, -1*(clipLength-retainerLength)/2])
        cube([retainerOD, clipHeight, clipLength]);
}

module directionalTab () {
    translate ([
        -1*retainerOD/2,
        -1*directionalTabHeight,
        (retainerLength-directionalTabWidth)/2])
        cube([retainerOD/2, directionalTabHeight, directionalTabWidth]);
}

module clip () {
    translate ([0,0,(clipLength-retainerLength)/2])
        union () {
            clipBody();
            retainerTabs();
        }
}

module boltHoles () {
    standoff = boltHoleD * 1.5;
    top = clipLength + frontWallWidth/2;
    bottom = -1 * backWallWidth/2;
    left = partHeight/2 - standoff;
    right = -1*partHeight/2 + standoff;

    boltHole(left,top);
    boltHole(right,top);
    boltHole(left,bottom);
    boltHole(right,bottom);
}

module boltHole (x,y) {
    translate ([-1*clipWidth, x, y])
        rotate ([0,90,0])
	   cylinder (h = clipWidth * 2, d = boltHoleD);
}
	
module part () {
    difference () {
        translate ([-1*partWidth/2,
                    -1*partHeight/2,
                    -1*backWallWidth])
            cube([partWidth, partHeight, partLength]);

        // This is the hole that the ram will be guided by
        #translate ([0,0,-1*backWallWidth])
            cylinder(h=partLength, r = ramRodOD/2);
        // Hole for holding the chamber tee joint
        #translate ([0,0,clipLength])
            cylinder(h=frontWallWidth, r = chamberOD/2);
        #clip ();
        boltHoles();
    }
}

module testBox () {
    newBackWall = 5;
    width = partWidth*0.8;
    length = clipLength + newBackWall + newBackWall;
    newHeight = partHeight*0.82;
    translate ([-1*width/2,-1*partHeight/2,-1*newBackWall])
        cube([width, newHeight, length]);
}

//intersection () {
//    testBox();
    part();
//}
