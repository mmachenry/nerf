/*
For use with 1-1/4" PVC plunger tube and 1" skirt seal.

Stretch-Fit Rotary-Shaft Ring Seal,
1" Shaft Diameter, 0.95" to 1.07" Shaft Diameter
McMaster-Carr part #9562K46

Use a fender washer for the face plate.
*/

function fromInches (i) = i * 25.4;
$fn = 100;

// Printed as 0.874 on the website but making it a nice 7/8"
uCupID = fromInches(0.875);
// Clearance close fit for #6-32 bolt
drillHoleID = fromInches(0.14400);
//Base disk OD is half way between the OD and ID of the skirt
baseDiskOD = fromInches(1.250);
shaftH = fromInches(0.325);
// Enough wall to hold the skirt seal on.
baseH = fromInches(0.150);
// Enough wall to hold the plunger head on.
drillWallH = fromInches(0.150);
totalH = shaftH + baseH;

module baseDisk () {
    cylinder(h = baseH, r = baseDiskOD / 2);
}

module shaft (h) {
    cylinder(h = h, r = uCupID / 2);
}

module drillHole (h) {
    cylinder(h = h, r = drillHoleID / 2);
}

module plungerRodHole (width, height) {
    translate ([-1*width/2,-1*width/2, 0])
        cube ([width,width,height]);
}

module plungerHead () {
    difference () {
        union () {
            baseDisk();
            translate ([0,0,baseH]) shaft(shaftH);
        }
		plungerRodHole(
            fromInches(0.4),
			totalH - drillWallH);
        drillHole(totalH);
    }
}

plungerHead();
