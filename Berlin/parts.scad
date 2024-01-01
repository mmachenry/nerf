plungerTubeOD = 42.12;
plungerTubeWallThickness = 3.8;
plungerTubeID = plungerTubeOD - 2*plungerTubeWallThickness;

rivetLength = fromInches(0.438);
// this is fudged to be x2 so that it sits deeper inside a
// curved surface
rivetFlangeThickness = fromInches(0.032)*2;

function fromInches (i) = i * 25.4;
function pressFitGap (d) = d + 0.15;

/*
McMaster-Carr part #93482A605
Aluminum Rivet Nut, 6-32 Internal Thread, .010"-.075"
Material Thickness
*/

module rivetRing (radius, n) {
	for (i = [1:n]) {
		rotate([0,0,360/n*i])
		translate([0,rivetLength+rivetFlangeThickness-radius,0])
		rotate([90,0,0])
		rivetNut();
	}
}

module rivetNut () {
	diameter = fromInches(0.189); // #12 drill hole
	flangeDiameter = fromInches(0.325);

	union () {
		cylinder (d=pressFitGap(diameter), h=rivetLength);
		translate ([0,0,rivetLength])
			cylinder(
				d=pressFitGap(flangeDiameter),
				h = rivetFlangeThickness);
	}
}
