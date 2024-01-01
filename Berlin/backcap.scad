use <parts.scad>

$fn = 50;

plungerTubeOD = 42.12;
plungerTubeWallThickness = 3.8;
plungerTubeID = plungerTubeOD - 2*plungerTubeWallThickness;

diameter = pressFitGap(plungerTubeID);

plungerRodWidth = fromInches(3/8);

insertedLength = fromInches(3/8);
capWidth = fromInches(1/4);

difference () {
	union () {
		// Inserted body
		cylinder (
			d = diameter,
			h = insertedLength);
		// Outer cap
		translate ([0,0,insertedLength])
			cylinder (d = plungerTubeOD, h = capWidth);
	}
	// Plunger rod hole
	translate ([plungerRodWidth/-2,plungerRodWidth/-2,0])
		cube ([
			plungerRodWidth,
			plungerRodWidth,
			insertedLength+capWidth]);
	// rivets
	translate([0,0,insertedLength/2])
	#rivetRing(diameter/2,2);
}

