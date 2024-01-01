$fn = 100;

tubeOD = 42.16;
tubeWall = 3.556;
tubeID = tubeOD - 2 * tubeWall;
barrelOD = 21.34;
lipH = 5;
insertH = 10;

difference () {
    union () {
        cylinder(r=tubeOD/2, h=lipH);
        translate([0,0,lipH]) cylinder(r=tubeID/2, h=insertH);
    }
    cylinder(r=barrelOD/2, h=lipH+insertH);
}