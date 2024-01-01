$fn = 100;

function fromInches (i) = i*25.4;

tubeOD = 42.16;
tubeWall = 3.556;
tubeID = tubeOD - 2*tubeWall;

barrelOD = 21.34;
barrelWall = 2.769;
barrelID = barrelOD - 2*barrelWall;

oRingOD = fromInches(1+1/6);
oRingWall = fromInches(3/32);
oRingID = fromInches(7/8);

screwOD = fromInches(0.144);

tubeCapOD = barrelOD+3;
rubberH = 3;
insertH = 25;
airHoleH = 7;
lipH = 5;

module body () {
    union () {
        cylinder(h=lipH, r=tubeOD/2);
        cylinder(h=insertH+airHoleH, r = tubeCapOD/2);
    }
}

module barrelHole () {
    cylinder(h=insertH, r=barrelOD/2);
}

module airHole () {
    translate([0,0,insertH]) cylinder(h=airHoleH, r=barrelID/2);
}

module drillHole (y) {
    translate([0,tubeID/2,y])
        rotate([90,0,0]) cylinder(r=screwOD/2,h=tubeID);
}

module oRingGroove () {
    difference () {
        cylinder(h=oRingWall, r=tubeID/2);
        cylinder(h=oRingWall, r=tubeID/2-oRingWall/2);
    }
}

module tubeCap () {
    difference () {
        body();
        barrelHole();
        airHole();
        drillHole(insertH+ airHoleH/2);
        //translate([0,0,lipH+(insertH-lipH)*3/4]) oRingGroove();
        //translate([0,0,lipH+(insertH-lipH)*1/4]) oRingGroove();
    }
}

module rubber () {
    difference () {
        cylinder (h=airHoleH+rubberH, r=tubeID/2);
        cylinder (h=airHoleH, r=tubeCapOD/2);
        translate ([0,0,airHoleH])
            cylinder(h=rubberH,r=barrelID/2);
        drillHole(airHoleH/2);
    }
}

union () {
    tubeCap();
    translate ([0,0,lipH+30]) rubber();
}