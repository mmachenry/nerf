$fn = 100;

function fromInches (i) = i*25.4;

//1-1/4" PVC metric measurements from wikipedia nominal pipe sizes.
tubeOD = 42.16;
tubeWall = 3.556;
tubeID = tubeOD - 2*tubeWall;

//1/2" PVC metric measurements from wikipedia nominal pipe sizes.
barrelOD = 21.34;
barrelWall = 2.769;
barrelID = barrelOD - 2*barrelWall;

// #6-32 bolt clearance hole
screwOD = fromInches(0.144);

uCupHeight = fromInches(1/4);

bodyOD = tubeID-1; //review
insertH = 25; //review
airHoleH = 7; //review
lipH = 5; // review

module body () {
    union () {
        cylinder(h=lipH, r=bodyOD/2);
        cylinder(h=insertH+airHoleH, r = bodyOD/2);
    }
}

module barrelHole () {
    cylinder(h=insertH, r=barrelOD/2);
}

module airHole () {
    translate([0,0,insertH]) cylinder(h=airHoleH, r=barrelID/2);
}

module drillHole () {
    translate([0,tubeID/2,0])
    rotate([90,0,0]) cylinder(r=screwOD/2,h=tubeID);
}

module part () {
    difference () {
        body();
        barrelHole();
        airHole();
        drillHole();
    }
}

module uCupSurface () {
    cylinder(r=barrelOD/2, h=uCupHeight);
}

module uCupRetainer () {
    cylinder(r=(tubeID+barrelOD)/4, h=airHoleH);
}

module lip () {
    cylinder(r=tubeOD/2, h=lipH);
}

module insert () {
    cylinder(r=(tubeID-1)/2, h=insertH);
}

module part2 () {
    difference () {
        union () {
            translate([0,0,uCupHeight+insertH])
                difference () {
                    uCupRetainer();
                    translate ([0,0,airHoleH/2])
                        #drillHole();
                }
            translate([0,0,insertH]) uCupSurface();
            translate([0,0,0]) insert();
            translate([0,0,0]) lip();
        }
        cylinder(r=(barrelOD+1)/2, h=insertH);
        translate ([0,0,insertH]) cylinder(r=barrelID/2, h=airHoleH+100);
    }
}

part2();