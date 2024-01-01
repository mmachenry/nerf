/* TODO
Counter sink bolt heads.
*/

/* Parts:
98002A207 Zinc-Plated Steel Binding Post for 1-1/4" to 1-3/8" Thick Material,
    1/4" Diameter Barrel. $4.54 for pkg of 10.
*/

use <parts.scad>

$fn=100;

// Binding Post
// free-fit clearance drill hole for 1/4"
bindingPostDrillHole = fromInches(0.2660);
// I measured the head with calipers and got 7/16". I have add 0.4mm to the
// diameter to get an 0.2mm free fit around the nut since it didn't fit when
// printed at 7/16" exactly.
bindingPostHeadD = fromInches(7/16) + 0.4 * 2;
bindingPostHeadH = fromInches(1/8);

// Clearance for I think a #6 screw if memory serves.
drillHoleD = fromInches(0.1440);

// 1-1/4" PVC pipe OD
plungerTubeOD = fromInches (1.660);
drillHoleInset = drillHoleD;
distanceBetweenDrillHoles = fromInches(0.5);
l = distanceBetweenDrillHoles + drillHoleInset*2;
h = fromInches(3/4);

// Thickness of the two plates on either side that
// will make up the grip.
// TODO add slide fit clearance to this.
sidePanelWidth = fromInches(1/4) + 0.15; // press fit gap, maybe use 0.15 on both sides?

// The amount the side panels are inserted up into
// this connector piece.
sidePanelHeight = fromInches(3/4);

// The distance between the two side panels. A hex nut usually
// has a 5/16" hex length. That needs to go between the two panels
// to hold the plunger tube on. So this needs to be a little
// bigger than 5/16"
midVerticalWidth = fromInches(3/8);

// The distance the piece will go above the side panel
// insert to provide structure. If the side panels went
// all the way up to the tangent with the pipe, there
// would be virtually nothing connecting the side walls
// to the stand off.
sidePanelRecessHeight = 3;//mm

// The amount of space that is places outside the
// side panels.
sideWallWidth = fromInches (1/4) + 2.23;

// The amount of space to allow the bolt to come down from
// the plunger tube and out the bottom. If there is no recess
// then the bolt (which is a bit sharp and not comfortable)
// will stick down out of the handle.
boltRecessHeight = fromInches (3/16);

w =  2*sideWallWidth + 2*sidePanelWidth + midVerticalWidth;



module plungerTube () {
	translate ([-1/2*l,w/2,plungerTubeOD/2+h+sidePanelRecessHeight])
	rotate ([0,90,0])
	cylinder (r = plungerTubeOD/2, h=l*2);
}

// Note that this uses the drill and tap size for the shaft so it
// should be able to pass through but I am using the exact bolt
// size here for the head so the head might not fit in the hole
// I've made.
// Copied from
// https://www.boltdepot.com/Product-Details.aspx?product=3588
module countersunkBolt () {
	length = fromInches(5/8);
	countersunkHeight = fromInches(0.083);
	headDiameterMin = fromInches(0.238);
	headDiameterMax = fromInches(0.262);
	shaftClearence = fromInches(0.1440);
	union () {
		cylinder( 
			h = countersunkHeight,
			d1 = headDiameterMax,
			d2 = headDiameterMin);
		cylinder(d = shaftClearence, h = fromInches(5/8));
	}
}

module basePiece () {
	difference () {
		// arbitrarity double the height to get it to
		// hug the pipe it's under.
		cube ([l, w, h*2]);
	
		// two drill holes down for plunger tube
		translate ([drillHoleInset,w/2,0])
		cylinder (h = h*2, d = drillHoleD);
	
		translate ([l-drillHoleInset,w/2,0])
		cylinder (h = h*2, d = drillHoleD);

		// remove the plunger tube for curve
		plungerTube();

		// remove the two side panel inserts.
	     translate ([0, sideWallWidth, 0])
		cube ([l, sidePanelWidth, sidePanelHeight]);
	  	
		translate ([0, w-sideWallWidth-sidePanelWidth, 0])
		cube ([l, sidePanelWidth, sidePanelHeight]);

		// remove the area under the grip for the bolt recess
		// so that there's an area for the bolt to go which
		// doesn't stick out from the bottom of the handle.
	    translate ([0, sideWallWidth+sidePanelWidth, 0])
		cube ([l, midVerticalWidth, boltRecessHeight]);

		// Hole for the binding posts. McMaster-Carr part #98002A207
		translate ([l/2,0,h/2])
		rotate([-90,0,0])
		cylinder (d = bindingPostDrillHole, h = w);

		// Counter sink for the binding post bolt heads
		translate ([l/2,0,h/2])
		rotate([-90,0,0])
         cylinder (d = bindingPostHeadD, h = bindingPostHeadH);

		translate ([l/2,w-bindingPostHeadH,h/2])
		rotate([-90,0,0])
         cylinder (d = bindingPostHeadD, h = bindingPostHeadH);
	}
}

module roundingBox () {
	union () {
		translate ([0,0,sideWallWidth])
		cube ([l, w, h*2]);

		translate ([0,sideWallWidth,0])
		cube ([l, w - sideWallWidth*2, h*2]);

		translate ([0,sideWallWidth,sideWallWidth])
		rotate ([0,90,0])
		cylinder(r=sideWallWidth, h=l);

		translate ([0,w-sideWallWidth,sideWallWidth])
		rotate ([0,90,0])
		cylinder(r=sideWallWidth, h=l);
	}
}
		
module rounded () {
	intersection () {
		roundingBox();
		basePiece();
	}
}

rounded();