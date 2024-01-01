use <parts.scad>

sideBarX = fromInches(4+7/8);
sideBarY = fromInches(3/4);

bindingPostOD = fromInches(0.2660); // clearance drill hole for 1/4"

module sideBar () {
	difference () {
		square ([sideBarX, sideBarY]);
		for (offset = [
				fromInches(5/8),
				fromInches(1),
				sideBarX/2,
				sideBarX-fromInches(1),
				sideBarX-fromInches(5/8)
		]) {
			translate ([offset,sideBarY/2,0])
				circle (d=bindingPostOD);
		}
	}
}

module geometricHandle () {
	difference () {
		union() {
			sideBar();
			// In order to avoid an artifact the grip must be
			// pushed in at least as far as the rounding edge
			// reaches.
			translate ([sideBarY/4,0]) geometricGrip();
			//triggerGuard();
		}

		fingerRoom();

		difference () {
			square([sideBarY/4, sideBarY/4]);
			translate ([sideBarY/4,sideBarY/4])
			circle (d = sideBarY/2);
		}

	}
}

module geometricGrip () {
	roundingInset = 20;
	gripAngle = 17;
	gripWidth = 45;
	gripLength = 110;

	theta = 90 - gripAngle;
	r = roundingInset * tan (theta/2);
	px = r * sin(theta);
	py = -1 * r * (1+cos(theta));
	bx = roundingInset - gripLength * cos(theta);
	by = gripLength * sin(theta);

	difference () {
		polygon ([
			[0,0],
			[roundingInset+gripWidth,0],
			[bx+gripWidth,-1*by],
			[bx,-1*by],
			[px,py]
		]);
		translate ([0,-1*r]) circle(r = r);
	}
}

module triggerGuard () {
	triggerR = 15.875;
	triggerBottomY = -10;
	height = triggerR-triggerBottomY+5;

	translate ([sideBarX/2-5, -1*height])
	square([sideBarX/2,height]);
	
}

module fingerRoom () {
	triggerR = 15.875;
	triggerBottomY = -10;

	translate ([sideBarX/2,0])
	difference () {
		translate ([0,triggerBottomY]) circle(r = triggerR);
		translate ([-1*triggerR,0]) square([2*triggerR,sideBarY]);
	}
	translate ([sideBarX/2+35,0])
	difference () {
		translate ([0,triggerBottomY]) circle(r = triggerR);
		translate ([-1*triggerR,0]) square([2*triggerR,sideBarY]);
	}
	translate ([sideBarX/2, triggerBottomY-triggerR])
		square([35,triggerR-triggerBottomY]);
}

module originalHandle () {
	sideBar ();
	originalGrip ();
}

module originalGrip () {
	translate ([0,-104.775])
	polygon ([
		[20.6375,0.0],
		[50.8,76.2],[39.6875,76.2],
		[39.6875,104.775],
		[0.0,104.775],[0.0,76.2],
		[-38.1,0.0]
	]);
}

module originalTrigger () {
	union () {
		intersection ()  {
			translate ([15.875, 15.875])
				circle (15.875);
			square([15.875, 15.875]);
		}
	difference () {
	polygon([
			[0.0,15.875],
			[0.0,37.30625],
			[7.14375,37.30625],
			[15.875,46.0375],
			[23.8125,46.0375],
			[28.575,37.30625],
			[28.575,27.78125],
			[22.225,27.78125],
			[15.875,23.8125],
			[15.875,15.875]
		]);
        translate ([15.875, 37.30625]) circle (d=3.96875);
        }
    }
}


module geometricTrigger () {
	sidePanelRecessHeight = 3; // copied from handle connector scad
	littleR = sideBarY/2 + sidePanelRecessHeight;
	touchR = littleR / 2;
	bigR = littleR + 2 * touchR;

	difference () {
		union () {
			circle(r = littleR);

			difference () {
				circle(r = bigR);

				translate ([-1*bigR,0])
				square ([bigR, bigR]);

				translate ([0,littleR])
				square ([bigR*2,bigR-littleR]);
			}

			
			translate ([-3*touchR, 0])
			union () {
				circle(r = touchR);
				square ([littleR,touchR]);
			}
		}
		circle (d = fromInches (3/16));
	}
}


union () {
	geometricHandle();
	/*
	#translate ([sideBarX/2-15.875,-15.875-sideBarY/2])
		originalTrigger();
     #originalHandle();
	//*/
}

