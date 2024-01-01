// Warning, this is in inches.
coneHeight = 0.150;
backStopR = 1.250 / 2;
backStopHeight = 0.150;
shaftHeight = 0.325;
sleeveHeight = 0.500;
coneFrontR = 0.771 / 2;
coneBackR = backStopR * 0.9;
shaftR = 0.875 / 2;
sleeveHeight = 0.500;
sleeveR = 0.575 / 2;
rodHoleR = 0.500 / 2;

$fn = 1000;

difference () {
    union () {
        cylinder (r2 = coneBackR, r1 = coneFrontR, h = coneHeight);
        translate ([0,0,coneHeight])
            cylinder (r = shaftR, h = shaftHeight);
        translate ([0,0,coneHeight+shaftHeight])
            cylinder (r = backStopR, h = backStopHeight);
        translate ([0,0,coneHeight+shaftHeight+backStopHeight])
            cylinder (r = sleeveR, h = sleeveHeight);
    }
    translate ([0,0,coneHeight])
        cylinder (
            r = rodHoleR,
            h = coneHeight+shaftHeight+backStopHeight+sleeveHeight+1);
}
