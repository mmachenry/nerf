#!/usr/bin/env python
"""
spring.py

A simple spring class that shines light on the properties of
Mcmaster cut-to-length compression springs.
"""

__author__ = "Thomas Megantz a.k.a. T da B"

class Spring(object):
    def __init__(self, name, rate_constant, coils_per_inch,
                 max_length, wire_diameter):
        self.name = name
        self.rate_constant = rate_constant
        self.coils_per_inch = coils_per_inch
        self.max_length = max_length
        self.wire_diameter = wire_diameter
        self.deflection_rate = 1 - (self.coils_per_inch * self.wire_diameter)
        self.k = self.rate_constant / self.max_length
        self.max_load = self.deflection_rate * self.rate_constant

    def print_stats(self):
        print '[ {0} ]'.format(self.name)
        print '='*40
        print 'Deflection rate = ' + str(self.deflection_rate)
        print 'Max load = {0:.2f} lbs ({1:.2f} kg)'.format(self.max_load, self.max_load/2.20462)
        print '-'*20
        for double_length in range(self.max_length*2):
            length = double_length / 2.0
            print 'length = ' + str(length+1) + ' in'
            spring_constant = self.rate_constant/(length+1)
            print 'spring constant = {0:.2f} lbs/in'.format(spring_constant)
            max_draw = (length+1)*self.deflection_rate
            print 'max draw = {0:.2f} in'.format(max_draw)
            for draw in range(1, int(max_draw)+2):
                if draw <= max_draw:
                    print ('{0} inch draw produces {1:.2f} lbs ({2:.2f} kg) of '
                           'force').format(draw, draw*spring_constant, draw*spring_constant/2.20462)
            print "-" * 20

    def get_length_for_load(self, draw, desired_load):
        spring_constant = desired_load / draw
        length = self.rate_constant / spring_constant
        print ("{0:.2f} lbs of force with a {1:.2f} inch draw requires a "
               "{2:.2f} inch spring").format(desired_load, draw, length)

def print_spring_data():
    # Add any springs here
    k26 = Spring(name='K26', rate_constant=42.69, coils_per_inch=3.09, max_length=11, wire_diameter=.08)
    k25 = Spring(name='K25', rate_constant=38.53, coils_per_inch=2.18, max_length=11, wire_diameter=.08)
    k18 = Spring(name='K18', rate_constant=32.92, coils_per_inch=2.09, max_length=11, wire_diameter=.091)
    k15 = Spring(name='K15', rate_constant=67.98, coils_per_inch=4, max_length=11, wire_diameter=.105)
    k14 = Spring(name='K14', rate_constant=73.96, coils_per_inch=2.45, max_length=11, wire_diameter=.105)

    # Make sure to add any new springs into the list below
    #springs = [k26, k25, k18, k15, k14]
    springs = [k25]

    for spring in springs:
        spring.print_stats()
        spring.get_length_for_load(3, 30)
        print "=" * 40

if __name__ == '__main__':
    print_spring_data()
