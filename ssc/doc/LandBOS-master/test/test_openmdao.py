#!/usr/bin/env python
# encoding: utf-8
"""
test_openmdao.py

Created by Andrew Ning on 2014-03-11.
Copyright (c) NREL. All rights reserved.
"""

import unittest
from landbos import LandBOS


class TestDefaultCosts(unittest.TestCase):

    def test_totalCost(self):
        bos = LandBOS()
        bos.machine_rating = 2000
        bos.rotor_diameter = 110
        bos.hub_height = 100
        bos.turbine_number = 100
        bos.voltage = 137
        bos.distInter = 5
        bos.terrain = 'FLAT_TO_ROLLING'
        bos.layout = 'COMPLEX'
        bos.soil = 'STANDARD'
        # bos.TCC = 1000.0
        bos.turbine_cost = 1000.0 * bos.machine_rating
        bos.RNA_mass = 88.0 *1000

        bos.run()

        self.assertAlmostEqual(285646716, bos.bos_costs, delta=0.5)


if __name__ == '__main__':
    unittest.main()
