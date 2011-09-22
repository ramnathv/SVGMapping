#!/usr/bin/env python

import inkex
# simplestyle module provides functions for style parsing.
from simplestyle import *
from simpletransform import *

""" Ependorf effect extension: Create a labelled ependorf shape """
class EpendorfEffect(inkex.Effect):
    """ Constructor. Defines "--what" option of a script."""
    def __init__(self):
        # Call base class constructor.
        inkex.Effect.__init__(self)

        # Define string option
        self.OptionParser.add_option('-l', '--label', action='store',
                                     type='string', dest='label',
                                     default='ependorf',
                                     help='Shape Label?')

    """ Effect behaviour. Oberrides base class' method. """
    def effect(self):

        # retrieve label
        label = self.options.label

        # define styles
        style1 = {
            'fill' : '#d6e6ec',
            'fill-opacity' : '1',
            'stroke' : '#000000',
            'stroke-width' : '4',
            'stroke-linecap' : 'butt',
            'stroke-linejoin' : 'miter',
            'stroke-miterlimit' : '4',
            'stroke-opacity' : '1',
            'stroke-dasharray' : 'none'
        }
        style2 = {
            'fill' : '#ff4343',
            'fill-opacity' : '1',
            'stroke' : 'none',
            'display' : 'inline'
        }
        style3 = {
            'opacity' : '0.3',
            'fill' : '#000000',
            'fill-opacity' : '1',
            'stroke' : 'none'
        }
        style4 = {
            'fill' : 'none',
            'stroke' : '#000000',
            'stroke-width' : '4',
            'stroke-linecap' : 'butt',
            'stroke-linejoin' : 'miter',
            'stroke-miterlimit' : '4',
            'stroke-opacity' : '1',
            'stroke-dasharray' : 'none',
            'display' : 'inline'
        }
        
        # Get access to main SVG document element and get its dimensions.
        svg = self.document.getroot()
        width = inkex.unittouu(svg.get('width'))
        height = inkex.unittouu(svg.get('height'))

        # Create a new group for the shape
        group = inkex.etree.SubElement(svg,'g')
        group.set('transform', 'matrix(0.2,0,0,0.2,%f,%f)' % (width/2, height/2) )

        # back 1/5
        path = inkex.etree.Element(inkex.addNS('path','svg'))
        path.set('style', formatStyle(style1))
        path.set('d', 'm 53.626832,237.83881 132.865088,87.10045 -9.70888,14.6732 L 43.825127,252.64 z')
        group.append(path)

        # back 2/5
        path = inkex.etree.Element(inkex.addNS('path','svg'))
        path.set('style', formatStyle(style1))
        path.set('d', 'm 90.193079,213.41382 -21.810543,34.09817 103.387834,67.77647 23.5192,-35.63517 z')
        group.append(path)

         # back 3/5
        path = inkex.etree.Element(inkex.addNS('path','svg'))
        path.set('style', formatStyle(style1))
        path.set('d', 'm 249.1764,338.56763 0,17.63402 151.03923,0 0,-17.63402 z')
        group.append(path)

         # back 4/5
        path = inkex.etree.Element(inkex.addNS('path','svg'))
        path.set('style', formatStyle(style1))
        path.set(inkex.addNS('nodetypes','sodipodi'), 'cczccc')
        path.set('d', 'm 257.61006,356.20165 0,230.00899 c 0,0 27.64765,226.9422 66.70261,226.9422 39.05496,0 68.236,-226.9422 68.236,-226.9422 l 0,-230.00899 z')
        group.append(path)

        # back 5/5
        path = inkex.etree.Element(inkex.addNS('path','svg'))
        path.set('style', formatStyle(style1))
        path.set(inkex.addNS('nodetypes','sodipodi'), 'czcczcc')
        path.set('d', 'm 184.71041,327.63169 c 0,0 12.71439,12.24709 23.75073,15.53612 11.03634,3.28903 40.71526,0 40.71526,0 l 0,9.96706 c 0,0 -29.85465,3.5594 -41.79819,0 -11.94354,-3.5594 -27.59766,-18.0526 -27.59766,-18.0526 z')
        group.append(path)

        # content 1/1
        path = inkex.etree.Element(inkex.addNS('path','svg'))
        path.set('style', formatStyle(style2))
        path.set(inkex.addNS('nodetypes','sodipodi'), 'cczccc')
        path.set('d', 'm 257.61006,356.20165 0,230.00899 c 0,0 27.64765,226.9422 66.70261,226.9422 39.05496,0 68.236,-226.9422 68.236,-226.9422 l 0,-230.00899 z')
        path.set(inkex.addNS('label', 'inkscape'), label)
        group.append(path)

        # front 1/x
        rect = inkex.etree.Element(inkex.addNS('rect','svg'))
        rect.set('style', formatStyle(style3))
        rect.set('width', '72.836182')
        rect.set('height', '7.666966')
        rect.set('x', '267.57712')
        rect.set('y', '397.60327')
        rect.set('ry', '2.8527629')
        group.append(rect)
        
        # front 2/x
        rect = inkex.etree.Element(inkex.addNS('rect','svg'))
        rect.set('style', formatStyle(style3))
        rect.set('width', '72.836182')
        rect.set('height', '7.666966')
        rect.set('x', '267.57712')
        rect.set('y', '510.30768')
        rect.set('ry', '2.8527629')
        group.append(rect)

        # front 3/x
        rect = inkex.etree.Element(inkex.addNS('rect','svg'))
        rect.set('style', formatStyle(style3))
        rect.set('width', '72.836182')
        rect.set('height', '7.666966')
        rect.set('x', '267.57712')
        rect.set('y', '623.77875')
        rect.set('ry', '2.8527629')
        group.append(rect)

        # front 4/x
        path = inkex.etree.Element(inkex.addNS('path','svg'))
        path.set('style', formatStyle(style4))
        path.set('d', 'm 249.1764,338.56763 0,17.63402 151.03923,0 0,-17.63402 z')
        group.append(path)

        # front 5/x
        path = inkex.etree.Element(inkex.addNS('path','svg'))
        path.set('style', formatStyle(style4))
        path.set(inkex.addNS('nodetypes','sodipodi'), 'cczccc')
        path.set('d', 'm 257.61006,356.20165 0,230.00899 c 0,0 27.64765,226.9422 66.70261,226.9422 39.05496,0 68.236,-226.9422 68.236,-226.9422 l 0,-230.00899 z')
        group.append(path)


# Create effect instance and apply it
effect = EpendorfEffect()
effect.affect()
