#!/usr/bin/env python

import inkex
# simplestyle module provides functions for style parsing.
from simplestyle import *
from simpletransform import *

""" Thermometer effect extension: Create a labelled thermometer shape """
class ThermometerEffect(inkex.Effect):
    
    """ Constructor. Defines "--what" option of a script."""
    def __init__(self):
        # Call base class constructor.
        inkex.Effect.__init__(self)

        # Define options
        self.OptionParser.add_option('-l', '--label', action='store',
                                     type='string', dest='label',
                                     default='thermometer',
                                     help='Shape Label?')
        self.OptionParser.add_option('-g', '--grad_labels', action='store',
                                     type='string', dest='grad_labels',
                                     default='true',
                                     help='Graduation labels?')
        self.OptionParser.add_option('-m', '--grad_min', action='store',
                                     type='float', dest='min_range',
                                     default=0.0,
                                     help='Minimum range value?')
        self.OptionParser.add_option('-M', '--grad_max', action='store',
                                     type='float', dest='max_range',
                                     default=100.0,
                                     help='Maximum range value?')
        self.OptionParser.add_option('-u', '--grad_unit', action='store',
                                     type='string', dest='grad_unit',
                                     default='',
                                     help='Graduation Units?')
        

    def checkGradient(self, gid):
        try:
            retval = self.document.xpath('//svg:linearGradient[@id="%s"]' % gid, namespaces=inkex.NSS)[0]
        except:
            return False
        return True

    def addLinearGradient(self, colors, gid):
        defs = self.xpathSingle('/svg:svg//svg:defs')
        if defs == None:
            defs = inkex.etree.SubElement(self.document.getroot(),inkex.addNS('defs','svg'))
        gradient = inkex.etree.SubElement(defs,inkex.addNS('linearGradient','svg'))
        gradient.set('id', gid)
        for (off,color) in colors.items():
            lg_stop = inkex.etree.Element(inkex.addNS('stop','svg'))
            lg_stop.set('style',formatStyle(color))
            lg_stop.set('offset', '%d' % off)
            gradient.append(lg_stop)
        defs.append(gradient)

    def addLinearGradientRef(self, href_id, gid, x1, y1, x2, y2, transform=None):
        defs = self.xpathSingle('/svg:svg//svg:defs')
        if defs == None:
            defs = inkex.etree.SubElement(self.document.getroot(),inkex.addNS('defs','svg'))
        gradient = inkex.etree.SubElement(defs,inkex.addNS('linearGradient','svg'))
        gradient.set(inkex.addNS('collect', 'inkscape'), 'always')
        gradient.set('id', gid)
        gradient.set(inkex.addNS('href','xlink'), '#%s' % href_id)
        gradient.set('x1', '%f' % x1)
        gradient.set('y1', '%f' % y1)
        gradient.set('x2', '%f' % x2)
        gradient.set('y2', '%f' % y2)
        gradient.set('gradientUnits', 'userSpaceOnUse')
        if not transform == None:
            gradient.set('gradientTransform', transform)
        defs.append(gradient)

    def addRadialGradientRef(self, href_id, gid, cx, cy, fx, fy, r, transform=None):
        defs = self.xpathSingle('/svg:svg//svg:defs')
        if defs == None:
            defs = inkex.etree.SubElement(self.document.getroot(),inkex.addNS('defs','svg'))
        gradient = inkex.etree.SubElement(defs,inkex.addNS('radialGradient','svg'))
        gradient.set(inkex.addNS('collect', 'inkscape'), 'always')
        gradient.set('id', gid)
        gradient.set(inkex.addNS('href','xlink'), '#%s' % href_id)
        gradient.set('cx', '%f' % cx)
        gradient.set('cy', '%f' % cy)
        gradient.set('fx', '%f' % fx)
        gradient.set('fy', '%f' % fy)
        gradient.set('r', '%f' % r)
        gradient.set('gradientUnits', 'userSpaceOnUse')
        if not transform == None:
            gradient.set('gradientTransform', transform)
        defs.append(gradient)
        
    def addPath(self, group, style, d, nodetypes=None, label=None):
        path = inkex.etree.Element(inkex.addNS('path','svg'))
        path.set('style', formatStyle(style))
        path.set('d', d)
        if not label==None:
            path.set(inkex.addNS('label','inkscape'),label)
        if not nodetypes==None:
            path.set(inkex.addNS('nodetypes', 'sodipodi'), nodetypes)
        group.append(path)

    def addArc(self, group, style, d, cx, cy, rx, ry, start=None, end=None, open=None, transform=None):
        path = inkex.etree.Element(inkex.addNS('path','svg'))
        path.set('style', formatStyle(style))
        path.set('d', d)
        path.set(inkex.addNS('type', 'sodipodi'), 'arc')
        path.set(inkex.addNS('cx', 'sodipodi'), '%f' % cx)
        path.set(inkex.addNS('cy', 'sodipodi'), '%f' % cy)
        path.set(inkex.addNS('rx', 'sodipodi'), '%f' % rx)
        path.set(inkex.addNS('ry', 'sodipodi'), '%f' % ry)
        if not start==None:
            path.set(inkex.addNS('start','sodipodi'), '%f' % start)
            path.set(inkex.addNS('end','sodipodi'), '%f' % end)
            path.set(inkex.addNS('open','sodipodi'), open)
        if not transform==None:
            path.set('transform', transform)
        group.append(path)

    def addRect(self, group, style, x, y, width, height, ry=0, transform=None):
        rect = inkex.etree.Element(inkex.addNS('rect','svg'))
        rect.set('style', formatStyle(style))
        rect.set('x', '%f' % x)
        rect.set('y', '%f' % y)
        rect.set('width', '%f' % width)
        rect.set('height', '%f' % height)
        rect.set('ry', '%f' % ry)
        if not transform==None:
            rect.set('transform', transform)
        group.append(rect)

    def addText(self, group, style, x, y, text, transform=None):
        etext = inkex.etree.Element(inkex.addNS('text','svg'))
        etext.set('style', formatStyle(style))
        etext.set('x', '%f' % x)
        etext.set('y', '%f' % y)
        etext.set(inkex.addNS('linespacing', 'sodipodi'), '125%')
        etext.text = text
        if not transform==None:
            etext.set('transform', transform)
        group.append(etext)
        
        
    """ Effect behaviour. Oberrides base class' method. """
    def effect(self):
        
        # retrieve label
        label = self.options.label
        if self.options.grad_labels=='true':
            grad_labels = True
        else:
            grad_labels = False
        min_range = self.options.min_range
        max_range = self.options.max_range
        grad_unit = self.options.grad_unit

        # gradient colors
        color4266_1 = { 'stop-color' : '#7d7d7d' , 'stop-opacity' : '1' }
        color4266_2 = { 'stop-color' : '#c4c4c4' , 'stop-opacity' : '1' }
        color4246_1 = { 'stop-color' : '#ff0909' , 'stop-opacity' : '1' }
        color4246_2 = { 'stop-color' : '#00338A' , 'stop-opacity' : '1' }
        color0001_1 = { 'stop-color' : '#003DC7' , 'stop-opacity' : '1' }
        color0001_2 = { 'stop-color' : '#00338A' , 'stop-opacity' : '1' }
        color4400_1 = { 'stop-color' : '#909090' , 'stop-opacity' : '1' }        
        color4400_2 = { 'stop-color' : '#4d4d4d' , 'stop-opacity' : '1' }

        # Get access to main SVG document element and get its dimensions.
        svg = self.document.getroot()
        width = inkex.unittouu(svg.get('width'))
        height = inkex.unittouu(svg.get('height'))

        # Add Gradient definitions
        if not self.checkGradient('THlinearGradient4266'):
            self.addLinearGradient({0:color4266_1, 1:color4266_2}, 'THlinearGradient4266')
            self.addLinearGradientRef('THlinearGradient4266', 'THlinearGradient4340',
                                      x1=438.19787, y1=341.11095, x2=441.19787, y2=341.11095)
            self.addLinearGradient({0:color4246_1, 1:color4246_2}, 'THlinearGradient4246')
            self.addLinearGradientRef('THlinearGradient4246', 'THlinearGradient4288',
                                      x1=450.0, y1=197.2, x2=450.0, y2=476.4)
            self.addLinearGradient({0:color0001_1, 1:color0001_2}, 'THlinearGradient0001')
            self.addRadialGradientRef('THlinearGradient0001', 'THradialGradient4252',
                                       cx=92.899834, cy=496.64462,
                                       fx=92.899834, fy=496.64462, 
                                       r=16.394089,
                                       transform='matrix(0.88150756,0,0,0.56269906,11.007928,217.18317)')
            self.addLinearGradient({0:color4400_1, 1:color4400_2}, 'THlinearGradient4400')

        # Create a new group for the shape
        group = inkex.etree.SubElement(svg,'g')
        group.set('transform', 'matrix(1.0,0,0,1.0,%f,%f)' % (0, 0) )

        # Path 1 (bottom circle)
        style = {
            'fill' : 'url(#THradialGradient4252)',
            'fill-opacity' : '1',
            'stroke' : '#6e6e6e',
            'stroke-width': '0.5',
            'stroke-linecap' : 'round',
            'stroke-linejoin' : 'round',
            'stroke-miterlimit': '4',
            'stroke-opacity' : '1',
            'stroke-dasharray' : 'none'
            }
        self.addArc(group, style,
                    'm 102.94692,483.35783 a 16.394089,16.814449 0 1 1 -19.997154,-0.0766',
                    cx=92.899834, cy=496.64462, rx=16.394089, ry=16.814449,
                    start=5.3720488, end=10.343382, open='true', transform='translate(356.88671,-5.3629009)')

        # Path 2 (thermometer background)
        style = {
            'fill' : '#d6e6ec',
            'stroke' : 'none',
            'stroke-width' : '1px',
            'stroke-linecap' : 'butt',
            'stroke-linejoin' : 'miter',
            'stroke-opacity' : '1',
            'fill-opacity' : '1'
            }
        self.addPath(group, style,
                     'm 439.63455,478.2371 20.24066,0.12296 0,-275.96832 c 0,0 0,-7.56651 -10.08867' +
                     ',-7.56651 -10.08867,0 -10.08867,7.56651 -10.08867,7.56651 z',
                     'ccczcc')

        # Path 3 (filling up to level 0)
        style = {
            'fill' : '#00338A',
            'stroke' : 'none',
            'stroke-width' : '1px',
            'fill-opacity' : '1'
            }
        self.addPath(group, style,
                     'm 439.7786,478.66217 20.24066,0 -0.0387,-5.2259 -20.28271,0 z',
                     'ccccc')

        # Path 4 (main bar filling)
        style = {
            'fill' : 'url(#THlinearGradient4288)',
            'stroke' : 'none',
            'stroke-width' : '1px',
            'fill-opacity' : '1'
            }
        self.addPath(group, style,
                     'm 439.7786,473.74954 20.24066,0 -0.0387,-267.40595 -20.28271,0 z',
                     'ccccc',label=label)

        # Rect (graduations)
        style = {
            'fill': 'url(#THlinearGradient4400)',
            'fill-opacity' : '1.0',
            'stroke' : '#030303',
            'stroke-width': '0.1',
            'stroke-miterlimit' : '4',
            'stroke-dasharray' : 'none',
            'stroke-opacity' : '1'
            }
        major_first = 205.82706
        major_final = 473.24843
        M_step = (major_final - major_first)/10
        for i in range(0,10) :
            self.addRect(group, style, x=453.3757, y=major_first + i*M_step,
                         width=6.4867501, height=1.07219)
            for j in range(1,4):
                minor_y = major_first + i*M_step + j*(M_step/4)
                self.addRect(group, style, 457.02112, y=minor_y,
                             width=2.8413255, height=1.07219)
        self.addRect(group, style, x=453.3757, y=major_final,
                         width=6.4867501, height=1.07219)

        
        # Text (graduation values)
        if grad_labels:
            style = {
                'font-size' : '10.7975893px',
                'font-style' : 'normal',
                'font-variant' : 'normal',
                'font-weight' : 'normal',
                'font-stretch' : 'normal',
                'text-align' : 'start',
                'line-height' : '125%',
                'letter-spacing' : '0px',
                'word-spacing' : '0px',
                'writing-mode' : 'lr-tb',
                'text-anchor' : 'start',
                'fill' : '#000000',
                'fill-opacity': '1',
                'stroke' : 'none',
                'font-family' : 'Arial',
                '-inkscape-font-specification': 'Arial'
            }
            grad_first = min_range
            grad_last = max_range
            grad_step = (grad_last-grad_first)/10
            font_delta = 10.7975893/2
            for i in range(0,11):
                self.addText(group,style,x=464.82135,y=major_first + (10-i)*M_step + font_delta,
                             text='%.1f%s' % (grad_first + i*grad_step, grad_unit))

        # Path 5 (main bar contour)
        style = {
            'fill' : 'none',
            'stroke' : '#6e6e6e',
            'stroke-width' : '0.5',
            'stroke-linecap' : 'round',
            'stroke-linejoin' : 'miter',
            'stroke-miterlimit' : '4',
            'stroke-opacity' : '1',
            'stroke-dasharray' : 'none'
            }
        self.addPath(group, style,
                     'm 439.69787,202.39174 c 0,0 0,-7.56651 10.08867,-7.56651 ' +
                     '10.08867,0 10.08867,7.56651 10.08867,7.56651 l -0.0774,275.82761',
                     'czcc')

        # Path 6 (left bar)
        style = {
            'fill' : 'none',
            'stroke' : 'url(#THlinearGradient4340)',
            'stroke-width' : '2',
            'stroke-linecap': 'round',
            'stroke-linejoin' : 'miter',
            'stroke-miterlimit' : '4',
            'stroke-opacity' : '1',
            'stroke-dasharray': 'none'
            }
        self.addPath(group, style, 'm 439.7753,477.8943 -0.0774,-275.50256')


# Create effect instance and apply it
effect = ThermometerEffect()
effect.affect()
