#!/usr/bin/env python

import inkex
# simplestyle module provides functions for style parsing.
from simplestyle import *
from simpletransform import *

""" Erlenmeyer effect extension: Create a labelled Erlenmeyer shape """
class ErlenmeyerEffect(inkex.Effect):

    """ Constructor. Defines "--what" option of a script."""
    def __init__(self):
        # Call base class constructor.
        inkex.Effect.__init__(self)

        # Define options
        self.OptionParser.add_option('-l', '--label', action='store',
                                     type='string', dest='label',
                                     default='thermometer',
                                     help='Shape Label?')
        self.OptionParser.add_option('-v', '--volume', action='store',
                                     type='int', dest='volume',
                                     default=500,
                                     help='Volume value?')
        self.OptionParser.add_option('-u', '--volume_unit', action='store',
                                     type='string', dest='volume_unit',
                                     default='ml',
                                     help='Volume Unit?')


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
        volume = self.options.volume
        volume_unit = self.options.volume_unit

        # gradient colors
        color376_1 = { 'stop-color' : '#aaaaaa' , 'stop-opacity' : '1' }
        color376_2 = { 'stop-color' : '#777777' , 'stop-opacity' : '1' }

        # Get access to main SVG document element and get its dimensions.
        svg = self.document.getroot()
        width = inkex.unittouu(svg.get('width'))
        height = inkex.unittouu(svg.get('height'))

        # Add Gradient definitions
        if not self.checkGradient('ERlinearGradient376'):
            self.addLinearGradient({0:color376_1, 1:color376_2}, 'ERlinearGradient376')
            self.addLinearGradientRef('ERlinearGradient376', 'ERlinearGradient382',
                                      x1=339.7515, y1=510.84944, x2=368.33879, y2=510.84944,
                                      transform='matrix(1.1071194,0,0,1.2904384,-35.841742,-147.2028)')
            self.addLinearGradientRef('ERlinearGradient376', 'ERlinearGradient386',
                                      x1=339.7515, y1=510.84944, x2=368.33879, y2=510.84944,
                                      transform='matrix(1.4115359,0,0,1.2822548,-154.82265,-61.724023)')
            self.addLinearGradientRef('ERlinearGradient376', 'ERlinearGradient390',
                                      x1=339.7515, y1=510.84944, x2=368.33879, y2=510.84944,
                                      transform='matrix(1.5800346,0,0,1.2702251,-211.89779,-5.7221458)')
            self.addLinearGradientRef('ERlinearGradient376', 'ERlinearGradient394',
                                      x1=339.7515, y1=510.84944, x2=368.33879, y2=510.84944,
                                      transform='matrix(1.7162832,0,0,1.2584892,-259.59041,41.338857)')


        # Create a new group for the shape
        group = inkex.etree.SubElement(svg,'g')

        # Path 1 (erlenmeyer background)
        style = {
            'fill' : '#d6e6ec',
            'fill-opacity' : '1',
            'stroke' : 'none'
            }
        self.addPath(group, style,
                    'm 272.92676,304.83298 c 0,11.76641 0,11.76641 9.0818,11.76641 l 0,85.42525 c 0,0 -79.56639,200.51847 -95.70128,306.24409 -1.1104,7.27605 0,22.08087 0,22.08087 0,19.78554 17.23234,34.50135 40.49865,34.50135 l 187.73889,0 c 26.10408,0 40.87297,-14.71581 40.87297,-34.24934 0,0 1.06374,-15.26759 0,-22.78911 C 440.57654,602.8727 367.24768,402.32986 367.24768,402.32986 l 0,-85.73047 c 10.40048,0 10.40048,0 10.40048,-11.76641 z',
                    'cccsccccscccc')

        # Path 2 (flask filling)
        style = {
            'fill' : '#69b969',
            'fill-opacity' : '1',
            'stroke' : 'none'
            }
        self.addPath(group, style,
                     'm 240.80285,511.99368 c 0,0 -51.92205,155.22675 -54.49557,196.27505 -0.46055,7.34587 0,22.08087 0,22.08087 0,19.78554 17.23234,34.50135 40.49865,34.50135 l 187.73889,0 c 26.10408,0 40.87297,-14.71581 40.87297,-34.24934 0,0 0.78465,-15.23338 0,-22.78911 -6.35059,-61.15295 -50.70266,-195.81882 -50.70266,-195.81882 z',
                     'csccccscc', label=label)

        # Path 3 (erlenmeyer contour)
        style = {
            'fill' : 'none',
            'stroke' : '#000000',
            'stroke-width' : '1px',
            'stroke-linecap' : 'butt',
            'stroke-linejoin' : 'miter',
            'stroke-opacity' : '1'
            }
        self.addPath(group, style,
                    'm 272.92676,304.83298 c 0,11.76641 0,11.76641 9.0818,11.76641 l 0,85.42525 c 0,0 -79.56639,200.51847 -95.70128,306.24409 -1.1104,7.27605 0,22.08087 0,22.08087 0,19.78554 17.23234,34.50135 40.49865,34.50135 l 187.73889,0 c 26.10408,0 40.87297,-14.71581 40.87297,-34.24934 0,0 1.06374,-15.26759 0,-22.78911 C 440.57654,602.8727 367.24768,402.32986 367.24768,402.32986 l 0,-85.73047 c 10.40048,0 10.40048,0 10.40048,-11.76641 z',
                    'cccsccccscccc')

        # Rect (flask opening)
        style = {
            'fill' : '#d6e6ec',
            'fill-opacity' : '1',
            'stroke' : '#000000',
            'stroke-width' : '1.8',
            'stroke-miterlimit' : '4',
            'stroke-opacity' : '1',
            'stroke-dasharray' : 'none'
            }
        self.addRect(group,style, x=272.92676, y=298.6994,
                     width=104.7214, height=6.1335702)

        # Rect (graduations)
        style = {
            'fill' : 'url(#ERlinearGradient382)',
            'fill-opacity': '1',
            'stroke' : '#707070',
            'stroke-width' : '0.6',
            'stroke-linecap': 'round',
            'stroke-linejoin' : 'miter',
            'stroke-miterlimit' : '4',
            'stroke-opacity': '1',
            'stroke-dasharray' : 'none',
            'stroke-dashoffset' : '0'
            }
        self.addRect(group, style, x=341.41083, y=509.91632,
                     width=29.435314, height=4.2011352)
        style['fill'] = 'url(#ERlinearGradient386)'
        self.addRect(group, style, x=326.16037, y=591.22772,
                     width=37.528923, height=4.1744928,
                     transform='matrix(0.99996559,-0.00829607,0.0329568,0.99945678,0,0)')
        style['fill'] = 'url(#ERlinearGradient390)'
        self.addRect(group, style, x=326.50137, y=641.10394,
                     width=42.00885, height=4.1353288,
                     transform='matrix(0.99998363,-0.00572226,0.03449938,0.99940472,0,0)')
        style['fill'] = 'url(#ERlinearGradient394)'
        self.addRect(group, style, x=325.23566, y=682.18878,
                     width=45.631332, height=4.0971222,
                     transform='matrix(0.99998638,-0.00521868,0.03782388,0.99928442,0,0)')

        # Rect (decoration)
        style = {
            'fill' : '#585858',
            'fill-opacity' : '1',
            'stroke' : 'none'
            }
        self.addRect(group, style, x=314.23929, y=547.14032,
                     width=69.893227, height=51.11594, ry=15.647737,
                     transform='matrix(0.99128658,0.13172285,-0.13172285,0.99128658,0,0)')

        # Text (decorations)
        style = {
            'font-size': '26.298px',
            'font-style' : 'normal',
            'font-weight' : 'bold',
            'line-height' : '125%',
            'letter-spacing' : '0px',
            'word-spacing' : '0px',
            'fill' : '#5a5a5a',
            'fill-opacity': '1',
            'stroke' : 'none',
            'font-family' : 'Bitstream Vera Sans',
            '-inkscape-font-specification' : 'Bitstream Vera Sans Bold'
            }
        self.addText(group,style,x=310.61066,y=481.88983,
                     text='Pyrex',
                     transform='matrix(0.93460484,0.1179669,-0.0949635,1.0579845,0,0)')
        style['font-size'] = '18px'
        style['font-weight'] = 'normal'
        self.addText(group,style,x=322.96979, y=540.66229,
                     text='%d %s' % (volume, volume_unit),
                     transform='matrix(0.98998402,0.14117944,-0.14117944,0.98998402,0,0)')
        style['font-size'] = '10.77px'
        self.addText(group,style,x=-682.25464, y=301.17377,
                     text='APPROX. VOL',
                     transform='matrix(-0.06004289,-0.9981958,0.9981958,-0.06004289,0,0)')

        # Text (graduations)
        if volume>=1000:
            style['font-size'] = '12px'
        else:
            style['font-size'] = '14px'
        vol_step = volume//5
        self.addText(group,style,x=375.35226, y=517.07501,
                     text='%d' % (5*vol_step))
        self.addText(group,style, x=380.24792, y=600.80164,
                     text='%d' % (4*vol_step),
                     transform='matrix(0.99989512,-0.01448274,0.01448274,0.99989512,0,0)')
        self.addText(group,style,x=396.09082, y=645.86896,
                     text='%d' % (3*vol_step))
        self.addText(group,style, x=402.30695, y=686.91119,
                     text='%d' % (2*vol_step))
        
# Create effect instance and apply it
effect = ErlenmeyerEffect()
effect.affect()
