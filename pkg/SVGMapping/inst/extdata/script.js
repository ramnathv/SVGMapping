// SVGMapping JavaScript script

// Tooltip system

var annotElements = new Array()

function init(evt)
{
	if (window.svgDocument == null)
		svgDocument = evt.target.ownerDocument;
}

function displayAnnotation(evt, name, description, foldchanges, colors)
{
	var svgNS = "http://www.w3.org/2000/svg";

	var n = foldchanges.length

	var h = 110;
	var w = 400;

	ww = svgDocument.rootElement.getBBox().width;
	wh = svgDocument.rootElement.getBBox().height;

	bgrect = svgDocument.createElementNS(svgNS, "rect")
	bgrect.setAttributeNS(null, "width", w)
	bgrect.setAttributeNS(null, "height", h)
	bgrect.setAttributeNS(null, "fill", "#eeeeee")
	bgrect.setAttributeNS(null, "stroke", "black")
	svgDocument.rootElement.appendChild(bgrect)
	annotElements.push(bgrect);

	if (description != null) {
		descel = svgDocument.createElementNS(svgNS, "text")
		descel.appendChild(svgDocument.createTextNode(description))
		descel.setAttributeNS(null, "font-size", "80%")
		svgDocument.rootElement.appendChild(descel)
		var tw = descel.getComputedTextLength()
		var l = description.length
		while (tw + 20 > w && tw > 10) {
			l = l - 1
			descel.firstChild.replaceWholeText(description.substr(0, l) + "...")
			tw = descel.getComputedTextLength()
		}
		annotElements.push(descel);
	}

	bgrectTitle = svgDocument.createElementNS(svgNS, "rect")
	bgrectTitle.setAttributeNS(null, "width", w)
	bgrectTitle.setAttributeNS(null, "height", 30)
	bgrectTitle.setAttributeNS(null, "fill", "#dddddd")
	bgrectTitle.setAttributeNS(null, "stroke", "black")
	svgDocument.rootElement.appendChild(bgrectTitle)
	annotElements.push(bgrectTitle);

	title1el = svgDocument.createElementNS(svgNS, "text")
	title1el.setAttributeNS(null, "font-weight", "bold")
	title1el.appendChild(svgDocument.createTextNode(name))
	svgDocument.rootElement.appendChild(title1el)
	annotElements.push(title1el);

	var cw = (w-20)/n
	var boxes = new Array(n)
	var fcs = new Array(n)
	for (var i=0; i<n; i++) {
		el = svgDocument.createElementNS(svgNS, "rect")
		el.setAttributeNS(null, "width", cw)
		el.setAttributeNS(null, "height", 30)
		el.setAttributeNS(null, "fill", colors[i])
		el.setAttributeNS(null, "stroke", "black")
		svgDocument.rootElement.appendChild(el)
		annotElements.push(el);
		boxes[i] = el
		elt = svgDocument.createElementNS(svgNS, "text")
		elt.appendChild(svgDocument.createTextNode(foldchanges[i]))
		elt.setAttributeNS(null, "font-family", "Arial")
		fcval = parseFloat(foldchanges[i])
		elt.setAttributeNS(null, "fill", "black")
		if (colors[i].charAt(0) == '#' && colors[i].length == 7) {
			var c = colors[i].toLowerCase();
			if (parseInt(c.substring(1,3),16)*299 + parseInt(c.substring(3,5),16)*587 + parseInt(c.substring(5,7),16)*114 < 125000) {
				// This background color appears quite dark, let's write the text in white
				elt.setAttributeNS(null, "fill", "white")
			}
		}
		svgDocument.rootElement.appendChild(elt)
		annotElements.push(elt);
		fcs[i] = elt
	}

	// Compute box position
	x = evt.clientX + 9 + window.pageXOffset;
	y = evt.clientY + 17 + window.pageYOffset;
	if (x + w > ww) {
		// The tooltip is out of the SVG document (on the right), so display it on the left of the mouse cursor
		x = x - w;
	}
	if (y + h > wh) {
		// Same idea, when tooltip is below the document bottom
		y = y - h - 25;
	}

	// Put all elements on the right place
	bgrect.setAttributeNS(null, "x", x)
	bgrect.setAttributeNS(null, "y", y)
	bgrectTitle.setAttributeNS(null, "x", x)
	bgrectTitle.setAttributeNS(null, "y", y)
	title1el.setAttributeNS(null, "x", x + 10)
	title1el.setAttributeNS(null, "y", y + 20)
	if (description != null) {
		descel.setAttributeNS(null, "x", x + 10)
		descel.setAttributeNS(null, "y", y + 50)
	}
	for (var i=0; i<n; i++) {
		boxes[i].setAttributeNS(null, "y", y + 70)
		boxes[i].setAttributeNS(null, "x", x + 10 + cw*i)
		fcs[i].setAttributeNS(null, "y", y + 90)
		fcs[i].setAttributeNS(null, "x", x + 10 + cw*i + cw/2 - fcs[i].getComputedTextLength()/2)
	}
}

function hideAnnotation(evt)
{
	while (annotElements.length > 0) {
		el = annotElements.pop();
		el.parentNode.removeChild(el);
	}
}



// Animation engine

var timerIncrement = 10
var currentAnimation = null
var offset1 = null
var offset2 = null
var stop1 = null
var stop2 = null

function stopAnimation() {
	if (currentAnimation == "partial-fill") {
		// end animation: restore original values
		stop1.offset.baseVal = offset1
		stop2.offset.baseVal = offset2
		currentAnimation = null
	}
}

function nextAnimationStep() {
	if (currentAnimation == "partial-fill") {
		var currentOffset = stop1.offset.baseVal
		currentOffset += 0.02
		if (currentOffset > offset1) {
			stopAnimation()
		} else {
			// continue animation
			stop1.offset.baseVal = currentOffset
			stop2.offset.baseVal = currentOffset
			setTimeout("nextAnimationStep()", timerIncrement)
		}
	}
}

function animatePartialFill(evt) {
	// stop any already running animation
	stopAnimation()
	var element = evt.target
	var gradientId = element.style.fill
	var gradient = svgDocument.getElementById(gradientId.substring(1,gradientId.length))
	var k = 1
	for (var i=0; i<gradient.childNodes.length; i++) {
		var child = gradient.childNodes[i]
		if (child.tagName == "stop") {
			if (k == 2) {
				stop1 = child
			} else if (k == 3) {
				stop2 = child
			}
			k++
		}
	}
	currentAnimation = "partial-fill"
	// save real values
	offset1 = stop1.offset.baseVal
	offset2 = stop2.offset.baseVal
	stop1.offset.baseVal = 0
	stop2.offset.baseVal = 0
	nextAnimationStep()
}
