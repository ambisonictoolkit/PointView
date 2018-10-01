PointView : View {
	var <points;     // points should be Cartesians
	var <pointsNorm; // points normalized to rho = 1
	var <connections;
	var axisPnts;

	// drawing
	var <cen, <minDim;
	var <skewX = 0, <skewY = -0.95;
	var <translateX = 0, <translateY = 0.8;
	var az, bz = 3;               // perspective parameters, see originDist_, eyeDist_
	var <showIndices = true;      // show indices of points
	var <showAxes = true;         // show world axes
	var <showConnections = false; // show connections between points
	var perspective = true, ortho = false, orthoAxis = '+X', orthoRotations, orthoOffset;
	var xyz, <axisColors, <axisScale = 0.2;
	var frameRate = 25;
	var <pointColors, prPntDrawCols;
	var colsByHue = true, huesScrambled = false;  // if the colors have been set by hue range
	var <pointSize = 15, <pointDistScale = 0.333;
	var renderDistanceSize = true;
	var prevColors, highlighted = false;
	var connectionColor, indicesColor;
	var connStrokeWidthNear = 3, connStrokeWidthFar;
	var <groupColors, <colorGroups, defaultGroupColor;

	// movement
	var <baseRotation, <baseTilt, <baseTumble; // radians, rotations before any movement offsets are applied
	var <rotate, <tilt, <tumble;               // radians, rotations after any movement offsets are applied
	var <rotateRate, <tiltRate, <tumbleRate;   // Hz
	var <rotateStep, tiltStep, tumbleStep;     // radians
	var <rotateDir, <tiltDir, <tumbleDir;      // +/-1
	var <cycRotate, <cycTilt, <cycTumble;
	var <oscRotate, <oscTumble, <oscTilt;
	var rotateOscT, rotateOscPhsInc;
	var tiltOscT, tiltOscPhsInc;
	var tumbleOscT, tumbleOscPhsInc;
	var <rotateOscWidth, tiltOscWidth, tumbleOscWidth;
	var >rotatePhase, >tiltPhase, >tumblePhase; // phase index into the rotation oscillators
	var <rotateMode;       // \rtt or \ypr
	var <randomizedAxes;   // dictionary of booleans for randomize state of each axis
	var <>randomVariance;  // normalized value to apply to movement speed if randomized
	var extrinsicRotation = false; // flag if mouse interaction requires additional extrinsic rotation
	var exRot = 0, exTum = 0, prevExRot = 0, prevExTum = 0;
	var rotPxStep = 1;

	// views
	var <userView, <rotationView, <showView, <perspectiveView;

	// other
	var sdClass;


	*new { |parent, bounds = (Rect(0,0, 1000, 715))|
		^super.new(parent, bounds).init;
	}

	init { |argSpec, initVal|
		var initOscWidth = 8.degrad;

		points = [];
		pointsNorm = [];
		az = bz + 1; // distance to point from eye

		// init  vars
		tumbleRate = tiltRate = rotateRate = 30.reciprocal;
		baseRotation = -45.degrad;
		baseTilt = baseTumble = 0;
		rotate = tilt = tumble = 0;
		rotateDir = tiltDir = tumbleDir = 1;
		cycRotate = cycTumble = cycTilt = false;
		oscRotate = oscTumble = oscTilt = false;
		rotatePhase = tiltPhase = tumblePhase = 0;
		rotateMode = \rtt;
		randomVariance = 0.15;
		connectionColor = Color.blue.alpha_(0.1);
		indicesColor = Color.black;
		groupColors = [Color.red];
		defaultGroupColor = Color.gray.alpha_(0.3);
		connStrokeWidthFar = connStrokeWidthNear * pointDistScale;

		sdClass = \SphericalDesign.asClass; // support for SphericalDesign without explicit dependency

		userView = UserView(this, this.bounds.origin_(0@0))
		.resize_(5)
		.frameRate_(frameRate)
		.drawFunc_(this.drawFunc)
		;

		// origin, x, y, z
		axisPnts = [[0,0,0], [1,0,0], [0,1,0], [0,0,1]].collect(_.asCartesian);
		axisColors = [\blue, \red, \green].collect{ |col| Color.perform(col, 1, 0.7) };
		xyz = #["X", "Y", "Z"];

		// init draw colors
		prPntDrawCols = [Color.hsv(0,1,1,1), Color.hsv(0.999,1,1,1)];
		colsByHue = true;
		randomizedAxes = IdentityDictionary(know: true).putPairs([
			\rotate, true,
			\tilt,   true,
			\tumble, true
		]);

		// init movement variables
		this.rotateOscPeriod_(this.rotatePeriod);
		this.tiltOscPeriod_(this.tiltPeriod);
		this.tumbleOscPeriod_(this.tumblePeriod);
		tumbleOscWidth = tiltOscWidth = rotateOscWidth = initOscWidth;
		this.rotateMode_(rotateMode);
		this.rotateCycRate_(rotateRate);
		this.tiltCycRate_(tiltRate);
		this.tumbleCycRate_(tumbleRate);

		this.initInteractions; // method currently empty

		// initialize canvas
		this.updateCanvasDims;
		this.onResize_({ this.updateCanvasDims });

		// init controls ui
		rotationView = PointViewUI(this, Rect(5, 35, 405, 700));
		this.addDependant(rotationView);
		rotationView.onClose({ this.removeDependant(rotationView) });
		this.makeShowView;
		this.makePerspectiveView;

		this.layItOut;
	}

	layItOut {
		var rTxt, sTxt, pTxt;
		var onCol = Color.blue;
		var offCol = Color.gray;
		var tempSkewX, tempTransX;

		tempSkewX = skewX;
		tempTransX = translateX;

		this.layout_(
			VLayout(
				HLayout(
					rTxt = StaticText().string_("Rotation")
					.mouseDownAction_({ |txt|
						var cur;
						cur = rotationView.visible;
						if (cur) {
							txt.stringColor_(offCol);
							this.skewX_(tempSkewX);
							this.translateX_(tempTransX);
						} {
							txt.stringColor_(onCol);
							showView.visible_(false);
							perspectiveView.visible_(false);
							sTxt.stringColor_(offCol);
							pTxt.stringColor_(offCol);
							tempSkewX = skewX;
							this.skewX_(0.0);
							tempTransX = translateX;
							this.translateX_(0.6);
						};
						rotationView.visible_(cur.not);
					})
					.stringColor_(offCol),

					sTxt = StaticText().string_("Show")
					.mouseDownAction_({ |txt|
						var cur;
						cur = showView.visible;
						if (cur) {
							txt.stringColor_(offCol);
						} {
							txt.stringColor_(onCol);

							// return translation if rotation view is visible
							if (rotationView.visible) {
								this.translateX_(tempTransX);
							};
							rotationView.visible_(false);
							perspectiveView.visible_(false);
							rTxt.stringColor_(offCol);
							pTxt.stringColor_(offCol);
						};
						showView.visible_(cur.not)
					})
					.stringColor_(offCol),

					pTxt = StaticText().string_("Perspective")
					.mouseDownAction_({ |txt|
						var cur;
						cur = perspectiveView.visible;
						if (cur) {
							txt.stringColor_(offCol);
						} {
							txt.stringColor_(onCol);

							// return translation if rotation view is visible
							if (rotationView.visible) {
								this.translateX_(tempTransX);
							};
							showView.visible_(false);
							rotationView.visible_(false);
							sTxt.stringColor_(offCol);
							rTxt.stringColor_(offCol);
						};
						perspectiveView.visible_(cur.not)
					})
					.stringColor_(offCol),

					nil
				).spacing_(20),

				rotationView.maxWidth_(430).visible_(false),
				showView.maxWidth_(330).visible_(false),
				perspectiveView.maxWidth_(430).visible_(false),

				nil
			)
		);
		}

	makeShowView {
		var axChk, axLenSl;
		var indcChk;
		var connChk, triBut, seqBut, statusTxt;

		statusTxt = StaticText().string_("").align_(\center).stringColor_(Color.gray);

		axChk = CheckBox()
		.action_({ |cb|
			this.showAxes_(cb.value);
		})
		.value_(showAxes)
		;

		axLenSl = Slider()
		.action_({ |sl|
			this.axisScale_(sl.value.linlin(0,1,0,1.5))
		})
		.orientation_(\horizontal)
		.value_(axisScale / 1.5);

		indcChk = CheckBox()
		.action_({ |cb|
			this.showIndices_(cb.value);
		})
		.value_(showIndices)
		;

		connChk = CheckBox()
		.action_({ |cb|
			this.showConnections_(cb.value);
		})
		.value_(showConnections)
		;

		triBut = Button()
		.action_({
			var triplets, connPairs, warning;

			statusTxt.string_("Triangulating points...");
			fork({
				if (\SphericalDesign.asClass.isNil) {
					warning = "SphericalDesign quark is required to compute triangulation."
				} {
					try {
						triplets = SphericalDesign().points_(points).calcTriplets.triplets;
						connPairs = this.reduceTriplets(triplets);
						this.connections_(connPairs);
						statusTxt.string_("");
					} {
						warning = "Couldn't calulate the triangulation of points :(";
					};
				};

				warning !? {
					statusTxt.string_(warning);
					warning.warn;
					defer { 3.wait; statusTxt.string_("") };
				};

			}, AppClock);
			connChk.valueAction_(true);
		})
		.states_([["Triangulation"]])
		.maxHeight_(25)
		;

		seqBut = Button()
		.action_({
			this.connections_([(0..points.size-1)]);
			connChk.valueAction_(true);
		})
		.states_([["Sequential"]])
		.maxHeight_(25)
		;

		showView = View().layout_(
			VLayout(
				HLayout(
					axChk,
					StaticText().string_("Axes").align_(\left),
					15,
					StaticText().string_("length: ").align_(\left),
					axLenSl
				),
				HLayout(
					indcChk,
					StaticText().string_("Indices").align_(\left),
					nil
				),
				HLayout(
					connChk,
					StaticText().string_("Connections").align_(\left),
					nil,
					triBut,
					10,
					seqBut
				),
				statusTxt,
			)
		);
	}

	makePerspectiveView {
		var xposChk, xnegChk, yposChk, ynegChk, zposChk, znegChk;
		var skxSl, skySl, trxSl, trySl;
		var orDistSl, eyeDistSl;
		var offposChk, offnegChk, distChk;
		var pCtlView, geomView, orthoOffsetView;

		#xposChk, xnegChk, yposChk, ynegChk, zposChk, znegChk =
		['+X', '-X', '+Y', '-Y', '+Z', '-Z'].collect{ |ax|
			CheckBox()
			.action_({ |cb|
				if (cb.value) {
					this.setOrtho(ax,
						if (offposChk.value) {
							0.25pi
						} {
							if (offnegChk.value) { 0.25pi.neg } { 0 }
						}
					);
					orthoOffsetView.visible = true;
					geomView.visible = false;
					[xposChk, xnegChk, yposChk, ynegChk, zposChk, znegChk].do({ |me|
						if (me != cb) {me.value = false}
					});
				} {
					this.setPerspective;
					orthoOffsetView.visible = false;
					geomView.visible = true;
					[offposChk, offnegChk].do(_.value_(false));
				};
			})
			.value_(ortho and: { orthoAxis == ax })
		};

		offposChk = CheckBox()
		.action_({ |cb|
			if (cb.value) {
				offnegChk.value_(false);
				this.setOrtho(orthoAxis, 0.25pi)
			} {
				this.setOrtho(orthoAxis, 0)
			}
		});

		offnegChk = CheckBox()
		.action_({ |cb|
			if (cb.value) {
				offposChk.value_(false);
				this.setOrtho(orthoAxis, 0.25pi.neg)
			} {
				this.setOrtho(orthoAxis, 0)
			}
		});

		distChk = CheckBox()
		.action_({ |cb|
			this.renderDistanceSize_(cb.value)
		})
		.value_(renderDistanceSize);

		#skxSl, skySl, trxSl, trySl =
		[\skewX, \skewY, \translateX, \translateY].collect{ |meth|
			var setter;
			setter = (meth ++ \_).asSymbol;
			Slider()
			.action_({ |sl| this.perform(setter, sl.value.linlin(0,1,-2,2)) })
			.value_(this.perform(meth).linlin(-2,2,0,1))
			.orientation_(\horizontal)
			.maxWidth_(200)
		};

		orDistSl = Slider()
		.action_({ |sl|
			this.originDist_(sl.value.linlin(0,1,0.5.neg,3))
		})
		.orientation_(\horizontal)
		.value_((az-bz).linlin(0.5.neg,3,0,1))
		;
		eyeDistSl = Slider()
		.action_({ |sl|
			this.eyeDist_(sl.value.linlin(0,1,1.5,6))
		})
		.orientation_(\horizontal)
		.value_(bz.linlin(1.5,6,0,1))
		;

		perspectiveView = View().layout_(
			HLayout(
				VLayout(
					StaticText().string_("Ortho").font_(Font.default.bold_(true)).align_(\center).fixedWidth_(65),
					GridLayout.columns(
						[
							nil,
							StaticText().string_("X"),
							StaticText().string_("Y"),
							StaticText().string_("Z"),
						],
						[
							StaticText().string_(" +").align_(\left), xposChk, yposChk, zposChk
						],
						[
							StaticText().string_(" -").align_(\left), xnegChk, ynegChk, znegChk
						],
					),
					orthoOffsetView = View().layout_(
						VLayout(
							StaticText().string_("Offset\n45˚").align_(\center),
							GridLayout.columns(
								[StaticText().string_(" +").align_(\left), offposChk],
								[StaticText().string_(" -").align_(\left), offnegChk],
							)
						)
					).visible_(false),
					nil
				),
				25,
				pCtlView = View().layout_(
					VLayout(
						StaticText().string_("Perspective").align_(\center).font_(Font.default.bold_(true)),
						HLayout(
							distChk,
							StaticText().string_("Render size with distance").align_(\left),
						),
						10,
						geomView = View().layout_(
							VLayout(
								StaticText().string_("Skew").align_(\center),
								HLayout(
									StaticText().string_("X"), skxSl
								),
								HLayout(
									StaticText().string_("Y"), skySl
								),
								StaticText().string_("Translate").align_(\center),
								HLayout(
									StaticText().string_("X"), trxSl
								),
								HLayout(
									StaticText().string_("Y"), trySl
								),
								StaticText().string_("Origin Distance").align_(\center),
								orDistSl,
								StaticText().string_("Eye Distance").align_(\center),
								eyeDistSl
							).margins_(0)
						),
						nil
					).margins_(0)
				),
				nil
			)
		);
	}

	updateCanvasDims {
		var bnds;
		userView.bounds_(this.bounds.origin_(0@0));
		bnds = userView.bounds;
		cen  = bnds.center;
		minDim = min(bnds.width, bnds.height);
		rotPxStep = 2pi / bnds.width;
	}

	points_ { |cartesians, resetConnections = true|
		var sphericals, rhoScale;

		points = cartesians;
		sphericals = points.collect(_.asSpherical);
		rhoScale = sphericals.collect(_.rho).maxItem.reciprocal;
		pointsNorm = sphericals.collect({ |me| me.rho_(me.rho * rhoScale).asCartesian });

		if (resetConnections) { connections = [(0..points.size-1)] };
		this.prUpdateColors;
		this.refresh;
	}

	// Set points by directions.
	// Can be an Array of:
	// Sphericals, or
	// [[theta, phi], [theta, phi] ...], (rho assumed to be 1) or
	// [[theta, phi, rho], [theta, phi, rho] ...]
	directions_ { |dirArray, resetConnections = true|
		var first, sphericals;

		first = dirArray[0];
		sphericals = case
		{ first.isKindOf(Spherical) } {
			dirArray
		}
		{ first.size ==  2 } {
			dirArray.collect( Spherical(1, *_) )
		}
		{ first.size ==  3 } {
			dirArray.collect{ |tpr| Spherical(tpr[2], tpr[0], tpr[1]) }
		}
		{
			"[PointView:-directions_] Invalid dirArray argument."
			"Can be an Array of: Sphericals, or [[theta, phi], [theta, phi] ...], "
			"(rho assumed to be 1), or [[theta, phi, rho], [theta, phi, rho] ...]"
			.throw
		};

		this.points_(sphericals.collect(_.asCartesian), resetConnections);
	}


	drawFunc {
		^{ |v|
			var scale, pnts, pnts_xf, pnt_depths;
			var axPnts, axPnts_xf, axPnts_depths;
			var rotPnts, to2D, incStep, rho, offset;
			var strRect, minPntSize, variance;

			minPntSize = pointSize * pointDistScale;
			scale = minDim.half;

			rotPnts = { |carts|
				var rotated;
				rotated = switch (rotateMode,
					\rtt, {
						carts.collect{ |pnt|
							pnt.rotate(rotate).tilt(tilt).tumble(tumble)
						}
					},
					\ypr, {
						carts.collect{ |pnt|
							pnt.tilt(tilt).tumble(tumble).rotate(rotate)
						}
					}
				);

				// perform additional rotation from mouse
				// interaction after model rotation
				if (extrinsicRotation and: { ortho.not }) {
					rotated = rotated.collect{ |pnt| pnt.rotate(exRot).tumble(exTum) }
				};

				// orient so view matches ambisonics and return
				rotated.collect{ |pnt| pnt.rotate(0.5pi).tilt(0.5pi) };
			};

			// xformed points from 3D -> perspective -> 2D
			// + cart.z; accounts for depth adjusted by rotation
			// (az is the depth position of the _center_ of shape's rotation)
			// https://en.wikipedia.org/wiki/3D_projection
			to2D = { |carts|
				carts.collect{ |cart|
					if (ortho.not) {
						cart = cart
						+ (skewX @ skewY.neg)           // offset points within world, normalized
						* (bz / (az + cart.z))          // add perspective
						+ (translateX @ translateY.neg) // translate the "world"
					};
					cart.asPoint * scale; // discard z for 2D drawing, scale to window size
				}
			};

			variance = { |bool|
				if (bool) { 1 + rrand(randomVariance.neg, randomVariance) } { 1 }
			};

			// if rotating
			incStep = { |rand, curRot, step|
				(curRot + (step * variance.(rand))).wrap(-2pi, 2pi)
			};

			if (ortho) {
				#rotate, tilt, tumble = orthoRotations;
			} {
				rotate = if (cycRotate) {
					incStep.(randomizedAxes.rotate, rotate, rotateStep)
				} { baseRotation };

				tilt = if (cycTilt) {
					incStep.(randomizedAxes.tilt,   tilt,   tiltStep)
				} { baseTilt };

				tumble = if (cycTumble) {
					incStep.(randomizedAxes.tumble, tumble, tumbleStep)
				} { baseTumble };

				// if oscillating
				if (oscRotate) {
					rotatePhase = ( // 0 to 2pi
						rotatePhase + (rotateOscPhsInc * rotateDir)
					) % 2pi;
					rotate = sin(rotatePhase) * 0.5 * rotateOscWidth + baseRotation;
				};

				if (oscTilt) {
					tiltPhase = (
						tiltPhase + (tiltOscPhsInc * tiltDir)
					) % 2pi;
					tilt = sin(tiltPhase) * 0.5 * tiltOscWidth + baseTilt;
				};

				if (oscTumble) {
					tumblePhase = (
						tumblePhase + (tumbleOscPhsInc * tumbleDir)
					) % 2pi;
					tumble = sin(tumblePhase) * 0.5 * tumbleOscWidth + baseTumble;
				};
			};

			// rotate into ambisonics coords and rotate for user
			pnts = rotPnts.(pointsNorm);
			axPnts = rotPnts.(axisPnts * axisScale);

			// hold on to these point depths (z in screen coords) for use when drawing with perspective
			pnt_depths = pnts.collect(_.z);
			axPnts_depths = axPnts.collect(_.z);

			// transform 3D positions to 2D points with perspective
			pnts_xf = to2D.(pnts);
			axPnts_xf = to2D.(axPnts);

			/* DRAW */

			// move to center
			Pen.translate(cen.x, cen.y);

			// draw axes
			if (showAxes) {
				var lineDpth, pntDepth, pntSize;
				var r, oxy, theta, axStrings, refPnt;

				strRect = "XX".bounds.asRect;
				r = strRect.width / 2;

				if (ortho and: { orthoOffset == 0 }) {
					axStrings = xyz.copy;
					switch (orthoAxis,
						'+X', { axStrings[0] = "+X" },
						'-X', { axStrings[0] = "-X" },
						'+Y', { axStrings[1] = "+Y" },
						'-Y', { axStrings[1] = "-Y" },
						'+Z', { axStrings[2] = "+Z" },
						'-Z', { axStrings[2] = "-Z" },
					)
				} {
					axStrings = xyz;
				};

				// axPnts_xf = [origin,x,y,z]
				axPnts_xf[1..].do{ |axPnt, i|
					pntDepth = axPnts_depths[i+1];

					// average the depth between pairs of connected points
					lineDpth = axPnts_depths[0] + pntDepth * 0.5;
					pntSize = pntDepth.linlin(-1.0,1.0, 15, 5);

					Pen.strokeColor_(axisColors[i]);
					Pen.moveTo(axPnts_xf[0]);
					Pen.width_(lineDpth.linlin(-1.0,1.0, connStrokeWidthNear, connStrokeWidthFar));
					Pen.lineTo(axPnt);
					Pen.stroke;

					// draw axis label
					refPnt = axPnts[i+1];
					rho = refPnt.rho + (pntSize * if (ortho, { 2 }, { 1.2 }));
					offset = refPnt.asPolar.rho_(rho).asPoint - refPnt;
					strRect = strRect.center_(axPnt + offset);

					Pen.fillColor_(axisColors[i]);

					Pen.stringCenteredIn(
						axStrings[i],
						strRect,
						Font.default.pointSize_(
							pntDepth.linlin(-1.0,1.0, 18, 10) // change font size with depth
						)
					);

				};
			};

			// draw connecting lines
			if (showConnections and: { connections.notNil }) {
				connections.do{ |set, i|
					var pDpths;

					// collect and average the depth between pairs of connected points
					pDpths = set.collect(pnt_depths[_]);
					pDpths = pDpths + pDpths.rotate(-1) / 2;

					Pen.strokeColor_(connectionColor);
					Pen.moveTo(pnts_xf.at(set[0]));

					set.rotate(-1).do{ |idx, j|
						// change line width with depth
						Pen.width_(pDpths[j].lincurve(-1.0,1.0, connStrokeWidthNear, connStrokeWidthFar, -3.5));
						Pen.lineTo(pnts_xf[idx]);
						Pen.stroke;
						Pen.moveTo(pnts_xf[idx]);
					};
				};

			};

			// draw points and indices
			strRect = "000000".bounds.asRect;

			// // original method, just draws in point order, not depth order, no visual glitches
			// pnts_xf.do{ |pnt, i|
			// 	var pntSize, f, fCol;
			//
			// 	if (renderDistanceSize) {
			// 		pntSize = pnt_depths[i].linlin(-1.0,1.0, pointSize, pointSize * pointDistScale);
			// 		fCol = indicesColor;
			// 	} {
			// 		pntSize = pointSize;
			// 	};
			//
			// 	// draw index
			// 	if (showIndices) {
			// 		Pen.fillColor_(
			// 			indicesColor.alpha_(
			// 				pnt_depths[i].linlin(-1.0,1.0, 1, 0.35)
			// 			)
			// 		);
			//
			// 		f = Font.default.pointSize_(
			// 			pnt_depths[i].linlin(-1.0,1.0, 18, 10) // change font size with depth
			// 		);
			//
			// 		// index labels smoothly rotate around the point
			// 		// more expensive but looks better
			// 		rho = pnts[i].rho + (pntSize * 1.5);
			// 		offset = pnts[i].asPolar.rho_(rho).asPoint - pnts[i];
			// 		Pen.stringCenteredIn(i.asString, strRect.center_(pnt + offset), f);
			//
			// 		// // index labels always on the outside of the sphere, but jumpy
			// 		// Pen.stringCenteredIn(i.asString, strRect.center_(pnt + Point(pntSize * pnts[i].x.sign, pntSize * pnts[i].y.sign)), f);
			//
			// 		// // index labels always same offset from point, cheap but cluttered
			// 		// Pen.stringLeftJustIn(i.asString,	strRect.left_(pnt.x + pntSize).bottom_(pnt.y + pntSize), f);
			// 	};
			//
			// 	// draw point
			// 	Pen.fillColor_(prPntDrawCols.wrapAt(i));
			// 	Pen.fillOval(Size(pntSize, pntSize).asRect.center_(pnt));
			// };


			// NOTE: this seems to bring about dropouts on the points and
			// index labels at high point counts, not sure why... so leaving original method above
			// iterate over indices in order of farthest to nearest depth
			pnt_depths.order({ |a, b| a > b }).do{ |sortIdx, i|
				var pnt, pntSize, f, fCol;

				pnt = pnts_xf[sortIdx];

				if (renderDistanceSize) {
					pntSize = pnt_depths[sortIdx].linlin(-1.0,1.0, pointSize, pointSize * pointDistScale);
					fCol = indicesColor;
				} {
					pntSize = pointSize;
				};

				// draw index
				if (showIndices) {
					Pen.fillColor_(
						indicesColor.alpha_(
							pnt_depths[sortIdx].linlin(-1.0,1.0, 1, 0.35)
						)
					);

					f = Font.default.pointSize_(
						pnt_depths[sortIdx].linlin(-1.0,1.0, 18, 10) // change font size with depth
					);

					// index labels smoothly rotate around the point
					// more expensive but looks better
					rho = pnts[sortIdx].rho + (pntSize * 1.5);
					offset = pnts[sortIdx].asPolar.rho_(rho).asPoint - pnts[sortIdx];
					Pen.stringCenteredIn(sortIdx.asString, strRect.center_(pnt + offset), f);

					// // index labels always on the outside of the sphere, but jumpy
					// Pen.stringCenteredIn(i.asString, strRect.center_(pnt + Point(pntSize * pnts[sortIdx].x.sign, pntSize * pnts[sortIdx].y.sign)), f);

					// // index labels always same offset from point, cheap but cluttered
					// Pen.stringLeftJustIn(i.asString,	strRect.left_(pnt.x + pntSize).bottom_(pnt.y + pntSize), f);
				};

				// draw point
				Pen.fillColor_(prPntDrawCols.wrapAt(sortIdx));
				Pen.fillOval(Size(pntSize, pntSize).asRect.center_(pnt));
			};
		}
	}


	/* Perspective controls */

	// skew/offset the points in the world (before perspective is added)
	skewX_ { |norm|
		skewX = norm;
		this.refresh;
		this.changed(\skewX, norm);
	}
	skewY_ { |norm|
		skewY = norm;
		this.refresh;
		this.changed(\skewY, norm);
	}

	// translate the world (after perspective is added)
	translateX_ { |norm|   // translateX: left -> right = -1 -> 1
		translateX = norm;
		this.refresh;
		this.changed(\translateX, norm);
	}
	translateY_ { |norm|   // translateY: bottom -> top = -1 -> 1
		translateY = norm;
		this.refresh;
		this.changed(\translateY, norm);
	}

	// distance of points' origin to screen
	originDist_ { |norm|
		az = bz + norm;
		this.refresh;
		this.changed(\originDist, norm);
	}
	// distance of eye to screen
	eyeDist_ { |norm|
		var temp = az - bz; // store origin offset
		bz = norm;
		az = bz + temp;
		this.refresh;
		this.changed(\eyeDist, norm);
	}

	pointSize_ { |px = 15|
		pointSize = px;
		this.refresh;
		this.changed(\pointSize, px);
	}

	pointDistScale_ { |norm = 0.333|
		pointDistScale = norm;
		this.refresh;
		this.changed(\pointDistScale, norm);
	}

	renderDistanceSize_ { |bool|
		renderDistanceSize = bool;
		this.refresh;
		this.changed(\renderDistanceSize, bool);
	}

	// axis: '+X', '-X', '+Y', '-Y', '+Z', '-Z'
	// offset: additional rotation after orienting view along axis, radians
	setOrtho { |axis, offset = 0|
		ortho = true;
		perspective = false;

		orthoAxis = axis;
		orthoOffset = offset;

		// in [R,T,T]
		orthoRotations = switch (axis,
			'+X', { [0 + offset, 0, 0] },
			'-X', { [pi + offset, 0, 0] },
			'+Y', { [0.5pi.neg + offset, 0, 0] },
			'-Y', { [0.5pi + offset, 0, 0] },
			'+Z', { [0, 0, 0.5pi.neg + offset] },
			'-Z', { [0, 0, 0.5pi + offset] }
		);

		this.changed(\ortho, true);
		this.refresh;
	}

	setPerspective {
		perspective = true;
		ortho = false;
		this.changed(\ortho, false);
		this.refresh;
	}

	/* View movement controls */

	rotate_ { |radians|
		baseRotation = radians;
		cycRotate = false;
		this.refresh;
		this.changed(\rotate, radians);
	}
	tilt_ { |radians|
		baseTilt = radians;
		cycTilt = false;
		this.refresh;
		this.changed(\tilt, radians);
	}
	tumble_ { |radians|
		baseTumble = radians;
		cycTumble = false;
		this.refresh;
		this.changed(\tumble, radians);
	}

	// rotation direction: 1 ccw, -1 cw
	rotateDir_ { |dir|
		rotateDir = dir;
		rotateStep = (rotateRate / frameRate) * 2pi * rotateDir;
		this.changed(\rotateDir, rotateDir);
	}
	tiltDir_ { |dir|
		tiltDir = dir;
		tiltStep = (tiltRate / frameRate) * 2pi * tiltDir;
		this.changed(\tiltDir, tiltDir);
	}
	tumbleDir_ { |dir|
		tumbleDir = dir;
		tumbleStep = (tumbleRate / frameRate) * 2pi * tumbleDir;
		this.changed(\tumbleDir, tumbleDir);
	}
	allDir_ { |dir|
		this.rotateDir_(dir).tiltDir_(dir).tumbleDir_(dir);
	}

	rotateCycRate_ { |hz|
		rotateRate = hz;
		rotateStep = (rotateRate / frameRate) * 2pi * rotateDir;
		this.changed(\rate, \rotate, hz);
	}
	tiltCycRate_ { |hz|
		tiltRate = hz;
		tiltStep = (tiltRate / frameRate) * 2pi * tiltDir;
		this.changed(\rate, \tilt, hz);
	}
	tumbleCycRate_ { |hz|
		tumbleRate = hz;
		tumbleStep = (tumbleRate / frameRate) * 2pi * tumbleDir;
		this.changed(\rate, \tumble, hz);
	}
	allCycRate_ { |hz|
		this.rotateCycRate_(hz).tiltCycRate_(hz).tumbleCycRate_(hz);
	}

	rotateCycPeriod_ { |seconds| this.rotateCycRate_(seconds.reciprocal) }
	tiltCycPeriod_   { |seconds| this.tiltCycRate_(seconds.reciprocal) }
	tumbleCycPeriod_ { |seconds| this.tumbleCycRate_(seconds.reciprocal) }
	allCycPeriod_    { |seconds|
		this.rotateCycRate_(seconds).tiltCycRate_(seconds).tumbleCycRate_(seconds)
	}

	rotatePeriod { ^rotateRate.reciprocal }
	tiltPeriod   { ^tiltRate.reciprocal }
	tumblePeriod { ^tumbleRate.reciprocal }

	rotateCyc_ { |bool|
		cycRotate = bool;
		bool.if{ oscRotate = false };
		this.prCheckAnimate(\auto, \rotate, bool);
	}
	tiltCyc_ { |bool|
		cycTilt = bool;
		bool.if{ oscTilt = false };
		this.prCheckAnimate(\auto, \tilt, bool);
	}
	tumbleCyc_ { |bool|
		cycTumble = bool;
		bool.if{ oscTumble = false };
		this.prCheckAnimate(\auto, \tumble, bool);
	}
	allCyc_ { |bool|
		this.rotateCyc_(bool).tiltCyc_(bool).tumbleCyc_(bool);
		this.changed(\allCyc, bool);
	}

	rotateOsc_ { |bool|
		oscRotate = bool;
		bool.if{ cycRotate = false };
		this.prCheckAnimate(\rotate, bool);
	}
	tiltOsc_ { |bool|
		oscTilt = bool;
		bool.if{ cycTilt = false };
		this.prCheckAnimate(\tilt, bool);
	}
	tumbleOsc_ { |bool|
		oscTumble = bool;
		bool.if{ cycTumble = false };
		this.prCheckAnimate(\tumble, bool);
	}
	allOsc_ { |bool|
		this.rotateOsc_(bool).tiltOsc_(bool).tumbleOsc_(bool);
		this.changed(\allOsc, bool);
	}

	prCheckAnimate { |which, bool|
		userView.animate_(
			[   cycRotate, cycTilt, cycTumble,
				oscRotate, oscTilt, oscTumble
			].any({ |bool| bool })
		);
		this.changed(\osc, which, bool);
	}


	rotateOscPeriod_ { |seconds|
		rotateOscT = seconds;
		rotateOscPhsInc = 2pi / (seconds * frameRate);
		if (randomizedAxes.rotate) {
			rotateOscPhsInc = rotateOscPhsInc * (1 + rrand(0.05, randomVariance))
		};
		this.changed(\oscRotatePeriod, seconds);
	}
	tiltOscPeriod_ { |seconds|
		tiltOscT = seconds;
		tiltOscPhsInc = 2pi / (seconds * frameRate);
		if (randomizedAxes.tilt) {
			tiltOscPhsInc = tiltOscPhsInc * (1 + rrand(0.05, randomVariance))
		};
		this.changed(\oscTiltPeriod, seconds);
	}
	tumbleOscPeriod_ { |seconds|
		tumbleOscT = seconds;
		tumbleOscPhsInc = 2pi / (seconds * frameRate);
		if (randomizedAxes.tumble) {
			tumbleOscPhsInc = tumbleOscPhsInc * (1 + rrand(0.05, randomVariance))
		};
		this.changed(\oscTumblePeriod, seconds);
	}
	allOscPeriod_ { |seconds|
		this.rotateOscPeriod_(seconds).tiltOscPeriod_(seconds).tumbleOscPeriod_(seconds);
	}

	rotateOscWidth_  { |widthRad|
		rotateOscWidth = widthRad;
		this.changed(\oscRotateWidth, widthRad);
	}
	tiltOscWidth_  { |widthRad|
		tiltOscWidth = widthRad;
		this.changed(\oscTiltWidth, widthRad);
	}
	tumbleOscWidth_  { |widthRad|
		tumbleOscWidth = widthRad;
		this.changed(\oscTumbleWidth, widthRad);
	}
	allOscWidth_ { |widthRad|
		this.rotateOscWidth_(widthRad).tiltOscWidth_(widthRad).tumbleOscWidth_(widthRad);
	}

	rotateMode_ { |rttOrYpr|
		rotateMode = rttOrYpr;
		this.changed(\rotateMode, rotateMode);
		this.refresh;
	}

	varyMotion_ { |axis, bool|
		randomizedAxes[axis] = bool;
		// update osc periods
		this.rotateOscPeriod_(rotateOscT);
		this.tiltOscPeriod_(tiltOscT);
		this.tumbleOscPeriod_(tumbleOscT);
	}

	/* Display controls */

	showIndices_ { |bool|
		showIndices = bool;
		this.changed(\showIndices, bool);
		this.refresh;
	}

	showAxes_ { |bool|
		showAxes = bool;
		this.changed(\showAxes, bool);
		this.refresh;
	}

	showConnections_ { |bool|
		showConnections = bool;
		this.changed(\showConnections, bool);
		this.refresh;
	}

	// draw lines between these indices of points
	// e.g. [[1,3],[0,5],[2,4]]
	connections_ { |arraysOfIndices, update = true|
		if (arraysOfIndices.rank != 2) {
			"[PointView:-connections_] arraysOfIndices argument "
			"is not an array with rank == 2.".throw
		};

		connections = arraysOfIndices;
		if (update) {
			showConnections = true;
			this.refresh;
		};
	}

	// arrayOfIndices: triplets of indices of connected points
	connectTriplets_ { |arrayOfIndices|
		this.connections_(this.reduceTriplets(arrayOfIndices));
	}

	axisColors_ { |colorArray|
		axisColors = colorArray;
		this.changed(\axisColors, *axisColors);
		this.refresh;
	}

	axisScale_ { |scale|
		axisScale = scale;
		this.refresh;
	}

	frameRate_ { |hz|
		frameRate = hz;
		userView.frameRate_(hz);
		this.changed(\frameRate, hz);
		// update rotation oscillator's phase step
		this.rotateOscPeriod_(rotateOscT);
	}


	/* Point color controls */

	// arrayOfColors can be a Color, Array of Colors.
	// If (arrayOfColors.size != points.size), points will wrap through the
	// color array
	pointColors_ { |arrayOfColors|
		if (arrayOfColors.isKindOf(Color)) {
			arrayOfColors = [arrayOfColors];
		};

		if (
			arrayOfColors.isKindOf(Array) and:
			{ arrayOfColors.every({ |elem| elem.isKindOf(Color) }) }
		) {
			pointColors = arrayOfColors;
			prPntDrawCols = points.size.collect(pointColors.wrapAt(_));
			this.refresh;
		} {
			"[PointView:-pointColors_] arrayOfColors argument is not a Color or Array of Colors".throw;
		};
		colsByHue = false;
	}

	pointHueRange_ { |hueLow = 0, hueHigh = 0.999, sat = 0.9, val = 1, alpha = 0.8, scramble = false|
		var size = points.size;

		prPntDrawCols = size.collect{ |i|
			Color.hsv(
				(i / (size - 1)).linlin(0, 0.999, hueLow, hueHigh),
				sat, val, alpha
			)
		};
		if (scramble) {
			prPntDrawCols = prPntDrawCols.scramble;
			huesScrambled = scramble;
		};
		colsByHue = true;
		this.refresh;
	}

	groupColors_ { |arrayOfColors, defaultColor = (Color.gray.alpha_(0.3))|

		groupColors = arrayOfColors.isKindOf(Color).if({ [arrayOfColors] }, { arrayOfColors.flat });
		defaultGroupColor = defaultColor;

		// initialize colorGroups if not already
		colorGroups ?? {
			var npnts, ngrps, cnt = 0;

			npnts = this.points.size;
			ngrps = groupColors.size;

			colorGroups = Array.fill(ngrps, { Array.new } );

			// form color groups of the correct rank
			npnts.do{ |i|
				colorGroups[i % ngrps] = colorGroups[i % ngrps].add(i);
			};

			// reset indices so they're grouped in ascending order
			colorGroups.do{ |arr|
				arr.do{ |elem, i|
					arr[i] = cnt;
					cnt = cnt + 1;
				}
			};
		};

		this.colorGroups_(colorGroups);

	}

	// Set groups of point indices which belong to each color in
	// pointColors array.
	// defaultColor is a Color for points not included in arraysOfIndices
	colorGroups_ { |arraysOfIndices|

		prPntDrawCols = points.size.collect{ defaultGroupColor };

		if (arraysOfIndices.rank == 1) {
			arraysOfIndices = [arraysOfIndices];
		};

		colorGroups = arraysOfIndices;

		colorGroups.do{ |group, grpIdx|
			group.do{ |pntIdx|
				prPntDrawCols[pntIdx] = groupColors.wrapAt(grpIdx)
			}
		};

		colsByHue = false;
		this.refresh;
	}

	highlightPoints { |arrayOfIndices, highlightColor = (Color.red), defaultColor = (Color.gray.alpha_(0.25))|
		prevColors = prPntDrawCols.copy;
		this.groupColors_(highlightColor, defaultColor);
		this.colorGroups_([arrayOfIndices]);
		highlighted = true;
	}

	removeHighlight {
		if (highlighted) { this.pointColors_(prevColors) }
	}

	connectionColor_ { |aColor|
		connectionColor = aColor;
		this.refresh;
	}

	connectionStrokeWidth_ { |px|
		connStrokeWidthNear = px;
		connStrokeWidthFar = px * pointDistScale;
		this.refresh;
	}

	connectionStrokeWidth { ^connStrokeWidthNear }

	indicesColor_ { |aColor|
		indicesColor = aColor;
		this.refresh;
	}

	// called when points are set
	prUpdateColors {
		var hues, sat, val, alpha;
		if (colsByHue) {
			var colsHSV = prPntDrawCols.collect(_.asHSV);
			hues = colsHSV.collect({arg col; col[0];}); // hue values
			sat = colsHSV.first[1]; // saturation
			val = colsHSV.first[2]; //val
			alpha = colsHSV.first[3]; // alpha
			this.pointHueRange_(hues.minItem, hues.maxItem, sat, val, alpha, huesScrambled);
		};

		prPntDrawCols ?? {
			prPntDrawCols = points.size.collect(pointColors.wrapAt(_));
			^this
		};
	}

	// for UI controls
	units_ { |radiansOrDegrees|
		this.changed(\units, radiansOrDegrees)
	}

	initInteractions {
		var mouseDownPnt, mouseUpPnt, mouseMovePnt;

		userView.mouseMoveAction_({ |v,x,y,modifiers|
			var deltaX, deltaY;

			extrinsicRotation = true;
			deltaX = x - mouseDownPnt.x;
			deltaY = y - mouseDownPnt.y;
			exRot = prevExRot + (deltaX * rotPxStep);
			exTum = prevExTum + (deltaY * rotPxStep);
			mouseMovePnt = x@y;

			this.refresh;
		});

		userView.mouseDownAction_({ |v,x,y, modifiers, buttonNumber, clickCount|
			mouseDownPnt = x@y;
			mouseMovePnt = x@y;
		});

		userView.mouseUpAction_({ |v,x,y, modifiers|
			mouseUpPnt = x@y;
			prevExRot = exRot;
			prevExTum = exTum;
		});
	}

	refresh {
		userView.animate.not.if{ userView.refresh };
	}

	// reset rotations
	reset {
		extrinsicRotation = false; // flag if mouse interaction requires additional extrinsic rotation
		exRot = exTum = prevExRot = prevExTum = 0;
		this.rotate_(-45.degrad).tilt_(0).tumble_(0);
		this.allOsc_(false).allCyc_(false);
		this.refresh;
	}

	// reduce triplets to pairs of connections so lines aren't
	// drawn multiple times. returns array of index pairs
	reduceTriplets { |tripletArray|
		var pairsDone, retPairs, wrapped;
		// reduce to point pairs to remove repeated lines
		retPairs = [];
		pairsDone = []; // list of pairs already found

		tripletArray.do{ |trip|
			wrapped = (trip ++ trip.first);
			3.collect({ |i|
				[wrapped[i], wrapped[i+1]]
			}).do{ |pair|
				// if (pairsDone.includes(pair).not) {
				if (
					pairsDone.any({ |pdone|
						{ |me| me[0] == pair[0] and: { me[1] == pair[1] } }.matchItem(pdone)
					}).not
				) {
					retPairs = retPairs.add(pair);         // add pair to connection list
					pairsDone = pairsDone.add(pair);         // keep track of a duple that's been used
					pairsDone = pairsDone.add(pair.reverse); // the reverse pair is identical
				};

			}
		};

		^retPairs
	}

	update { |who, what ... args|

		// Support for SphericalDesign class
		// It's assumed that connections are the design's
		// triplets if they have been calculated (triplets.notNil).
		if (sdClass.notNil) {
			if (who.isKindOf(sdClass)) {
				var triplets;

				switch (what,
					\points, {
						if (who.triplets.notNil and: {
							who.triplets.flat.maxItem <= who.points.size
						}) {
							// assume triplets are still valid, don't reset connections
							// user can SphericalDesign:-resetTriplets to explicitly clear them
							// it's more expensive to recalculate them with high point counts
							this.points_(who.points, false)
						} {
							// connections are no longer valid with the new points
							this.points_(who.points, true) // reset connections
						}
					},
					// when triplets change, it's assumed that connections are the triplets
					\triplets, {
						if (args[0]) {
							// new triplets
							triplets = who.triplets;
							this.connections_(this.reduceTriplets(triplets))
						} {
							// triplets removed and not recalculated
							connections = nil;
							this.showConnections_(false);
						}

					}
				)
			}
		};
	}

}



/*

Basic usage:

(
t = TDesign(13).visualize(bounds: [200,200, 1200,700].asRect, showConnections: true)
)

t.rotate_(0.25pi)
t.tilt_(-0.25pi)
t.reset
t.calcTriplets
t.mirrorX
t.resetTriplets

t.view.highlightPoints_( (4..7), Color.yellow )
// highlight all points +Z
t.view.highlightPoints_((0..t.numPoints-1).select({|i| t.points[i].z > 0}))

// connect pairs
t.view.connections_((0..t.points.size-1).clump(2))

*/

/*

// directions of a 13-point t-design
var dirs = [
	[ 0.0, 1.571 ], [ 0.0, 0.564 ], [ 1.571, 0.564 ],
	[ 3.142, 0.564 ], [ -1.571, 0.564 ], [ 0.0, -0.046 ],
	[ -1.571, -0.046 ], [ 3.142, -0.046 ], [ 1.571, -0.046 ],
	[ 0.0, -0.831 ], [ -1.571, -0.831 ], [ 3.142, -0.831 ],
	[ 1.571, -0.831 ]
];

~pv = PointView().directions_(dirs).front
)

(
// highlight all points above the horizon
var highPnts = ~pv.points.selectIndices{ |pnt| pnt.z > 0 };
~pv.highlightPoints(highPnts, Color.yellow);
)

// remove the highlight
~pv.removeHighlight;

// connect points with lines
~pv.connections_((0 .. ~pv.points.size-1).clump(2))

(
// ...or connect points that form a triangular mesh.
// This method is more efficient than setting
// connections to be the triplets directly, because
// some connections will be repeated
~pv.connectTriplets_([
	[ 0, 1, 2 ], [ 0, 1, 4 ], [ 0, 2, 3 ],
	[ 0, 3, 4 ], [ 1, 2, 8 ], [ 1, 4, 6 ],
	[ 1, 5, 6 ], [ 1, 5, 8 ], [ 2, 3, 7 ],
	[ 2, 7, 8 ], [ 3, 4, 6 ], [ 3, 6, 7 ],
	[ 5, 6, 10 ], [ 5, 8, 12 ], [ 5, 9, 10 ],
	[ 5, 9, 12 ], [ 6, 7, 11 ], [ 6, 10, 11 ],
	[ 7, 8, 12 ], [ 7, 11, 12 ], [ 9, 10, 11 ],
	[ 9, 11, 12 ]
])
)

// set random point colors
~pv.pointColors_(~pv.points.size.collect{ Color.rand })
// set point colors to a hue range
~pv.pointHueRange_(0.75, 0.95);

// group points by color
~pv.colorGroups_([(0..4),(5..8),(9..12)])
// need to set the group colors...
~pv.groupColors_([Color.red, Color.green, Color.yellow])

// rotate the perspective
~pv.rotate_(25.degrad)
~pv.tumble_(45.degrad)
~pv.reset

// orthographic projection down the Z axis
~pv.setOrtho('-Z')
// back to perspective view
~pv.setPerspective

~pv.showConnections = false;

*/
